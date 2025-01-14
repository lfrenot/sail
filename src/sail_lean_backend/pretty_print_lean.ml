open Libsail

open Type_check
open Ast
open Ast_defs
open Ast_util
open Reporting
open Rewriter
open PPrint
open Pretty_print_common

let prefix_recordtype = true

type global_context = { effect_info : Effects.side_effect_info }

type context = {
  global : global_context;
  kid_id_renames : id option KBindings.t; (* tyvar -> argument renames *)
  kid_id_renames_rev : kid Bindings.t; (* reverse of kid_id_renames *)
  is_monadic : bool;
}

let empty_context =
  {
    global = { effect_info = Effects.empty_side_effect_info };
    kid_id_renames = KBindings.empty;
    kid_id_renames_rev = Bindings.empty;
    is_monadic = false;
  }

let add_single_kid_id_rename ctxt id kid =
  let kir =
    match Bindings.find_opt id ctxt.kid_id_renames_rev with
    | Some kid -> KBindings.add kid None ctxt.kid_id_renames
    | None -> ctxt.kid_id_renames
  in
  {
    ctxt with
    kid_id_renames = KBindings.add kid (Some id) kir;
    kid_id_renames_rev = Bindings.add id kid ctxt.kid_id_renames_rev;
  }

let implicit_parens x = enclose (string "{") (string "}") x
let doc_id_ctor (ctxt : context) (Id_aux (i, _)) =
  match i with Id i -> string i | Operator x -> string (Util.zencode_string ("op " ^ x))

let doc_id = doc_id_ctor

let doc_field_name ctxt typ_id field_id =
  if prefix_recordtype && string_of_id typ_id <> "regstate" then
    doc_id ctxt typ_id ^^ string "_" ^^ doc_id ctxt field_id
  else doc_id ctxt field_id

let doc_kid ctxt (Kid_aux (Var x, _) as ki) =
  match KBindings.find_opt ki ctxt.kid_id_renames with
  | Some (Some i) -> string (string_of_id i)
  | _ -> string ("k_" ^ String.sub x 1 (String.length x - 1))

(* TODO do a proper renaming and keep track of it *)

let is_enum env id = match Env.lookup_id id env with Enum _ -> true | _ -> false

let pat_is_plain_binder env (P_aux (p, _)) =
  match p with
  | (P_id id | P_typ (_, P_aux (P_id id, _))) when not (is_enum env id) -> Some (Some id)
  | P_wild | P_typ (_, P_aux (P_wild, _)) -> Some None
  | P_var (_, _) -> Some (Some (Id_aux (Id "var", Unknown)))
  | P_app (_, _) -> Some (Some (Id_aux (Id "app", Unknown)))
  | P_vector _ -> Some (Some (Id_aux (Id "vect", Unknown)))
  | P_tuple _ -> Some (Some (Id_aux (Id "tuple", Unknown)))
  | P_list _ -> Some (Some (Id_aux (Id "list", Unknown)))
  | P_cons (_, _) -> Some (Some (Id_aux (Id "cons", Unknown)))
  | P_lit _ -> Some (Some (Id_aux (Id "lit", Unknown)))
  | _ -> None

(* Copied from the Coq PP *)
let args_of_typ l env typs =
  let arg i typ =
    let id = mk_id ("arg" ^ string_of_int i) in
    ((P_aux (P_id id, (l, mk_tannot env typ)), typ), E_aux (E_id id, (l, mk_tannot env typ)))
  in
  List.split (List.mapi arg typs)

(* Copied from the Coq PP *)
(* Sail currently has a single pattern to match against a list of
   argument types.  We need to tweak everything to match up,
   especially so that the function is presented in curried form.  In
   particular, if there's a single binder for multiple arguments
   (which rewriting can currently introduce) then we need to turn it
   into multiple binders and reconstruct it in the function body. *)
let rec untuple_args_pat typs (P_aux (paux, ((l, _) as annot)) as pat) =
  let env = env_of_annot annot in
  let identity body = body in
  match (paux, typs) with
  | P_tuple [], _ ->
      let annot = (l, mk_tannot Env.empty unit_typ) in
      ([(P_aux (P_lit (mk_lit L_unit), annot), unit_typ)], identity)
  (* The type checker currently has a special case for a single arg type; if
     that is removed, then remove the next case. *)
  | P_tuple pats, [typ] -> ([(pat, typ)], identity)
  | P_tuple pats, _ -> (List.combine pats typs, identity)
  | P_wild, _ ->
      let wild typ = (P_aux (P_wild, (l, mk_tannot env typ)), typ) in
      (List.map wild typs, identity)
  | P_typ (_, pat), _ -> untuple_args_pat typs pat
  | P_as _, _ :: _ :: _ | P_id _, _ :: _ :: _ ->
      let argpats, argexps = args_of_typ l env typs in
      let argexp = E_aux (E_tuple argexps, annot) in
      let bindargs (E_aux (_, bannot) as body) = E_aux (E_let (LB_aux (LB_val (pat, argexp), annot), body), bannot) in
      (argpats, bindargs)
  (* TODO Occurrences of the unit literal are removed right now, in order to be able to compile `initialize_registers`. *)
  | P_lit (L_aux (L_unit, _)), _ -> ([], identity)
  | _, [typ] -> ([(pat, typ)], identity)
  | _, _ -> unreachable l __POS__ "Unexpected pattern/type combination"

let string_of_nexp_con (Nexp_aux (n, l)) =
  match n with
  | Nexp_constant _ -> "NExp_constant"
  | Nexp_id _ -> "Nexp_id"
  | Nexp_var _ -> "Nexp_var"
  | Nexp_app _ -> "Nexp_app"
  | Nexp_if _ -> "Nexp_if"
  | Nexp_times _ -> "Nexp_times"
  | Nexp_sum _ -> "Nexp_sum"
  | Nexp_minus _ -> "Nexp_minus"
  | Nexp_neg _ -> "Nexp_neg"
  | Nexp_exp _ -> "Nexp_exp"

let doc_nexp ctxt (Nexp_aux (n, l) as nexp) =
  match n with
  | Nexp_constant i -> string (Big_int.to_string i)
  | Nexp_var ki -> doc_kid ctxt ki
  | _ -> failwith ("NExp " ^ string_of_nexp_con nexp ^ " " ^ string_of_nexp nexp ^ " not translatable yet.")

let string_of_typ_con (Typ_aux (t, _)) =
  match t with
  | Typ_app _ -> "Typ_app"
  | Typ_var _ -> "Typ_var"
  | Typ_fn _ -> "Typ_fn"
  | Typ_tuple _ -> "Typ_tuple"
  | Typ_exist _ -> "Typ_exist"
  | Typ_bidir _ -> "Typ_bidir"
  | Typ_internal_unknown -> "Typ_internal_unknown"
  | Typ_id _ -> "Typ_id"

let rec doc_typ ctxt (Typ_aux (t, _) as typ) =
  match t with
  | Typ_id (Id_aux (Id "unit", _)) -> string "Unit"
  | Typ_id (Id_aux (Id "int", _)) -> string "Int"
  | Typ_id (Id_aux (Id "bool", _)) -> string "Bool"
  | Typ_id (Id_aux (Id "bit", _)) -> parens (string "BitVec 1")
  | Typ_id (Id_aux (Id "nat", _)) -> string "Nat"
  | Typ_app (Id_aux (Id "bitvector", _), [A_aux (A_nexp m, _)]) | Typ_app (Id_aux (Id "bits", _), [A_aux (A_nexp m, _)])
    ->
      parens (string "BitVec " ^^ doc_nexp ctxt m)
  | Typ_app (Id_aux (Id "atom", _), [A_aux (A_nexp (Nexp_aux (Nexp_var ki, _)), _)]) ->
      string "Int" (* TODO This probably has to be generalized *)
  | Typ_app (Id_aux (Id "register", _), t_app) ->
      string "register_ref Unit Unit " (* TODO: Replace units with real types. *)
      ^^ separate_map comma (doc_typ_app ctxt) t_app
  | Typ_app (Id_aux (Id "implicit", _), [A_aux (A_nexp (Nexp_aux (Nexp_var ki, _)), _)]) ->
      underscore (* TODO check if the type of implicit arguments can really be always inferred *)
  | Typ_tuple ts -> parens (separate_map (space ^^ string "×" ^^ space) (doc_typ ctxt) ts)
  | Typ_id (Id_aux (Id id, _)) -> string id
  | _ -> failwith ("Type " ^ string_of_typ_con typ ^ " " ^ string_of_typ typ ^ " not translatable yet.")

and doc_typ_app ctxt (A_aux (t, _) as typ) =
  match t with
  | A_typ t' -> doc_typ ctxt t'
  | A_bool nc -> failwith ("Constraint " ^ string_of_n_constraint nc ^ "not translatable yet.")
  | A_nexp m -> doc_nexp ctxt m

let rec captured_typ_var ((i, Typ_aux (t, _)) as typ) =
  match t with
  | Typ_app (Id_aux (Id "atom", _), [A_aux (A_nexp (Nexp_aux (Nexp_var ki, _)), _)])
  | Typ_app (Id_aux (Id "implicit", _), [A_aux (A_nexp (Nexp_aux (Nexp_var ki, _)), _)]) ->
      Some (i, ki)
  | _ -> None

let doc_typ_id ctxt (typ, fid) = flow (break 1) [doc_id_ctor ctxt fid; colon; doc_typ ctxt typ]

let doc_kind (K_aux (k, _)) =
  match k with
  | K_int -> string "Int"
  | K_bool -> string "Bool"
  | _ -> failwith ("Kind " ^ string_of_kind_aux k ^ " not translatable yet.")

let doc_typ_arg ctxt ta = string "foo" (* TODO implement *)

let rec doc_nconstraint ctxt (NC_aux (nc, _)) =
  match nc with
  | NC_and (n1, n2) -> flow (break 1) [doc_nconstraint ctxt n1; string "∧"; doc_nconstraint ctxt n2]
  | NC_or (n1, n2) -> flow (break 1) [doc_nconstraint ctxt n1; string "∨"; doc_nconstraint ctxt n2]
  | NC_equal (a1, a2) -> flow (break 1) [doc_typ_arg ctxt a1; string "="; doc_typ_arg ctxt a2]
  | NC_not_equal (a1, a2) -> flow (break 1) [doc_typ_arg ctxt a1; string "≠"; doc_typ_arg ctxt a2]
  | NC_app (f, args) -> string (string_of_id f) ^^ parens (separate_map comma_sp (doc_typ_arg ctxt) args)
  | NC_false -> string "false"
  | NC_true -> string "true"
  | NC_ge (n1, n2) -> flow (break 1) [doc_nexp ctxt n1; string "≥"; doc_nexp ctxt n2]
  | NC_le (n1, n2) -> flow (break 1) [doc_nexp ctxt n1; string "≤"; doc_nexp ctxt n2]
  | NC_gt (n1, n2) -> flow (break 1) [doc_nexp ctxt n1; string ">"; doc_nexp ctxt n2]
  | NC_lt (n1, n2) -> flow (break 1) [doc_nexp ctxt n1; string "<"; doc_nexp ctxt n2]
  | NC_id i -> string (string_of_id i)
  | NC_set (n, vs) ->
      flow (break 1)
        [
          doc_nexp ctxt n;
          string "∈";
          implicit_parens (separate_map comma_sp (fun x -> string (Nat_big_num.to_string x)) vs);
        ]
  | NC_var ki -> doc_kid ctxt ki

let doc_quant_item ctxt (QI_aux (qi, _)) =
  match qi with
  | QI_id (KOpt_aux (KOpt_kind (k, ki), _)) -> flow (break 1) [doc_kid ctxt ki; colon; doc_kind k]
  | QI_constraint c -> doc_nconstraint ctxt c

let doc_typ_quant ctxt tq = match tq with TypQ_tq qs -> List.map (doc_quant_item ctxt) qs | TypQ_no_forall -> []

let lean_escape_string s = Str.global_replace (Str.regexp "\"") "\"\"" s

let doc_lit (L_aux (lit, l)) =
  match lit with
  | L_unit -> string "()"
  | L_zero -> string "0#1"
  | L_one -> string "1#1"
  | L_false -> string "false"
  | L_true -> string "true"
  | L_num i ->
      let s = Big_int.to_string i in
      string s
  | L_hex n -> utf8string ("0x" ^ n)
  | L_bin n -> utf8string ("0b" ^ n)
  | L_undef -> utf8string "(Fail \"undefined value of unsupported type\")"
  | L_string s -> utf8string ("\"" ^ lean_escape_string s ^ "\"")
  | L_real s -> utf8string s (* TODO test if this is really working *)

let string_of_pat_con (P_aux (p, _)) =
  match p with
  | P_app _ -> "P_app"
  | P_wild -> "P_wild"
  | P_lit _ -> "P_lit"
  | P_or _ -> "P_or"
  | P_not _ -> "P_not"
  | P_as _ -> "P_as"
  | P_typ _ -> "P_typ"
  | P_id _ -> "P_id"
  | P_var _ -> "P_var"
  | P_vector _ -> "P_vector"
  | P_vector_concat _ -> "P_vector_concat"
  | P_vector_subrange _ -> "P_vector_subrange"
  | P_tuple _ -> "P_tuple"
  | P_list _ -> "P_list"
  | P_cons _ -> "P_cons"
  | P_string_append _ -> "P_string_append"
  | P_struct _ -> "P_struct"

let rec doc_pat ctxt apat_needed (P_aux (p, (l, annot))) =
  let env = env_of_annot (l, annot) in
  let typ = Env.expand_synonyms env (typ_of_annot (l, annot)) in
  match p with
  (* Special case translation of the None constructor to remove the unit arg *)
  | P_app (id, _) when string_of_id id = "None" -> string "None"
  | P_app (id, (_ :: _ as pats)) -> begin
      let pats_pp = separate_map comma (doc_pat ctxt true) pats in
      let pats_pp = match pats with [_] -> pats_pp | _ -> parens pats_pp in
      let ppp = doc_unop (doc_id_ctor ctxt id) pats_pp in
      if apat_needed then parens ppp else ppp
    end
  | P_app (id, []) -> doc_id_ctor ctxt id
  | P_lit lit -> doc_lit lit
  | P_wild -> underscore
  | P_id id -> doc_id ctxt id
  | P_var (p, _) -> doc_pat ctxt true p
  | P_as (p, id) -> parens (separate space [doc_pat ctxt true p; string "as"; doc_id ctxt id])
  | P_typ (ptyp, p) ->
      let doc_p = doc_pat ctxt true p in
      doc_p
  (* Type annotations aren't allowed everywhere in patterns in Coq *)
  (*parens (doc_op colon doc_p (doc_typ typ))*)
  | P_vector pats ->
      let ppp = brackets (separate_map semi (fun p -> doc_pat ctxt true p) pats) in
      if apat_needed then parens ppp else ppp
  | P_vector_concat pats ->
      raise
        (Reporting.err_unreachable l __POS__
           "vector concatenation patterns should have been removed before pretty-printing"
        )
  | P_vector_subrange _ -> unreachable l __POS__ "Must have been rewritten before Coq backend"
  | P_tuple pats -> (
      match pats with [p] -> doc_pat ctxt apat_needed p | _ -> parens (separate_map comma_sp (doc_pat ctxt false) pats)
    )
  | P_list pats -> brackets (separate_map semi (doc_pat ctxt false) pats)
  | P_cons (p, p') ->
      let ppp = doc_op (string "::") (doc_pat ctxt true p) (doc_pat ctxt true p') in
      if apat_needed then parens ppp else ppp
  | P_string_append _ -> unreachable l __POS__ "string append pattern found in Coq backend, should have been rewritten"
  | P_struct (fpats, _) ->
      let type_id =
        match typ with
        | (Typ_aux (Typ_id tid, _) | Typ_aux (Typ_app (tid, _), _)) when Env.is_record tid env -> tid
        | _ -> Reporting.unreachable l __POS__ "P_struct pattern with no record type"
      in
      string "{|" ^^ space
      ^^ separate_map (semi ^^ space)
           (fun (field, pat) -> separate space [doc_field_name ctxt type_id field; coloneq; doc_pat ctxt false pat])
           fpats
      ^^ space ^^ string "|}"
  | P_not _ -> unreachable l __POS__ "Coq backend doesn't support not patterns"
  | P_or _ -> unreachable l __POS__ "Coq backend doesn't support or patterns yet"

let rebind_cast_pattern_vars pat typ exp =
  let rec aux pat typ =
    match (pat, typ) with
    | P_aux (P_typ (target_typ, P_aux (P_id id, (l, ann))), _), source_typ when not (is_enum (env_of exp) id) ->
        if Typ.compare target_typ source_typ == 0 then []
        else (
          let l = Parse_ast.Generated l in
          let cast_annot = Type_check.replace_typ source_typ ann in
          let e_annot = Type_check.mk_tannot (env_of exp) source_typ in
          [LB_aux (LB_val (pat, E_aux (E_id id, (l, e_annot))), (l, ann))]
        )
    | P_aux (P_tuple pats, _), Typ_aux (Typ_tuple typs, _) -> List.concat (List.map2 aux pats typs)
    | _ -> []
  in
  let add_lb (E_aux (_, ann) as exp) lb = E_aux (E_let (lb, exp), ann) in
  (* Don't introduce new bindings at the top-level, we'd just go into a loop. *)
  let lbs =
    match (pat, typ) with
    | P_aux (P_tuple pats, _), Typ_aux (Typ_tuple typs, _) -> List.concat (List.map2 aux pats typs)
    | _ -> []
  in
  List.fold_left add_lb exp lbs

let string_of_exp_con (E_aux (e, _)) =
  match e with
  | E_block _ -> "E_block"
  | E_ref _ -> "E_ref"
  | E_app_infix _ -> "E_app_infix"
  | E_if _ -> "E_if"
  | E_loop _ -> "E_loop"
  | E_for _ -> "E_for"
  | E_vector_access _ -> "E_vector_access"
  | E_vector_subrange _ -> "E_vector_subrange"
  | E_vector_update _ -> "E_vector_update"
  | E_vector_update_subrange _ -> "E_vector_update_subrange"
  | E_vector_append _ -> "E_vector_append"
  | E_list _ -> "E_list"
  | E_cons _ -> "E_cons"
  | E_struct _ -> "E_struct"
  | E_struct_update _ -> "E_struct_update"
  | E_field _ -> "E_field"
  | E_match _ -> "E_match"
  | E_assign _ -> "E_assign"
  | E_sizeof _ -> "E_sizeof"
  | E_constraint _ -> "E_constraint"
  | E_exit _ -> "E_exit"
  | E_throw _ -> "E_throw"
  | E_try _ -> "E_try"
  | E_return _ -> "E_return"
  | E_assert _ -> "E_assert"
  | E_var _ -> "E_var"
  | E_internal_plet _ -> "E_internal_plet"
  | E_internal_return _ -> "E_internal_return"
  | E_internal_assume _ -> "E_internal_assume"
  | E_internal_value _ -> "E_internal_value"
  | E_id _ -> "E_id"
  | E_lit _ -> "E_lit"
  | E_typ _ -> "E_typ"
  | E_app _ -> "E_app"
  | E_tuple _ -> "E_tuple"
  | E_vector _ -> "E_vector"
  | E_let _ -> "E_let"

let rec doc_exp (ctxt : context) (E_aux (e, (l, annot)) as full_exp) =
  let env = env_of_tannot annot in
  match e with
  | E_id id -> string (string_of_id id) (* TODO replace by a translating via a binding map *)
  | E_lit l -> doc_lit l
  | E_app (Id_aux (Id "internal_pick", _), _) ->
      string "sorry" (* TODO replace by actual implementation of internal_pick *)
  | E_internal_plet (pat, e1, e2) ->
      (* doc_exp ctxt e1 ^^ hardline ^^ doc_exp ctxt e2 *)
      let e0 = doc_pat ctxt false pat in
      let e1_pp = doc_exp ctxt e1 in
      let e2' = rebind_cast_pattern_vars pat (typ_of e1) e2 in
      let e2_pp = doc_exp ctxt e2' in
      (* infix 0 1 middle e1_pp e2_pp *)
      let e0_pp =
        begin
          match pat with
          | P_aux (P_typ (_, P_aux (P_wild, _)), _) -> string ""
          | _ -> separate space [string "let"; e0; string ":="] ^^ space
        end
      in
      e0_pp ^^ e1_pp ^^ hardline ^^ e2_pp
  | E_app (f, args) ->
      let d_id =
        if Env.is_extern f env "lean" then string (Env.get_extern f env "lean")
        else doc_exp ctxt (E_aux (E_id f, (l, annot)))
      in
      let d_args = List.map (doc_exp ctxt) args in
      nest 2 (parens (flow (break 1) (d_id :: d_args)))
  | E_vector vals -> failwith "vector found"
  | E_typ (typ, e) -> begin
      match e with
      | E_aux (E_assign _, _) -> doc_exp ctxt e
      | _ -> parens (separate space [doc_exp ctxt e; colon; doc_typ ctxt typ])
    end
  | E_tuple es -> parens (separate_map (comma ^^ space) (doc_exp ctxt) es)
  | E_let (LB_aux (LB_val (lpat, lexp), _), e) ->
      let id =
        match pat_is_plain_binder env lpat with
        | Some (Some (Id_aux (Id id, _))) -> id
        | Some None -> "x" (* TODO fresh name or wildcard instead of x *)
        | _ -> failwith "Let pattern not translatable yet."
      in
      nest 2 (flow (break 1) [string "let"; string id; coloneq; doc_exp ctxt lexp]) ^^ hardline ^^ doc_exp ctxt e
  | E_struct fexps ->
      let args = List.map (doc_fexp ctxt) fexps in
      braces (separate comma args)
  | E_field (exp, id) -> doc_exp ctxt exp ^^ dot ^^ doc_id_ctor ctxt id
  | E_struct_update (exp, fexps) ->
      let args = List.map (doc_fexp ctxt) fexps in
      braces (doc_exp ctxt exp ^^ string " with " ^^ separate comma args)
  | E_assign ((LE_aux (le_act, tannot) as le), e) -> string "set_" ^^ doc_lexp_deref ctxt le ^^ space ^^ doc_exp ctxt e
  | _ -> failwith ("Expression " ^ string_of_exp_con full_exp ^ " " ^ string_of_exp full_exp ^ " not translatable yet.")

and doc_fexp ctxt (FE_aux (FE_fexp (field, exp), _)) = doc_id_ctor ctxt field ^^ string " := " ^^ doc_exp ctxt exp

and doc_lexp_deref ctxt (LE_aux (lexp, (l, annot))) =
  match lexp with
  | LE_id id -> doc_id ctxt id
  | LE_typ (typ, id) -> doc_id ctxt id
  | _ -> raise (Reporting.err_unreachable l __POS__ "doc_lexp_deref: Unsupported lexp")
let doc_binder ctxt i t =
  let paranthesizer =
    match t with
    | Typ_aux (Typ_app (Id_aux (Id "implicit", _), [A_aux (A_nexp (Nexp_aux (Nexp_var ki, _)), _)]), _) ->
        implicit_parens
    | _ -> parens
  in
  (* Overwrite the id if it's captured *)
  let ctxt = match captured_typ_var (i, t) with Some (i, ki) -> add_single_kid_id_rename ctxt i ki | _ -> ctxt in
  (ctxt, separate space [string (string_of_id i); colon; doc_typ ctxt t] |> paranthesizer)

let doc_funcl_init ctxt (FCL_aux (FCL_funcl (id, pexp), annot)) =
  let env = env_of_tannot (snd annot) in
  let TypQ_aux (tq, l), typ = Env.get_val_spec_orig id env in
  let arg_typs, ret_typ, _ =
    match typ with
    | Typ_aux (Typ_fn (arg_typs, ret_typ), _) -> (arg_typs, ret_typ, no_effect)
    | _ -> failwith ("Function " ^ string_of_id id ^ " does not have function type")
  in
  let pat, _, exp, _ = destruct_pexp pexp in
  let pats, _ = untuple_args_pat arg_typs pat in
  let binders : (id * typ) list =
    pats
    |> List.map (fun (pat, typ) ->
           match pat_is_plain_binder env pat with
           | Some (Some id) -> (id, typ)
           | Some None -> (Id_aux (Id "x", l), typ) (* TODO fresh name or wildcard instead of x *)
           | _ -> failwith "Argument pattern not translatable yet."
       )
  in
  let ctxt, binders =
    List.fold_left
      (fun (ctxt, bs) (i, t) ->
        let ctxt, d = doc_binder ctxt i t in
        (ctxt, bs @ [d])
      )
      (ctxt, []) binders
  in
  let typ_quants = doc_typ_quant ctxt tq in
  let typ_quant_comment =
    if List.length typ_quants > 0 then
      string "/-- Type quantifiers: " ^^ nest 2 (flow comma_sp typ_quants) ^^ string " -/" ^^ hardline
    else empty
  in
  (* Use auto-implicits for type quanitifiers for now and see if this works *)
  let doc_ret_typ = doc_typ ctxt ret_typ in
  let is_monadic = effectful (effect_of exp) in
  (* Add monad for stateful functions *)
  let doc_ret_typ = if is_monadic then string "SailM " ^^ doc_ret_typ else doc_ret_typ in
  let decl_val = [doc_ret_typ; coloneq] in
  (* Add do block for stateful functions *)
  let decl_val = if is_monadic then decl_val @ [string "do"] else decl_val in
  ( typ_quant_comment,
    separate space ([string "def"; string (string_of_id id)] @ binders @ [colon; doc_ret_typ; coloneq])
  )

let doc_funcl_body ctxt (FCL_aux (FCL_funcl (id, pexp), annot)) =
  let _, _, exp, _ = destruct_pexp pexp in
  let is_monadic = effectful (effect_of exp) in
  if is_monadic then nest 2 (flow (break 1) [string "return"; doc_exp ctxt exp]) else doc_exp ctxt exp

let doc_funcl ctxt funcl =
  let signature = doc_funcl_init ctxt funcl in
  nest 2 (signature ^^ hardline ^^ doc_funcl_body ctxt funcl)

let doc_fundef ctxt (FD_aux (FD_function (r, typa, fcls), fannot)) =
  match fcls with
  | [] -> failwith "FD_function with empty function list"
  | [funcl] -> doc_funcl ctxt funcl
  | _ -> failwith "FD_function with more than one clause"

let string_of_type_def_con (TD_aux (td, _)) =
  match td with
  | TD_abbrev _ -> "TD_abbrev"
  | TD_record _ -> "TD_record"
  | TD_variant _ -> "TD_variant"
  | TD_abstract _ -> "TD_abstract"
  | TD_bitfield _ -> "TD_bitfield"
  | TD_enum _ -> "TD_enum"

let doc_typdef ctxt (TD_aux (td, tannot) as full_typdef) =
  match td with
  | TD_enum (Id_aux (Id id, _), fields, _) ->
      let derivers = if List.length fields > 0 then [string "Inhabited"] else [] in
      let fields = List.map (doc_id_ctor ctxt) fields in
      let fields = List.map (fun i -> space ^^ pipe ^^ space ^^ i) fields in
      let enums_doc = concat fields in
      nest 2
        (flow (break 1) [string "inductive"; string id; string "where"]
        ^^ enums_doc ^^ hardline ^^ string "deriving" ^^ space
        ^^ separate (comma ^^ space) derivers
        )
  | TD_record (Id_aux (Id id, _), TypQ_aux (tq, _), fields, _) ->
      let fields = List.map (doc_typ_id ctxt) fields in
      let enums_doc = separate hardline fields in
      let rectyp = doc_typ_quant ctxt tq in
      (* TODO don't ignore type quantifiers *)
      nest 2 (flow (break 1) [string "structure"; string id; string "where"] ^^ hardline ^^ enums_doc)
  | TD_abbrev (Id_aux (Id id, _), tq, A_aux (A_typ t, _)) ->
      nest 2 (flow (break 1) [string "def"; string id; coloneq; doc_typ ctxt t])
  | TD_abbrev (Id_aux (Id id, _), tq, A_aux (A_nexp ne, _)) ->
      nest 2 (flow (break 1) [string "def"; string id; colon; string "Int"; coloneq; doc_nexp ctxt ne])
  | _ -> failwith ("Type definition " ^ string_of_type_def_con full_typdef ^ " not translatable yet.")

let doc_def ctxt (DEF_aux (aux, def_annot) as def) =
  match aux with
  | DEF_fundef fdef -> group (doc_fundef ctxt fdef) ^/^ hardline
  | DEF_type tdef -> group (doc_typdef ctxt tdef) ^/^ hardline
  | _ -> empty

(* Remove all imports for now, they will be printed in other files. Probably just for testing. *)
let rec remove_imports (defs : (Libsail.Type_check.tannot, Libsail.Type_check.env) def list) depth =
  match defs with
  | [] -> []
  | DEF_aux (DEF_pragma ("include_start", _, _), _) :: ds -> remove_imports ds (depth + 1)
  | DEF_aux (DEF_pragma ("include_end", _, _), _) :: ds -> remove_imports ds (depth - 1)
  | d :: ds -> if depth > 0 then remove_imports ds depth else d :: remove_imports ds depth

let pp_ast_lean ({ defs; _ } as ast : Libsail.Type_check.typed_ast) o effect_info =
  let defs = remove_imports defs 0 in
  let global = { effect_info } in
  let ctxt = { empty_context with global } in
  let regs = State.find_registers defs in
  let register_refs = match regs with [] -> empty | _ -> State.register_refs_lean (doc_id ctxt) (doc_typ ctxt) regs in
  let output : document = separate_map empty (doc_def ctxt) defs in
  print o (register_refs ^^ hardline ^^ output);
  ()
