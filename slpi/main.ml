type id = string
type binop = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

let prog =
  CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
      CompoundStm
        ( AssignStm
            ( "b",
              EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                  OpExp (NumExp 10, Times, IdExp "a") ) ),
          PrintStm [ IdExp "b" ] ) )

exception VariableUndefined of string

type variables = id * int list

let rec maxargs stm =
  let rec maxargs_over_exp exp =
    match exp with
    | IdExp _ -> 0
    | NumExp _ -> 0
    | OpExp (fst, _, snd) -> maxargs_over_exp fst + maxargs_over_exp snd
    | EseqExp (fst_stm, snd_exp) -> maxargs fst_stm + maxargs_over_exp snd_exp
  in
  match stm with
  | CompoundStm (fst, snd) -> maxargs fst + maxargs snd
  | AssignStm (_, exp) -> maxargs_over_exp exp
  | PrintStm [] -> 0
  | PrintStm l ->
      List.length l
      + List.fold_left (fun acc item -> acc + maxargs_over_exp item) 0 l

let interp stm =
  let ident_exists vars ident =
    match vars with
    | [] -> None
    | ls ->
        List.find_index (fun (on_list_ident, _) -> ident == on_list_ident) ls
  in

  let update vars entry =
    let idx_option = ident_exists vars (fst entry) in
    match (vars, idx_option) with
    | [], _ -> [ entry ]
    | ls, None -> [ entry ] @ ls
    | ls, Some idx ->
        List.map
          (fun (curr_id, curr_v) ->
            if curr_id == fst entry then entry else (curr_id, curr_v))
          ls
  in

  let get vars ident =
    List.find (fun (on_list_ident, _) -> String.equal on_list_ident ident) vars
  in

  let rec eval_exp exp variables =
    match exp with
    | IdExp id -> get variables id |> snd
    | NumExp n -> n
    | OpExp (fst, op, snd) -> (
        let fst_eval = eval_exp fst variables in
        let snd_eval = eval_exp snd variables in
        match op with
        | Plus -> fst_eval + snd_eval
        | Minus -> fst_eval - snd_eval
        | Times -> fst_eval * snd_eval
        | Div -> fst_eval / snd_eval)
    | EseqExp (stm, exp) -> interp_stm stm variables |> eval_exp exp
  and interp_stm stm variables =
    match stm with
    | CompoundStm (fst, snd) -> interp_stm fst variables |> interp_stm snd
    | AssignStm (ident, exp) ->
        eval_exp exp variables |> fun v -> update variables (ident, v)
    | PrintStm exp_list -> (
        match exp_list with
        | [] -> variables
        | ls ->
            let _ =
              List.iter
                (fun a -> Printf.printf "%d\n" (eval_exp a variables))
                ls
            in
            variables)
  in
  let _ = interp_stm stm [] in
  ()

let () = interp prog
