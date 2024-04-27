type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
    | AssignStm of id * exp
    | PrintStm of exp list

    and exp = IdExp of id
    | NumExp of int
    | OpExp of exp * binop * exp
    | EseqExp of stm * exp

let prog = 
    CompoundStm( 
        AssignStm ( "a", OpExp( NumExp 5, Plus, NumExp 3 ) ),
        CompoundStm ( 
            AssignStm ("b", 
                EseqExp ( 
                    PrintStm [ IdExp "a"; OpExp ( IdExp "a", Minus, NumExp 1 ) ],
                    OpExp ( NumExp 10, Times, IdExp "a" )
                )
            ),
            PrintStm[ IdExp "b" ]
        )
     )

let rec maxargs stm =
    let rec maxargs_over_exp exp =
        match exp with
        | IdExp _ -> 0
        | NumExp _ -> 0
        | OpExp (fst, _, snd) -> (maxargs_over_exp fst) + (maxargs_over_exp snd)
        | EseqExp (fst_stm, snd_exp) -> (maxargs fst_stm) + (maxargs_over_exp snd_exp)
    in
        match stm with
        | CompoundStm (fst, snd) -> (maxargs fst) + (maxargs snd)
        | AssignStm (_, exp) -> maxargs_over_exp exp
        | PrintStm [] -> 0
        | PrintStm l -> (List.length l) + 
            List.fold_left (fun acc item -> acc + (maxargs_over_exp item)) 0 l
