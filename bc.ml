open Printf

type sExpr =
    | Atom of string
    | List of sExpr list

type expr =
    | Num of float
    | Var of string
    | Op1 of string * expr
    | Op2 of string * expr * expr
    | Fct of string * expr list

type statement =
    | Assign of string * expr
    | Return of expr
    | Expr of expr
    | If of expr * statement list * statement list
    | While of expr * statement list
    | For of statement * expr * statement * statement list
    | FctDef of string * string list * statement list

type block = statement list

type env = string * float

type envQueue = env list

let searchQueue (_v:string) (_q:envQueue): env =
    match _v with
    | v -> List.find (fun s -> fst s = v) _q
    | _ -> raise (Failure "not found")

let varEval (_v:string) (_q:envQueue): float  =
    match _v with
    | v -> (
        let e = searchQueue v _q in
        snd e
    )
    | _ -> 0.0 

let rec evalExpr (_e:expr) (_q:envQueue): float  =
    match _e with
    | Num(e) -> e
    | Var(e) -> varEval e _q
    | Op1(op, x) -> (
        match op with
        | "++" -> evalExpr x _q +. 1.
        | "--" -> evalExpr x _q -. 1.
        | "!"  -> if evalExpr x _q != 0.0 then 1. else 0.
        | _ -> 0.0
    )
    | Op2(op, x, y) -> (
        match op with
        | "+" -> evalExpr x _q +. evalExpr y _q
        | "*" -> evalExpr x _q *. evalExpr y _q
        | "/" -> evalExpr x _q /. evalExpr y _q
        | "-" -> evalExpr x _q -. evalExpr y _q
        | "*" -> evalExpr x _q ** evalExpr y _q
        | "==" -> if compare (evalExpr x _q) (evalExpr y _q)=0 then 1. else 0.
        | "!=" -> if compare (evalExpr x _q) (evalExpr y _q)!=0 then 1. else 0.
        | "<" -> if compare (evalExpr x _q) (evalExpr y _q)<0 then 1. else 0.
        | "<=" -> if compare (evalExpr x _q) (evalExpr y _q)<=0 then 1. else 0.
        | ">" -> if compare (evalExpr x _q) (evalExpr y _q)>0 then 1. else 0.
        | ">=" -> if compare (evalExpr x _q) (evalExpr y _q)>=0 then 1. else 0.
        | "&&" -> if compare (evalExpr x _q) (evalExpr y _q)!=0 then 1. else 0.
        | _ -> 0.0
    )
    | Fct(name, xs) -> 0.0

(* Test for expression *)
let%expect_test "evalNum" =
    evalExpr (Op2(">", Num(11.0), Num(10.0))) [] |>
    printf "%F";
    [%expect {| 1. |}]

let evalCode (_code: block) (_q:envQueue): unit =
    (* crate new environment *)
    (* user fold_left  *)
    (* pop the local environment *)
    print_endline "Not implemented"

let evalStatement (s: statement) (q:envQueue): envQueue =
    match s with 
        | Assign(_v, _e) -> (
            let x = evalExpr _e q in
            let newEnv:env = _v, x in
                newEnv :: q
        )
        | Expr(_e) -> (
            print_float (evalExpr _e q); q
        )
        | If(e, codeT, codeF) -> (
            let cond = evalExpr e q in
                if(cond>0.0) then
                    evalCode codeT q
                else
                    evalCode codeF q
            ;q
        )
        | _ -> q (*ignore *)


(*
    v = 10;
    v // display v
 *)


let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v"))
]

	let%expect_test "p1" =
		let e:string = "a" in
		let q:float = 2.0 in
		let newEnv: env = e, q in
		    evalStatement (List.hd p1) [newEnv];
		    [%expect {| 1. |}]

(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)),
        [Assign("v", Op2("+", Var("v"), Num(1.0)))],
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p1" =
    evalCode p2 [];
    [%expect {| 3628800. |}]

(*  Fibbonaci sequence
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
 *)
 (*)
let p3: block =
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "p3" =
    evalCode p3 [];
    [%expect {|
        2.
        5.
    |}]

*)
