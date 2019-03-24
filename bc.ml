open Printf
open Core


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

type env =
    | Number of float
    | Variable of string * float
    | Function of string * string list * block

type envQueue = env list

type exprRet = float * envQueue

let getExprID (_e:expr): string =
    match _e with
    | Var(x) -> x
	| Op1(s, e) -> s
	| Op2(s, e, e2) -> s
    | _ -> ""

let getID (_n:env): string =
    match _n with
    | Variable(s, f) -> s
    | Function(s, sl, b) -> s
    | Number(f) -> ""
    | _ -> ""

let getParams (_n:env): string list =
	match _n with
	| Function(s, sl, b) -> sl
	| _ -> []

let getCode (_n:env): block =
	match _n with
	| Function(s, sl, b) -> b
	| _ -> []

let getValue (_n:env): float =
    match _n with
    | Variable(s, f) -> f
    | Number(f) -> f
    | Function(s, sl, b) -> 0.0
    | _ -> 0.0

let rec existsInQueue (_v:string) (_q:envQueue): bool =
    match _q with
    | [] -> false
    | n::tl -> (
        if getID n = _v then true
        else existsInQueue _v tl
    )

let rec searchQueue (_v:string) (_q:envQueue): env =
    match _q with
    | [] -> raise (Failure "Variable not in environment")
    | n::tl -> (
        if getID n = _v then n
        else searchQueue _v tl
    )

let expr_of_expr_opt (v): expr =
	match v with
	| Some expr -> expr
	| None -> raise(Failure "no expr found")

let string_of_str_opt (v): string =
	match v with
	| Some str -> str
	| None -> raise(Failure "no string found")

let rec print_list = function
	[] -> ()
	| e::l -> print_string e ; print_string " " ; print_list l

let rec getIndex (_n:env) (_q:envQueue): int =
    match _q with
    | [] -> raise (Failure "Variable not in environment")
    | h :: t -> if _n = h then 0 else 1 + getIndex _n t

let rec list_swap l u v =
  match l with
    | [] -> []
    | h::t -> (if h = u then v
                else if h = v then u
                  else h)::(list_swap t u v)

let removeEl (l:envQueue) (i:int): envQueue =
    match l with
    | [] -> []
    | lst -> let front = (List.slice l 0 i) in
            let back = (List.slice l (i+1) 0) in
            front@back

(* Test for expression *)
let%expect_test "searchQueueTest" =
    searchQueue "v" [Variable("v", 1.0)] |>
    getValue |>
    printf "%f";
    [%expect {| 1.0 |}]

let varEval (_v:string) (_q:envQueue): float  =
    match _v with
    | v -> (
        let e = searchQueue v _q in
        getValue e
    )
    | _ -> 0.0


let rec evalCode (_code: block) (_q:envQueue): envQueue =
    match _code with
        | [] -> ( (* no more code to be evaulauted *)
            Printf.printf ""; _q
        )
        | st::tail -> ( (* eval next statement in list *)
			if (existsInQueue "Return" _q) then (
				Printf.printf "Returned: %f" (getValue (searchQueue "Return" _q ));
				let qq = removeEl _q (getIndex (searchQueue "Return" _q) _q) in
					evalCode [] qq
			)
			else
            let qq = evalStatement st _q in
				evalCode tail qq
        )
        | _ -> print_endline "0"; _q

and evalStatement (s: statement) (q:envQueue): envQueue =
    match s with
        | Assign(_v, _e) -> (
            let (x, qq) = (evalExpr _e q) in
                if existsInQueue _v qq then(
                    let newList = removeEl qq (getIndex (searchQueue _v qq) qq) in
                    let newEnv:env = Variable(_v, x) in
                        newEnv::newList
                    )
                else
                    let newEnv:env = Variable(_v, x) in
                        newEnv::qq
        )
        | Return(_e) -> (
            evalStatement (Assign("Return", _e)) q

        )
        | Expr(_e) -> (
            let (x, qq) = (evalExpr _e q) in
			let op = (getExprID _e) in
			if(abs (compare "++" op) > 0 && abs (compare "--" op) > 0 )
			then (Printf.printf "%f\n" x); qq
        )
        | If(e, codeT, codeF) -> (
            let (cond, qq) = (evalExpr e q) in
            if cond > 0.0 then
                evalCode codeT qq
            else evalCode codeF qq
        )
        | While(e, code) -> (
            let qq = ref (snd (evalExpr e q)) in (
                let cond = ref (fst (evalExpr e !qq)) in (
                    while (!cond>0.0) do
                        qq := evalCode code !qq;
                        cond := fst (evalExpr e !qq)
                    done; !qq
                )
            )
        )
        | For(s, e, st, code) -> (
            let new_q = evalStatement s q in
                let qq = ref new_q in
                    let cond = ref (fst (evalExpr e !qq)) in
                        while (!cond>0.0) do
                            qq := evalCode code !qq;
                            qq := evalStatement st !qq;
                            cond := fst (evalExpr e !qq)
                        done; !qq
        )
        | FctDef(s, params, code) -> (
            if (existsInQueue s q) then (
				print_list params;
                let newList = (removeEl q (getIndex (searchQueue s q) q)) in
                let qq = ref newList in
                    for x = 0 to (List.length params) do (
                    let i = List.nth params x in
					let iStr = string_of_str_opt i in
                        qq := (evalStatement (Assign(iStr, Num(0.0))) !qq);
                    )
                    done;
                    let newFunc:env = Function(s, params, code) in
                        newFunc::!qq

            )
            else (
                let newFunc:env = Function(s, params, code) in
                    newFunc::q
            );
        )
        | _ -> q (*ignore *)

and evalExpr (_e:expr) (_q:envQueue): exprRet =
    match _e with
    | Num(e) -> (e, _q)
    | Var(e) -> (varEval e _q, _q)
    | Op1(op, x) -> (
        match op with
        | "++" -> (
            let varName = getID (searchQueue (getExprID x) _q) in
            let qq = evalCode [Assign(varName, Op2("+", x, Num(1.0)))] _q in
                evalExpr x qq
        )
        | "--" -> (
            let varName = getID (searchQueue (getExprID x) _q) in
            let qq = evalCode [Assign(varName, Op2("-", x, Num(1.0)))] _q in
                evalExpr x qq
        )
        | "!"  -> if Float.abs (fst (evalExpr x _q)) > 0.0 then (1., _q) else (0., _q)
        | _ -> (0.0, _q)
    )
    | Op2(op, x, y) -> (
        match op with
        | "+" -> (fst (evalExpr x _q) +. fst (evalExpr y _q), _q)
        | "*" -> (fst (evalExpr x _q) *. fst (evalExpr y _q), _q)
        | "/" -> (fst (evalExpr x _q) /. fst (evalExpr y _q), _q)
        | "-" -> (fst (evalExpr x _q) -. fst (evalExpr y _q), _q)
        | "*" -> (fst (evalExpr x _q) ** fst (evalExpr y _q), _q)
        | "==" -> if compare (fst (evalExpr x _q)) (fst (evalExpr y _q))=0 then (1.0, _q) else (0.0, _q)
        | "!=" -> if abs (compare (fst (evalExpr x _q)) (fst (evalExpr y _q)))>0 then (1., _q) else (0., _q)
        | "<" -> if compare (fst (evalExpr x _q)) (fst (evalExpr y _q))<0 then (1., _q) else (0., _q)
        | "<=" -> if compare (fst (evalExpr x _q)) (fst (evalExpr y _q))<=0 then (1., _q) else (0., _q)
        | ">" -> if compare (fst (evalExpr x _q)) (fst (evalExpr y _q))>0 then (1., _q) else (0., _q)
        | ">=" -> if compare (fst (evalExpr x _q)) (fst (evalExpr y _q))>=0 then (1., _q) else (0., _q)
        | "&&" -> if abs (compare (fst (evalExpr x _q)) (fst (evalExpr y _q)))>0 then (1., _q) else (0., _q)
        | _ -> (0.0, _q)
    )
    | Fct(name, xs) -> (*name is func_name, xs is list of args*)(
        if existsInQueue name _q then(
			let qq = ref _q in
			let calledFunc = searchQueue name !qq in
			let params = getParams calledFunc in
				for x = 0 to ((List.length params)-1) do (
					let i = List.nth params x in
					let iStr = string_of_str_opt i in
					let arg = expr_of_expr_opt (List.nth xs x) in
						qq := (evalStatement (Assign(iStr, arg)) !qq);
				)
				done;
				let bl = getCode calledFunc in
					(0.0, evalCode bl !qq)
        )
        else
            raise (Failure "Function not defined")
    )


let rec searchAndReplace (_v:string) (_e:expr) (_q:envQueue): envQueue =
    match _q with
    | [] -> raise (Failure "Variable not in environment")
    | n::tl -> (
        if getID n = _v then
            let x = fst (evalExpr _e _q) in
            let newEnv:env = Variable(_v, x) in
            list_swap _q n newEnv
        else searchAndReplace _v _e tl
    )


(* Test for expression *)
let%expect_test "evalNum" =
    fst (evalExpr (Num 10.0) [])|>
    printf "%F";
    [%expect {| 10. |}]


(* Test for nested expresions *)
let%expect_test "evalNum" =
    fst (evalExpr (Op2("-", Num 40.0, Op2("+", Num 20.0, Num 10.0))) []) |>
    printf "%F";
    [%expect {| 10. |}]


(*
    v = 10;
    v // display v
 *)


let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v"))
]

	let%expect_test "p1" =
		let _ = (evalCode p1 []) in print_endline "";
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
            Expr(Op1("++", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p2" =
    let _ = evalCode p2 [] in print_endline "";
    [%expect {| 3628800. |}]


(*While test*)
let p4: block = [
    Assign("v", Num(5.0));
    While(
        Op2("<", Var("v"), Num(10.0)),
        [
            Assign("v", Op2("+", Var("v"), Num(1.0)));
        ]
    );
    Expr(Var("v"))
]

let%expect_test "p4" =
    let _ = evalCode p4 [] in print_endline "";
    [%expect {| 10. |}]

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

let p3: block =
    [
        FctDef("f", ["x"], [
            (* If(
                Op2("<", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))]) *)
				Return(Num(34.0));
		]);
        Expr(Fct("f", [Num(3.0)]));
		Expr(Fct("f", [Num(3.0)]));
		Expr(Fct("f", [Num(3.0)]));
		Assign("c", Num(5.0));
		Expr(Var("c"))
        (* Expr(Fct("f", [Num(5.0)])); *)
    ]

let%expect_test "p3" =
	let _ = evalCode p3 [] in print_endline "";
    [%expect {|
        2.
        5.
    |}]
