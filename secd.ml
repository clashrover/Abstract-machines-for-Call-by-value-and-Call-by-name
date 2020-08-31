exception NotFound;;
exception NoArgument;;

type exp = 
	| Bool of bool
	| Num of int
	| Add of exp*exp
	| Mult of exp*exp
	| Or of exp*exp
	| Ifte of exp*exp*exp
	| V of string
	| Lambda of string*exp 
	| Funcall of exp*exp 
	| Gtz of exp;;

type closure = env * exp

and env = (string*ans) list

and ans = 	| N of int 
			| B of bool 
			| Cl of env*string*control

and control = opcode list

and stack = ans list

and triple = (stack*env*control) list

and opcode = 
	| LDC of int
	| LDB of bool
	| LOOKUP of string
	| PLUS
	| MULT
	| OR
	| IFTE of (opcode list)*(opcode list)
	| FUNCALL
	| CLOSURE of string*(opcode list)
	| RETURN
	| COND

let rec compile (e:exp) : opcode list =
	match e with
	| V(x) 		  		-> [LOOKUP(x)]
	| Num(n)			-> [LDC(n)]
	| Bool(b)			-> [LDB(b)]
	| Add(e1,e2)		-> (compile e1) @ (compile e2) @ [PLUS]
	| Mult(e1,e2)		-> (compile e1) @ (compile e2) @ [MULT]
	| Or(e1,e2)			-> (compile e1) @ (compile e2) @ [OR]
	| Ifte(e1,e2,e3)	-> (compile e1) @ [IFTE((compile e2), (compile e3))]

	| Lambda(x,e) 		-> [CLOSURE(x, (compile e)@ [RETURN])]
	| Funcall(e1, e2)	-> (compile e1) @ (compile e2) @ [FUNCALL]

	| Gtz(e1)			-> compile e1 @ [COND]


let rec lookup y x = match y with
| [] 			-> raise NotFound
| (n,a)::y2 	-> if n=x then a else lookup y2 x;; 

let rec evaluate (y:env) (c: opcode list) (s:stack) (d: triple) : ans =
	match (y,c,s,d) with
	| (_,[],x::xs,_)         	  -> x
	| (y, COND::c1 , (N(n)::s1), d)	-> if n>0 then evaluate y c1 (B(true)::s1) d else evaluate y c1 (B(false)::s1) d
	| (y, LDC(x) :: c1 , s, d) -> evaluate y c1 (N(x) :: s) d
	| (y, LDB(x) 	:: c1 , s, d) -> evaluate y c1 (B(x):: s) d
	| (y, LOOKUP(x) :: c1 , s, d) -> evaluate y c1 ((lookup y x)::s) d
	| (y, PLUS :: c1 , (n1::n2::s1), d) -> let N(p1) = n1 in let N(p2) = n2 in  
										evaluate y c1 (N(p1+p2)::s1) d

	| (y, MULT :: c1 , (n1::n2::s1), d) -> let N(p1) = n1 in let N(p2) = n2 in  
										evaluate y c1 (N(p1*p2)::s1) d

	| (y, OR :: c1 , (n1::n2::s1), d) -> let B(p1) = n1 in let B(p2) = n2 in  
										evaluate y c1 (B(p1 || p2)::s1) d

	| (y, IFTE(c1,c2)::c3 , (B(b)::s1), d) -> if b then (evaluate y (c1@c3) s1 d) else (evaluate y (c2@c3) s1 d)
	
	| (y, CLOSURE(x,c1)::c2 , s, d) -> evaluate y c2 (Cl(y,x,c1)::s) d             (* When first e1 is compiled which is a lambda we pack the closure *)

	| (y, FUNCALL::c1, (x::Cl(y1, x1, c2)::s1) , d) ->  evaluate ((x1,x)::y1) c2 [] ((s1,y,c1)::d)   (* When funcall is in opcode then stack contains answer and dump contains call time environment *)
	
	| (y, RETURN::c1 , (x::s'), (s1,y1,c2)::d)	-> evaluate y1 c2 (x::s1) d           (* when work of abstraction is done restore call time environment *)
;;

(* let e = Add(V("x"),V("y"));;
let opl = compile e in
evaluate [("x",N(5));("y",N(6))] opl [] [];;	 *)

(* let e = Mult(V("x"),V("y"));;
let opl = compile e in
evaluate [("x",N(5));("y",N(6))] opl [] [];; *)

(* let e = Ifte(V("b"),V("x"),V("y"));;
let opl = compile e in
evaluate [("b",B(false));("x",N(5));("y",N(6))] opl [] [];;		 *)

(* let e = Funcall(Lambda("z1",Add(V("z1"),V("x"))),Add(V("y"),V("x")));;
let opl = compile e in
evaluate [("x",N(5));("y",N(6))] opl [] [];;	 *) 

(* let e = Funcall(Lambda("z1",Funcall(Lambda("z2",Add(V("z1"),V("z2"))), Mult(V("x"),V("y")))),Add(V("y"),V("x")));;
let opl = compile e ;; *)	

(* Above gives  *)
(* [CLOSURE ("z1",[CLOSURE ("z2", [LOOKUP "z1"; LOOKUP "z2"; PLUS; RETURN]); LOOKUP "x";LOOKUP "y"; MULT; FUNCALL; RETURN]);LOOKUP "y"; LOOKUP "x"; PLUS; FUNCALL] *)

(* let e = Funcall(Lambda("z1",Funcall(Lambda("z2",Add(V("z1"),V("z2"))), Mult(V("x"),V("y")))),Add(V("y"),V("x")));;
let opl = compile e in
evaluate [("x",N(5));("y",N(6))] opl [] [];;	 *)

let a = Add(Num(2), Num(7));;
let aa = compile a;;
let aaa = evaluate [] aa [] [];;


let t12 = Funcall(Lambda("x", Num(3)), Add(Num(1), Num(0)));;
let c12 = compile t12;;
evaluate [] c12 [] [];;