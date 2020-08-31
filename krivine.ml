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
	| Funcall of exp*exp ;;

type closure = env * exp

and env = (string*ans) list

and ans = 	| N of int 
			| B of bool 
			| Cl of closure;;



type stack = closure list;;

let rec extend (y:env) (x:string) (d:closure) : env = 
	let c = (x,Cl(d)) in c::y;; 

let rec lookup (y:env) (x:string) : ans = match y with 
	| []			-> raise NotFound
	| (n,cl)::tail  -> if x = n then cl else lookup tail x;; 

let rec evaluate (cl:closure) (s:stack) : ans  = match (cl,s) with
	| ((y,Num(n)),s)			->	N(n)

	| ((y,Add(e1,e2)),s)		->	let N(n1) = evaluate (y,e1) s in 
									let N(n2) = evaluate (y,e2) s in
									N(n1+n2)

	| ((y,Mult(e1,e2)),s)		->	let N(n1) = evaluate (y,e1) s in 
									let N(n2) = evaluate (y,e2) s in
									N(n1*n2)

	| ((y,Bool(t)),s)			->	B(t)

	| ((y,Or(e1,e2)),s)			->	let B(n1) = evaluate (y,e1) s in 
									let B(n2) = evaluate (y,e2) s in
									B(n1 || n2)

	| ((y,Ifte(e1,e2,e3)),s)	->	let B(b1) = evaluate (y,e1) s in 
									if b1 then evaluate (y,e2) s else evaluate (y,e3) s

	| ((y,V(x)),s)				->	let a = lookup y x in 
									(match a with
									 | Cl(d) -> evaluate d s
									 | _	 -> a  )

	| ((y,Lambda(x,e)),arg::s1)	->	let y1 = extend y x arg in
									evaluate (y1,e) s1

	| ((y,Lambda(x,e)),[])		->	raise NoArgument 

	| ((y,Funcall(e1,e2)),s)	->	let cl1 = (y,e2) in
									evaluate (y,e1) (cl1::s) ;; 	


(* let env = [("x",N(3));("y",N(4))] in
let exp = Funcall(Lambda("z",Add(V("z"),Num(8))), Num(5)) in 
evaluate (env,exp) [];;
 *)

(* let env = [("x",B(false));("y",N(4))] in
let exp = Funcall(Lambda("z",Or(V("z"),Bool(false))), V("x")) in 
evaluate (env,exp) [];; *)

(* let g2 = Lambda( "x", Add(Mult(V("z"), V("x")), V("y"))) in
let g3 = Lambda("y", g2) in
let g4 = Lambda("z", g3) in
let a4 = Funcall(Funcall( Funcall(g4, Num(5)), Num(6) ), Num(7)) in
evaluate ([],a4) [];; *)

(* let env = [("x",N(13)); ("y",N(14))] in
let exp = V("x") in
evaluate (env,exp) [];; *)

let a = Add(Num(2), Num(7));;
evaluate ([],a) [];;

let t12 = Funcall(Lambda("x", Num(3)), Add(Num(1), Num(0)));;
evaluate ([],t12) [];;	