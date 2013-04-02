(******************拡張課題2***********************
*                 中間値法数式
*)
(* 
<EXP>  ::= <TERM>{{"+"|"-"|"*"|"/"}<TERM>}*
<TERM> ::= 非負整数 | 英字列 | "("<EXP>")"
*)

fun findValue s nil = raise NotDefined
	| findValue s (h::t) = 	let 
				   val (c,v) = h
				in 
				   if s = c then v
				   else findValue s t
				end;


fun compute_KK2 s mapL=
	let
	  fun EXP nil = raise SyntaxError 
		    | EXP (h::t) = let
                                      val (term1,t1) = TERM (h::t)
				   in 
				      if t1 = [] then 
					 (term1,t1)
                                      else if (hd t1) = "+" then
					 let
					    val (term2,t2) = TERM (tl t1)
					 in
					    if t2 = [] then
						(term1+term2,t2)
					    else if (hd t2) = "+" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" then
 						let
						   val (res,tt) = EXP (Int.toString(term2)::t2)
						in
						   (term1 + res ,tt)
						end
					    else
						(term1+term2,t2)
					 end
				      else if (hd t1) = "-" then
					 let
					    val (term2,t2) = TERM (tl t1)
					 in 
					    if t2 = [] then
						(term1 - term2,t2)
					    else if (hd t2) = "-" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/"  then
						let
						   val (res,tt) = EXP (Int.toString(term2)::t2)
						in
						   (term1 - res,tt)
						end
					    else 
						(term1-term2,t2)
					 end
				      else if (hd t1) = "*" then
					 let
					   val (term2,t2) = TERM (tl t1)
					 in 
				            if t2 = [] then
						(term1 * term2 ,t2)
					    else if (hd t2) = "*" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" then
					        let	
						   val (res,tt) = EXP (Int.toString(term2)::t2)
					     	in
						   (term1 * res,tt)	
						end
  					    else
						(term1 * term2,t2)
					 end
				      else if (hd t1) = "/" then
					 let 
					   val (term2,t2) = TERM (tl t1)
					 in 
					    if t2 = [] then
						(term1 div term2,t2)
					    else if (hd t2) = "*" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" then
						let
						   val (res,tt) = EXP (Int.toString(term2)::t2)
					    	in
						   (term1 div res,t2)
						end
					    else	
						(term1 div term2,t2)
					 end	
				      else raise SyntaxError
				   end
		and TERM nil = raise SyntaxError
		    | TERM (h::t) = if isInt h then
		    		      (toInt h,t)
			            else if isAlp h then
				      (findValue h mapL,t)
			            else if h = "(" then
				      let
				         val (v1,t1) = EXP t
				      in
				         if (hd t1) = ")" then
				  	   (v1,tl t1)
				         else if (tl t1) <> [] then
					   EXP (Int.toString(v1)::(tl t1)) 
					 else raise SyntaxError 
				      end
			            else raise SyntaxError
	in
	  let
	     val (result,rest) = EXP (separate s) 
	  in
	     if rest = nil then result else raise SyntaxError
	  end
	end;

(* execution result *) (*
* -compute "((1+a)-(b+3+c+2))" [("a",2),("b",3),("c",4)]; 
* val it = ~9 : int
* -compute "(b+3+c+2)" [("a",2),("b",3),("c",4)];
* val it = 12 : int
* -compute "((1+a)*(b+3+c+2))" [("a",2),("b",3),("c",4)]; 
* val it = 36 : int
* -compute "(1+a)*(b+3*(c+2))" [("a",2),("b",3),("c",4)]; 
* val it = 63 : int
* -compute "(b+(3*(c+2)))" [("a",2),("b",3),("c",4)];
* val it = 21 : int
* -compute "(1+2)*(3+15/(1+2))" [];
* val it = 45 : int
* -compute "(1+2)*(3+15/(1+2))" [];
* val it = 24 : int
*)

