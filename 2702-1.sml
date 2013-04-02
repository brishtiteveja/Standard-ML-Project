(******************拡張課題1***********************)

fun findValue s nil = raise NotDefined
	| findValue s (h::t) = 	let 
				   val (c,v) = h
				in 
				   if s = c then v
				   else findValue s t
				end;

(* execution result 
* val findValue = fn : ''a -> (''a * 'b) list -> 'b
* - findValue "a" [("a",1),("b",2)];
* val it = 1 : int
* - findValue "b" [("a",1),("b",2)];
* val it = 2 : int
*)


(** int to string converstion **)

fun chrval n = if n >=10 then raise SyntaxError
		else if n <0  then raise SyntaxError
		else chr(n + ord #"0");  
  (*conversion of nonnegative integer 0~9 to string *)

fun strval(n,l) = if n = 0 then l  (* return the list of characters for n, where l is a  *)
		   else strval(n div 10,chrval(n mod 10)::l);  (* partial list of rightmost digits already processed.*)
fun toStr n = implode(strval(n,[])); (* return the string representation of nonnegative integer n.*)

(*execution result *)
(*
* - toStr 45;
* val it = "45" : string  *)



fun printList nil = print("\n") | 
	printList (h::t) = (
			     print(h ^ " ");
			     printList t
				);
(*****Program has been optimized even for without brackets *)

fun compute s mapL= 
 let
   fun EXP nil = raise SyntaxError |
      EXP (h::t) =
	    if isInt h then 
			(toInt h,t)
	    else if isAlp h then
			(findValue h mapL,t)		
	    else if h = "(" orelse h = "fact" orelse h = "fibo" orelse h = "+" orelse h = "-" orelse h = "*" orelse h = "/" then 
		if h="(" then
			if (hd t) = "fact" orelse (hd t) = "fibo" then
				FUNC (h::t)
			else
				COMP (h::t)
		else
			if h = "fact" orelse h = "fibo" then
				FUNC (h::t)
			else
				COMP (h::t)

	    else raise SyntaxError

      and COMP nil = raise SyntaxError
	| COMP (h::t) = 
	    if h = "(" then
	    	if hd t = "+" then 
		   let 
		   	val (v1,t1) = EXP (tl t)
		   	val (v2,t2) = EXP t1
			val (v3,t3) = (v1 + v2,t2) 
		   in 
			if t3 = [] then 
			   raise SyntaxError
			else if (hd t3) = ")" then
			   (v3,tl t3)
			else if (hd t3) = "(" orelse isInt (hd t3) orelse isAlp (hd t3) orelse (hd t3) = "fact" orelse (hd t3) = "fibo" orelse (hd t3) = "+" orelse (hd t3) = "-" orelse (hd t3) = "*" orelse (hd t3) = "/" then
			   let
				val lst = "("::"+"::toStr(v3)::t3
				val (v4,t4) = COMP lst	
			   in
				(v4,t4)				
			   end
			else raise SyntaxError
		   end
	    	else if hd t = "-" then
	   	   let 
		       	val (v1,t1) = EXP (tl t)
		   	val (v2,t2) = EXP t1
		 	val (v3,t3) = (v1 - v2,t2)
	           in 
			if t3 = [] then
			   raise SyntaxError
			else if (hd t3) = ")" then
			   (v3,tl t3)
			else if (hd t3) = "(" orelse isInt (hd t3) orelse isAlp (hd t3) orelse (hd t3) = "fact" orelse (hd t3) = "fibo" orelse (hd t3) = "+" orelse (hd t3) = "-" orelse (hd t3) = "*" orelse (hd t3) = "/" then
			   let
				val lst = "("::"-"::toStr(v3)::t3
				val (v4,t4) = COMP lst	
			   in
				(v4,t4)				
			   end
			else raise SyntaxError

		   end
		else if hd t = "*" then
		   let 
		       	val (v1,t1) = EXP (tl t)
		   	val (v2,t2) = EXP t1
		 	val (v3,t3) = (v1 * v2,t2)
	           in 
			if t3 = [] then
			   raise SyntaxError
			else if (hd t3) = ")" then
			   (v3,tl t3)
			else if (hd t3) = "(" orelse isInt (hd t3) orelse isAlp (hd t3) orelse (hd t3) = "fact" orelse (hd t3) = "fibo" orelse (hd t3) = "+" orelse (hd t3) = "-" orelse (hd t3) = "*" orelse (hd t3) = "/" then
			   let
				val lst = "("::"*"::toStr(v3)::t3
				val (v4,t4) = COMP lst	
			   in
				(v4,t4)				
			   end
			else raise SyntaxError

		   end
	    	else if hd t = "/" then
		   let 
		       	val (v1,t1) = EXP (tl t)
		   	val (v2,t2) = EXP t1
		 	val (v3,t3) = (v1 div v2,t2)
	           in 
			if t3 = [] then
			   raise SyntaxError
			else if (hd t3) = ")" then
			   (v3,tl t3)
			else if (hd t3) = "(" orelse isInt (hd t3) orelse isAlp (hd t3) orelse (hd t3) = "fact" orelse (hd t3) = "fibo" orelse (hd t3) = "+" orelse (hd t3) = "-" orelse (hd t3) = "*" orelse (hd t3) = "/" then
			   let
				val lst = "("::"/"::toStr(v3)::t3
				val (v4,t4) = COMP lst	
			   in
				(v4,t4)				
			   end
			else raise SyntaxError

		   end
	    	else raise SyntaxError
	    else
		if h = "+" then 
		   let 
		   	val (v1,t1) = EXP t
		   	val (v2,t2) = EXP t1
			val (v3,t3) = (v1 + v2,t2)  
		   in 
			if t3 = [] then 
			   (v3,t3)
			else if (hd t3) = "(" orelse isInt (hd t3) orelse isAlp (hd t3) orelse (hd t3) = "fact" orelse (hd t3) = "fibo" orelse (hd t3) = "+" orelse (hd t3) = "-" orelse (hd t3) = "*" orelse (hd t3) = "/" then
			   let
				val lst = "+"::toStr(v3)::t3
				val (v4,t4) = COMP lst	
			   in
				(v4,t4)				
			   end
			else  
			   raise SyntaxError
		   end
	        else if h = "-" then
		  let 
		   	val (v1,t1) = EXP t
		   	val (v2,t2) = EXP t1
			val (v3,t3) = (v1 - v2,t2)  
		   in 
			if t3 = [] then 
			   (v3,t3)
			else if (hd t3) = "(" orelse isInt (hd t3) orelse isAlp (hd t3) orelse (hd t3) = "fact" orelse (hd t3) = "fibo" orelse (hd t3) = "+" orelse (hd t3) = "-" orelse (hd t3) = "*" orelse (hd t3) = "/" then
			   let
				val lst = "-"::toStr(v3)::t3
				val (v4,t4) = COMP lst	
			   in
				(v4,t4)				
			   end
			else  
			   raise SyntaxError
		   end
	        else if h = "*" then
		  let 
		   	val (v1,t1) = EXP t
		   	val (v2,t2) = EXP t1
			val (v3,t3) = (v1 * v2,t2)  
		   in 
			print("I am here" ^ "\n");
			if t3 = [] then 
			   (v3,t3)
			else if (hd t3) = "(" orelse isInt (hd t3) orelse isAlp (hd t3) orelse (hd t3) = "fact" orelse (hd t3) = "fibo" orelse (hd t3) = "+" orelse (hd t3) = "-" orelse (hd t3) = "*" orelse (hd t3) = "/" then
			   let
				val lst = "*"::toStr(v3)::t3
				val (v4,t4) = COMP lst	
			   in
				(v4,t4)				
			   end
			else  
			   raise SyntaxError
		   end
	        else if h = "/" then
		  let 
		   	val (v1,t1) = EXP t
		   	val (v2,t2) = EXP t1
			val (v3,t3) = (v1 div v2,t2)  
		   in 
			if t3 = [] then 
			   (v3,t3)
			else if (hd t3) = "(" orelse isInt (hd t3) orelse isAlp (hd t3) orelse (hd t3) = "fact" orelse (hd t3) = "fibo" orelse (hd t3) = "+" orelse (hd t3) = "-" orelse (hd t3) = "*" orelse (hd t3) = "/" then
			   let
				val lst = "/"::toStr(v3)::t3
				val (v4,t4) = COMP lst	
			   in
				(v4,t4)				
			   end
			else  
			   raise SyntaxError
		   end
	       else raise SyntaxError
	and FUNC nil = raise SyntaxError
	   | FUNC (h::t) =
		if h = "(" then
		   if (hd t) = "fact" then
			let  
			  val (v1,t1) = EXP (tl t)
			  val (factV,t2) = (fact v1,t1)
			in
			  if (hd t2) = ")" then
				(factV,tl t2)
			  else raise SyntaxError	
			end
		   else if (hd t) = "fibo" then
			let
			  val (v1,t1) = EXP (tl t)
			  val (fiboV,t2) = (fibo v1,t1) 
			in 
 			  if (hd t2) = ")" then
				(fiboV,tl t2)
			  else raise SyntaxError
			end
		   else raise SyntaxError
		else
		   if h = "fact" then
			let 
			   val (v1,t1) = EXP t
			   val (factV,t2) = (fact v1,t1)
			in
    			   (factV,t2)
			end
		   else if h = "fibo" then
			let
			   val (v1,t1) = EXP t
			   val (fiboV,t2) = (fibo v1,t1)
			in
			   (fiboV,t2)
			end
		   else raise SyntaxError		      
  in
   let 
      val (result,rest) = EXP (separate s)
   in
      if rest = nil then result else raise SyntaxError
   end
 end; 


	(*execution result
*
* - compute "(+ 10 2)" [];
* val it = 16 : int
* - findValue "b" [("a",1),("b",2)];
* val it = 2 : int
* - compute "(/ (* (fibo (* (fact a) 2)) (- b c)) a)" [("a",1),("b",2),("c",3)];
* val it = ~2 : int
* - compute "(/ (* (fibo (* (+ (fact a) b c) 2)) (- b c)) a)" [("a",1),("b",2),("c",3)];
* val it = 5 : int
* - compute "(+ 3 4 (* 1 a b) 7 c d e (fact 4) (/ 18 6 3) (fibo 2) (- 10 2 8))" [("a",3),("b",4),("c",1),("d",5),("e",8)];
* val it = 67 : int
* - compute "+ 3 4 7 c d e / 18 6 3" [("a",3),("b",4),("c",1),("d",5),("e",8)];
* val it = 28 : int
* - compute "+ 3 4 7 c d e / 18 6 3" [("a",3),("b",4),("c",1),("d",5),("e",8)];
* val it = 29 : int
* - compute "(+ 3 4 7 c d e (fact 4) (/ 18 6 3) (fibo 2) (- 10 2 8))" [("a",3),("b",4),("c",1),("d",5),("e",8)];
* val it = 55 : int
* - compute "+ 3 4 7 c d e (/ 18 6 3) (- 10 2 8)" [("a",3),("b",4),("c",1),("d",5),("e",8)];
* val it = 29 : int
* - compute "+ 3 4 7 c d e (/ 18 6 3) (- 10 2 3)" [("a",3),("b",4),("c",1),("d",5),("e",8)];
* val it = 34 : int
* - compute "(+ 1 2 3 (* a b c))" [("a",3),("b",5),("c",2)];
* val it = 36 : int
*
*)*)*)*)*)*)*)


