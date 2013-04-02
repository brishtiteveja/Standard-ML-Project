(******************課題6***********************)
(*
* <EXP> ::= 非負整数 | 英字列 | <COMP> | <FUNC>
* <COMP> ::= "(""+"<EXP><EXP>")" | "(""-"<EXP><EXP>")"
* | "(""*"<EXP><EXP>")" | "(""/"<EXP><EXP>")"
* <FUNC> ::= "(""fact"<EXP>")" | "(""fibo"<EXP>")"
*)

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
			if hd t3 = ")" then 
				(v3,tl t3)
			else raise SyntaxError
		   end
	    	else if hd t = "-" then
	   	   let 
		       	val (v1,t1) = EXP (tl t)
		   	val (v2,t2) = EXP t1
		 	val (v3,t3) = (v1 - v2,t2)
	           in 
		        if hd t3 = ")" then 
				(v3,tl t3)
			else raise SyntaxError
		   end
	    	else if hd t = "*" then
		   let 
		   	val (v1,t1) = EXP (tl t)
		   	val (v2,t2) = EXP t1
		   	val (v3,t3) = (v1 * v2,t2)
	           in 
		        if hd t3 = ")" then 
				(v3,tl t3)
			else raise SyntaxError
		   end
	    	else if hd t = "/" then
		   let 
		   	val (v1,t1) = EXP (tl t)
		   	val (v2,t2) = EXP t1
		  	val (v3,t3) = (v1 div v2,t2)
	           in 
		        if hd t3 = ")" then 
				(v3,tl t3)
			else raise SyntaxError
	           end
	    	else raise SyntaxError
	    else 
		if h = "+" then 
		  let 
		    val (v1,t1) = EXP t
		    val (v2,t2) = EXP t1
		  in 
		    (v1 + v2,t2)
		  end
	        else if h = "-" then
		  let 
		     val (v1,t1) = EXP t
		     val (v2,t2) = EXP t1
		  in 
		     (v1 - v2,t2)
		  end
	        else if h = "*" then
		  let 
		     val (v1,t1) = EXP t
		     val (v2,t2) = EXP t1
		  in 
		     (v1 * v2,t2)
		  end
	        else if h = "/" then
		  let 
		     val (v1,t1) = EXP t
		     val (v2,t2) = EXP t1
		  in 
		    (v1 div v2,t2)
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


(* execution result*
*
* - compute "(+ (* 10 a) (* b c))" [("a",1),("b",2),("c",3)];
* val it = 16 : int
* - findValue "b" [("a",1),("b",2)];
* val it = 2 : int
* - compute "(/ (* (fibo (* (fact a) 2)) (- b c)) a)" [("a",1),("b",2),("c",3)];
* val it = ~2 : int

*))*)*)*)*)



