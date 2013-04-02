(************課題3*****************
* <EXP> ::= 非負整数 | <COMP>
* <COMP> ::= "+"<EXP><EXP> | "-"<EXP><EXP> | "*"<EXP><EXP> | "/"<EXP><EXP>
*)

fun compute s =
   let
      fun EXP nil = raise SyntaxError
	| EXP (h::t) =
	    if isInt h then 
			(toInt h,t)
	    else if h = "+" orelse h = "-" orelse h = "*" orelse h = "/" then 
			COMP (h::t)
	    else raise SyntaxError
      and COMP nil = raise SyntaxError
	| COMP (h::t) = 
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
      in 
	let 
	   val (result,rest) = EXP (separate s)
	in
	   if rest = nil then result else raise SyntaxError
	end
      end;		

(* Execution Result
* val compute = fn : string -> int
* - compute "* 4 2";
* val it = 8 : int
* - compute "/ 9 3";
* val it = 3 : int
* - compute "* - 6 3 / 16 4";
* val it = 12 : int
*)

(************課題4*****************)
(* Solution 1
*
* <EXP> ::= 非負整数 | <COMP>
* <COMP> ::= "(""+"<EXP><EXP>")" | "(""-"<EXP><EXP>")"
*	 | "(""*"<EXP><EXP>")" | "(""/"<EXP><EXP>")"
*
* ****Program has been optimized even for without brackets *)


fun compute s =
   let
      fun EXP nil = raise SyntaxError
	| EXP (h::t) =
	    if isInt h then 
			(toInt h,t)	
	    else if h="(" orelse h = "+" orelse h = "-" orelse h = "*" orelse h = "/" then 
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
      in 
	let 
	   val (result,rest) = EXP (separate s)
	in
	   if rest = nil then result else raise SyntaxError
	end
      end;  




(* Solution 2 (Brackets are handled in EXP function) *)

fun compute s =
   let
      fun EXP nil = raise SyntaxError
	| EXP (h::t) =
	    if isInt h then 
			(toInt h,t)
	    else if h = "(" then
		let
		   val (v,t) = EXP t
		in
		   if (hd t) = ")" then
			(v,tl t)
		   else raise SyntaxError
		end		
	    else if h = "+" orelse h = "-" orelse h = "*" orelse h = "/" then 
			COMP (h::t)
	    else raise SyntaxError
      and COMP nil = raise SyntaxError
	| COMP (h::t) = 
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
      in 
	let 
	   val (result,rest) = EXP (separate s)
	in
	   if rest = nil then result else raise SyntaxError
	end
      end;		

(* Solution 3 (For bracket handling, COMP is recursively calling itself) *)

fun compute s =
   let
      fun EXP nil = raise SyntaxError
	| EXP (h::t) =
	    if isInt h then 
			(toInt h,t)
	    else if h = "(" then
		let
		   val (v,t) = EXP t
		in
		   if (hd t) = ")" then
			(v,tl t)
		   else raise SyntaxError
		end		
	    else if h = "(" orelse h = "+" orelse h = "-" orelse h = "*" orelse h = "/" then 
			COMP (h::t)
	    else raise SyntaxError
      and COMP nil = raise SyntaxError
	| COMP (h::t) = 
	    if h = "(" then 
		let
		   val (v,tt) = COMP t
		in
		   if hd tt = ")" then
			(v,tl tt)
		   else raise SyntaxError
		end
	    else if h = "+" then 
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
      in 
	let 
	   val (result,rest) = EXP (separate s)
	in
	   if rest = nil then result else raise SyntaxError
	end
      end;	


(******Execution Result************)
(* - compute "(+ 1 2)"; 
* val it = 3 : int
* - compute "+ 1 2";
* val it = 3 : int 
* - compute "(+ * 1 2 (/ 8 - 3 2))";
* val it = 10 : int 
* - compute "(+ ( * 1 2) (/8 (- 3 2)))";          
* val it = 10 : int 
* - compute "+ * 1 2 / 8 - 3 2";
* val it = 10 : int *)

(************************課題5*************************)
(*
* <EXP> ::= 非負整数 | <COMP> | <FUNC>
* <COMP> ::= "(""+"<EXP><EXP>")" | "(""-"<EXP><EXP>")"
* | "(""*"<EXP><EXP>")" | "(""/"<EXP><EXP>")"
* <FUNC> ::= "(""fact"<EXP>")" | "(""fibo"<EXP>")"
*
* ****Program has been optimized even for without brackets *)

fun compute s = 
 let
   fun EXP nil = raise SyntaxError |
      EXP (h::t) =
	    if isInt h then 
			(toInt h,t)	
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

(* Execution Results 
* - compute "fibo 5";
* val it = 8 : int
* - compute "fact 5";
* val it = 120 : int
* - compute "+ fact fibo 5 - 9 8";
* val it = 40321 : int
* - compute "(+ (fact (fibo 5)) (- 9 8))";
* val it = 40321 : int
* - compute "(+ (fibo 3) 9)";
* val it = 12 : int
* - compute "(/ (fact (fibo 3)) (* (- (fibo 3) 9) (fact 4)))";
* val it = ~1 : int
*
*)*)

