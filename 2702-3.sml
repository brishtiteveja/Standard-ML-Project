(******************拡張課題3***********************
*)

fun cisOpr c = c = #"+" orelse c = #"-" orelse c = #"*" orelse c = #"/"
              orelse c = #"(" orelse c = #")" orelse c = #"^" orelse c = #"%" orelse c = #"=" orelse c = #"|" orelse c = #"<" orelse c = #">";
fun power ((m, n) :(int * int)) = if n <= 0 then 1
           		else (m * power(m, n-1)) :int;

fun compute s mapL= 
 let
   fun EXP nil = raise SyntaxError |
      EXP (h::t) =
	    if isInt h then 
			(toInt h,t)
	    else if isAlp h then
			(findValue h mapL,t)		
	    else if h = "(" orelse h = "fact" orelse h = "fibo" orelse h = "+" orelse h = "-" orelse h = "*" orelse h = "/" orelse h = "^" orelse h = "%" then 
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
		else if hd t = "^" then
		   let
		      val (v1,t1) = EXP (tl t)
		      val (v2,t2) = EXP t1
		      val (v3,t3) = (power (v1,v2),t2)
		   in
	              if hd t3 = ")" then
			 (v3,tl t3)
		      else raise SyntaxError
		   end 
	    	else if hd t = "%" then
		   let
		      val (v1,t1) = EXP (tl t)
		      val (v2,t2) = EXP t1
		      val (v3,t3) = (v1 mod v2,t2)
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
		else if h = "^" then
		  let
		     val (v1,t1) = EXP t
		     val (v2,t2) = EXP t1
		  in
		     (power (v1,v2),t2)
		  end
		else if h = "%" then
		  let
		     val (v1,t1) = EXP t
		     val (v2,t2) = EXP t1
		  in
	             (v1 mod v2,t2)
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

(* execution result 
* - compute_KK3 "% 10 3" [];
* val it = 1 : int
* - compute_KK3 "(+ (% 10 3) (^ a b))" [("a",4),("b",2)];
* val it = 17 : int
*)


(* 中間値法数式 *)
fun compute_M s mapL=
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
					    else if (hd t2) = "+" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" orelse (hd t2) = "^" orelse (hd t2) = "%" orelse (hd t2) = "=" orelse (hd t2) = "<" orelse (hd t2) = ">"  then
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
					    else if (hd t2) = "-" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" orelse (hd t2) = "^" orelse (hd t2) = "%" orelse (hd t2) = "=" orelse (hd t2) = "<" orelse (hd t2) = ">"  then
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
					    else if (hd t2) = "*" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" orelse (hd t2) = "^" orelse (hd t2) = "%" orelse (hd t2) = "=" orelse (hd t2) = "<" orelse (hd t2) = ">"  then
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
					    else if (hd t2) = "*" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" orelse (hd t2) = "^" orelse (hd t2) = "%" orelse (hd t2) = "=" orelse (hd t2) = "<" orelse (hd t2) = ">"  then
						let
						   val (res,tt) = EXP (Int.toString(term2)::t2)
					    	in
						   (term1 div res,t2)
						end
					    else	
						(term1 div term2,t2)
					 end
				      else if (hd t1) = "^" then
					 let 
					   val (term2,t2) = TERM (tl t1)
					 in 
					    if t2 = [] then
						(power (term1,term2),t2)
					    else if (hd t2) = "*" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" orelse (hd t2) = "^" orelse (hd t2) = "%" orelse (hd t2) = "=" orelse (hd t2) = "<" orelse (hd t2) = ">"  then
						let
						   val (res,tt) = EXP (Int.toString(term2)::t2)
					    	in
						   (power (term1,res),t2)
						end
					    else	
						(power (term1,term2),t2)
					 end
				      else if (hd t1) = "%" then
					 let 
					   val (term2,t2) = TERM (tl t1)
					 in 
					    if t2 = [] then
						(term1 mod term2,t2)
					    else if (hd t2) = "*" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" orelse (hd t2) = "^" orelse (hd t2) = "%" orelse (hd t2) = "=" orelse (hd t2) = "<" orelse (hd t2) = ">"  then
						let
						   val (res,tt) = EXP (Int.toString(term2)::t2)
					    	in
						   (term1 mod res,t2)
						end
					    else	
						(term1 mod term2,t2)
					 end
				      else if (hd t1) = "=" then
					 let 
					   val (term2,t2) = TERM (tl t1)
					 in 
					    if t2 = [] then
						(if term1 = term2 then 1 else 0,t2)
					    else if (hd t2) = "*" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" orelse (hd t2) = "^" orelse (hd t2) = "%" orelse (hd t2) = "=" orelse (hd t2) = "<" orelse (hd t2) = ">"  then
						let
						   val (res,tt) = EXP (Int.toString(term2)::t2)
					    	in
						   (if term1 = res then 1 else 0,t2)
						end
					    else	
						(if term1 = term2 then 1 else 0,t2)
					 end
				      else if (hd t1) = "<" then
					 let 
					   val (term2,t2) = TERM (tl t1)
					 in 
					    if t2 = [] then
						(if term1 < term2 then 1 else 0,t2)
					    else if (hd t2) = "*" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" orelse (hd t2) = "^" orelse (hd t2) = "%" orelse (hd t2) = "=" orelse (hd t2) = "<" orelse (hd t2) = ">" then
						let
						   val (res,tt) = EXP (Int.toString(term2)::t2)
					    	in
						   (if term1 < res then 1 else 0,t2)
						end
					    else	
						(if term1 < term2 then 1 else 0,t2)
					 end
				      else if (hd t1) = ">" then
					 let 
					   val (term2,t2) = TERM (tl t1)
					 in 
					    if t2 = [] then
						(if term1 = term2 then 1 else 0,t2)
					    else if (hd t2) = "*" orelse (hd t2) = "-" orelse (hd t2) = "*" orelse (hd t2) = "/" orelse (hd t2) = "^" orelse (hd t2) = "%" orelse (hd t2) = "=" orelse (hd t2) = "<" orelse (hd t2) = ">" then
						let
						   val (res,tt) = EXP (Int.toString(term2)::t2)
					    	in
						   (if term1 > res then 1 else 0,t2)
						end
					    else	
						(if term1 > term2 then 1 else 0,t2)
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

(* execution result 
* - compute "% 10 3" [];
* val it = 1 : int
* - compute "(+ (% 10 3) (^ a b))" [("a",4),("b",2)];
* val it = 17 : int
* - compute_M "((3^2)+5*(10%7))" [];
* val it = 24 : int
* - compute_M "((3^a)+5*b*(10%c))" [("a",2),("b",3),("c",4)];
* val it = 39 : int
* - compute_M "2=3" [];
* val it = 0 : int
* - compute_M "2<3" [];
* val it = 1 : int
* compute_M "(a>(b=c))" [("a",5),("b",3),("c",2)];
* val it = 1 : int
*)
