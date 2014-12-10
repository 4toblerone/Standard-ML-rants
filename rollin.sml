fun pow(x : int, y : int) = 
	if y = 8
	then 1
	else 5


val s = pow(3,3)

fun swap(pr : int * bool)=
	(#2 pr, #1 pr)

fun sum_two_pairs( pr1 : int*int , pr2 : int*int)=
	(#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun div_mod( x : int , y : int)=
	(x div y, x mod y)


fun sort_pair( pr : int * int)=
	if (#1 pr) <  (#2 pr)
	then pr
	else (#2 pr ,#1 pr)

val it = swap((3,true))
val it =  sort_pair(4,2)

val i = [5,7,8,9]
val d = 6::i
val d = null i
val d = hd i

fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str:string, strlist : string list)=
	case strlist of [] => NONE 
 				  | x::xs => if same_string(x,str) 
 				  			 then SOME xs 
 							 else case all_except_option(str, xs) of
 												SOME y=> SOME (x::y)
 											   | NONE => NONE ;

fun get_substitutions1 (s : string , l : string list list)=
case l of []=> [] 
		| x::xs =>case all_except_option(s,x) of SOME y=> y @ get_substitutions1(s,xs)
															| NONE => [] @ get_substitutions1(s,xs);

fun get_substitutions2(s:string, l: string list list)=
 let fun get_list(acc:string list , s:string, l:string list list)=
 			case l of [] => acc
 				| x::xs => case all_except_option(s,x) of SOME y => get_list(acc@y,s,xs)
 														| NONE => get_list(acc,s,xs)
 in
  	get_list([], s, l)
end;

fun similiar_names (subs : string list list , fullname : {first:string , middle:string, last:string})=
let 
val namelist = get_substitutions1(#first fullname , subs);
	  fun replace (nl : string list, fulln : {first:string , middle:string, last:string})=
 			case nl of [] => []
 						| x::xs => {first = x , middle = #middle fullname, last = #last fullname} :: replace(xs,fulln) 
in 
fullname::replace(namelist,fullname)
end;

val de = similiar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"});

(*Second part of the second assignment*)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

val c = (Clubs ,Num 7);


fun card_color (c : card)=
	case c of (Clubs,_) => Black
			| (Spades,_)=> Black
			| (Diamonds,_)=> Red
			| (Hearts,_)=> Red

fun card_value (c : card)=
	case c of (_, Jack)=> 10
			| (_, Queen)=> 10
			| (_, King)=> 10
			| (_, Ace)=> 11
			| (_, Num i) => i;


fun remove_card(cs : card list, c : card, e: exn)=
	case cs of [] => raise e
			| x::xs => if x = c then xs else x::remove_card(xs,c,e);

(* find way to make it cleaner *)
fun all_same_color1(cs: card list)=
	case cs of [] => true
			| x::[]=>true
			| x::y::xs => if card_color(x)= card_color(y) 
						  then 
						  	  if all_same_color1(y::xs) 
						  	  then true 
						  	  else false
						  else false;

fun sum_cards (cs : card list)=
	let 
		fun helpsum(cs : card list, counter : int)=
			case cs of []=> counter
						| x::xs=> helpsum(xs,counter + card_value(x))
	in
		helpsum(cs,0)
	end;

fun score(cs : card list , goal : int)=
let
	val sum = sum_cards(cs)
	fun prelim_score(cs:card list)=
		if sum > goal 
		then 3*(sum-goal) 
		else goal - sum
in
	if all_same_color1(cs)
	then prelim_score(cs) div 2
	else prelim_score(cs)
end

fun officiate(cs: card list, ml : move list, goal : int)=
	(*returns the score at the end of the
game after processing (some or all of) the moves in the move list in order*)
	1

(*fun factorial 0 = 1
	| factorial(n:int) = n * factorial(n-1);

fun d = fn x=> x+1;

val p = d;*)

(*Part 3*)

(*fun only_capitals(ls)=
	List.filter (fn x=> Char.isUpper(String.sub(x,0))) ls;
*)

fun only_capitals(ls)=
	List.filter (fn x => Char.isUpper(String.sub(x,0))) ls;

fun longest_string1(ls)=
	List.foldl 
		(fn (x,y) =>if String.size(x)>String.size(y) 
					then x 
					else y) 
		"" 
		ls;

fun longest_string2(ls)=
	List.foldl 
		(fn (x,y) =>if String.size(x)>=String.size(y) 
					then x 
					else y) 
		"" 
		ls;

(*Write functions longest_string_helper, longest_string3, and longest_string4 such that:

• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
same behavior as longest_string2.

• longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and longest_string2
but is more general because it takes a function as an argument.

• If longest_string_helper is passed a function that behaves like > (so it returns true exactly
when its first argument is stricly greater than its second), then the function returned has the same
behavior as longest_string1.

• longest_string3 and longest_string4 are defined with val-bindings and partial applications
of longest_string_helper.*)

fun longest_string_helper f ls = 
	List.foldl 
		(fn (x,y) => if f(String.size(x),String.size(y)) then x else y) "" ls;

val longest_string3 = longest_string_helper (fn (x,y) => x>y);

val d = longest_string3 ["sta", "sada"];

val longest_string4 = longest_string_helper (fn (x,y) => x>=y);

(*Write a function longest_capitalized that takes a string list and returns the longest string in
the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings
have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions.
Resolve ties like in problem 2.*)

val longest_capitalized = longest_string2 o only_capitals;

(*Write a function rev_string that takes a string and returns the string that is the same characters in
reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions
in the String module. (Browse the module documentation to find the most useful functions.)
*)

