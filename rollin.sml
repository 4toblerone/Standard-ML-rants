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
 				  | x::xs => if same_string(x,str) then SOME xs 
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