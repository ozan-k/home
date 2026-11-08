(* Ozan Kahramanogullari *)
(* A simple ocaml program that solves the scramble squares puzzle. 
The program constructs the state space and tries all the possible solutions.
For the case I tried, the only solution was found after having explored 3%.
The compiled version required approximately 10 mins for this.*)

(* ###################################### *)
(* ###################################### *)
(* ###################################### *)
(* ###################################### *)

(* Michel Quercia's code for generating permutations*)
(* http://caml.inria.fr/pub/ml-archives/ocaml-beginners/2003/02/18d07773b1f69b17895790787eec79ca.en.html *)
let rec swap l x accu = match l with
| a::b::t when b > x -> a :: (swap (b::t) x accu)
| a::t -> (x::t) @ (a::accu)
| _ -> failwith "this can't happen"

(* permut l m accu computes the permutation p' following p = rev(l)@m,
stores it into accu and recalls itself until p has no successor *)
let rec permut l m accu = match l,m with
| a::_, b::t when a > b -> let p = swap l b t in permut [] p (p::accu)
| _, b::t -> permut (b::l) t accu
| _, [] -> accu

(* build x n accu returns [n; ...; x] @ accu *)
let rec build x n accu = if x>n then accu else build (x+1) n (x::accu)

(* permutations n returns the list of all permutations of 1..n *)
let permutations n =
let p = build 1 n [] in permut [] p [p]

(*  End of permutations.*)
(* ############################### *)			 
(* ############################### *)
(* ############################### *)
(* Generating the permutations for all the possible sequences.*)

let all_perms = permutations 9

let _ = print_string "\nPermutations computed.\n\n";
             flush stdout

(* ############################### *)			 
(* ############################### *)
(* ############################### *)
(* ############################### *)
(* ############################### *)
(* ############################### *)
(* All combinations of rotations for each card in clockwise.
   The pair (n,r) denotes r rotations for the n'th card in 
   clockwise direction.*)

let rec combine a lst = match lst with [] -> [] |
                  h::tail -> (a::h)::(combine a tail)

let rec expand set = match set with
                    [] -> [[]]
                | h::t -> (combine (h,0) (expand t))@(combine (h,1) (expand t))@(combine (h,2) (expand t))@(combine (h,3) (expand t))   
        
let rotations = expand [1;2;3;4;5;6;7;8;9]

let _ = print_string "\nRotations computed.\n\n";
             flush stdout

(* ############################### *)
(* ############################### *)
(* ############################### *)
(* ############################### *)
(* ############################### *)
(* ############################### *)
(* The definition of the cards.*)

type shark = St (*Sand_tiger*) | Gw (*Great_white*) | Le (*Leopard*) | Hh (*Hammerhead*)
type half = Hd | Tl

type card = { north : shark * half ;
              south : shark * half ;
              east  : shark * half ;
			  west  : shark * half } 

(* The definition of the cards .*)			  
let c1 = { north = (Hh,Hd) ; south = (St,Tl) ; east = (Gw,Tl) ; west = (Le,Hd) } 
let c2 = { north = (Hh,Hd) ; south = (St,Tl) ; east = (St,Hd) ; west = (Le,Tl) } 
let c3 = { north = (Gw,Tl) ; south = (Hh,Tl) ; east = (St,Tl) ; west = (Le,Tl) } 
let c4 = { north = (Le,Tl) ; south = (Gw,Hd) ; east = (St,Hd) ; west = (Hh,Tl) } 
let c5 = { north = (Hh,Hd) ; south = (Le,Hd) ; east = (Hh,Tl) ; west = (Gw,Hd) } 
let c6 = { north = (Le,Hd) ; south = (Gw,Hd) ; east = (Hh,Tl) ; west = (St,Hd) } 
let c7 = { north = (Hh,Hd) ; south = (St,Hd) ; east = (Le,Tl) ; west = (Gw,Tl) } 
let c8 = { north = (Gw,Hd) ; south = (Le,Tl) ; east = (Hh,Hd) ; west = (St,Hd) } 
let c9 = { north = (St,Hd) ; south = (Le,Tl) ; east = (Hh,Tl) ; west = (Gw,Tl) } 

(* The definition of the cards with a single rotation in clockwise direction.*)			 
let d1 = { north = (Le,Hd) ; south = (Gw,Tl) ; east = (Hh,Hd) ; west = (St,Tl) } 
let d2 = { north = (Le,Tl) ; south = (St,Hd) ; east = (Hh,Hd) ; west = (St,Tl) } 
let d3 = { north = (Le,Tl) ; south = (St,Tl) ; east = (Gw,Tl) ; west = (Hh,Tl) } 
let d4 = { north = (Hh,Tl) ; south = (St,Hd) ; east = (Le,Tl) ; west = (Gw,Hd) } 
let d5 = { north = (Gw,Hd) ; south = (Hh,Tl) ; east = (Hh,Hd) ; west = (Le,Hd) } 
let d6 = { north = (St,Hd) ; south = (Hh,Tl) ; east = (Le,Hd) ; west = (Gw,Hd) } 
let d7 = { north = (Gw,Tl) ; south = (Le,Tl) ; east = (Hh,Hd) ; west = (St,Hd) } 
let d8 = { north = (St,Hd) ; south = (Hh,Hd) ; east = (Gw,Hd) ; west = (Le,Tl) } 
let d9 = { north = (Gw,Tl) ; south = (Hh,Tl) ; east = (St,Hd) ; west = (Le,Tl) }

(* The definition of the cards with two rotations in clockwise direction.*)			 
let e1 = { north = (St,Tl) ; south = (Hh,Hd); east = (Le,Hd) ; west = (Gw,Tl) } 
let e2 = { north = (St,Tl) ; south = (Hh,Hd); east = (Le,Tl) ; west = (St,Hd) } 
let e3 = { north = (Hh,Tl) ; south = (Gw,Tl); east = (Le,Tl) ; west = (St,Tl) } 
let e4 = { north = (Gw,Hd) ; south = (Le,Tl); east = (Hh,Tl) ; west = (St,Hd) } 
let e5 = { north = (Le,Hd) ; south = (Hh,Hd); east = (Gw,Hd) ; west = (Hh,Tl) } 
let e6 = { north = (Gw,Hd) ; south = (Le,Hd); east = (St,Hd) ; west = (Hh,Tl) } 
let e7 = { north = (St,Hd) ; south = (Hh,Hd); east = (Gw,Tl) ; west = (Le,Tl) } 
let e8 = { north = (Le,Tl) ; south = (Gw,Hd); east = (St,Hd) ; west = (Hh,Hd) } 
let e9 = { north = (Le,Tl) ; south = (St,Hd); east = (Gw,Tl) ; west = (Hh,Tl) }

(* The definition of the cards with three rotations in clockwise direction.*)			 
let f1 = { north = (Gw,Tl) ; south = (Le,Hd) ; east = (St,Tl) ;  west = (Hh,Hd)  }
let f2 = { north = (St,Hd) ; south = (Le,Tl) ; east = (St,Tl) ;  west = (Hh,Hd)  }
let f3 = { north = (St,Tl) ; south = (Le,Tl) ; east = (Hh,Tl) ;  west = (Gw,Tl)  }
let f4 = { north = (St,Hd) ; south = (Hh,Tl) ; east = (Gw,Hd) ;  west = (Le,Tl)  }
let f5 = { north = (Hh,Tl) ; south = (Gw,Hd) ; east = (Le,Hd) ;  west = (Hh,Hd)  }
let f6 = { north = (Hh,Tl) ; south = (St,Hd) ; east = (Gw,Hd) ;  west = (Le,Hd)  }
let f7 = { north = (Le,Tl) ; south = (Gw,Tl) ; east = (St,Hd) ;  west = (Hh,Hd)  }
let f8 = { north = (Hh,Hd) ; south = (St,Hd) ; east = (Le,Tl) ;  west = (Gw,Hd)  }
let f9 = { north = (Hh,Tl) ; south = (Gw,Tl) ; east = (Le,Tl) ;  west = (St,Hd)  }

(* ############################### *)
(* ############################### *)
(* ############################### *)

let card_index0 = [ c1 ; c2 ; c3 ; c4 ; c5 ; c6 ; c7 ; c8 ; c9 ]

let card_index1 = [ d1 ; d2 ; d3 ; d4 ; d5 ; d6 ; d7 ; d8 ; d9 ]

let card_index2 = [ e1 ; e2 ; e3 ; e4 ; e5 ; e6 ; e7 ; e8 ; e9 ]

let card_index3 = [ f1 ; f2 ; f3 ; f4 ; f5 ; f6 ; f7 ; f8 ; f9 ]

(* Initializing the board.*)
let current_array = [| c1 ; c2 ; c3 ; c4 ; c5 ; c6 ; c7 ; c8 ; c9 |]

(* ############################### *)
(* ############################### *)
(* ############################### *)
(* Functions for printing. *)

let shark s = match s with 
      St  -> "tiger" | Gw -> "great" | Le -> "leo" | Hh -> "hammer"

let direction d = match d with 
      Hd -> "hd" | Tl -> "tl"

let print_shark (a1,a2) =  print_string ("" ^ (shark a1) ^ "," ^ (direction a2)) 
	  
let print_card { north = n; south = s; east = e ; west = w } =
		         print_string "north= "; print_shark n; print_string (" ;; ");
				 print_string "south= "; print_shark s; print_string (" ;; ");
				 print_string "east= "; print_shark e; print_string (" ;; ");
				 print_string "west= "; print_shark w; print_string (" \n")

(* ############################### *)
(* ############################### *)
(* ############################### *)
(* ############################### *)

let print_permutation_matrix () = 
		   print_card current_array.(0);
	       print_card current_array.(1);
		   print_card current_array.(2);
		   print_string "\n\n";
	       print_card current_array.(3);
	       print_card current_array.(4);
		   print_card current_array.(5);
	       print_string "\n";
		   print_card current_array.(6);
	       print_card current_array.(7);
		   print_card current_array.(8);
		   flush stdout
		   
(* ############################### *)

let rec print_permutation lst = match lst with [] -> () | 
             a::tail ->  print_string ( (string_of_int a) );
		                 if tail = [] then () else print_string " ; ";
		                 print_permutation tail

(* ############################### *)
		   
let rec print_rotation lst = match lst with [] -> () | 
         (a,b)::tail ->  print_string ( (string_of_int a)^ "-" ^(string_of_int b) );
		                 if tail = [] then () else print_string " , ";
		                 print_rotation tail
		   
(* ############################### *)
(* ############################### *)
(* Functions for checking the constraints. *)

let neg direction = 
            match direction with Hd -> Tl | Tl -> Hd
			
(* ############################### *)
			  
let my_compare (a1,b1) (a2,b2) =
       if (a1 = a2) && (neg(b1) = b2) then true else false
	   
(* ############################### *)
		   
let try_this_permutation_real () = 
	   my_compare (current_array.(0).east) (current_array.(1).west) &&
       my_compare (current_array.(1).east) (current_array.(2).west) &&
	   (* %%% *)
       my_compare (current_array.(3).east) (current_array.(4).west) &&
       my_compare (current_array.(4).east) (current_array.(5).west) &&   
       (* %%% *)
	   my_compare (current_array.(6).east) (current_array.(7).west) && 
       my_compare (current_array.(7).east) (current_array.(8).west) &&   
       (* %%% *)
	   (* %%% *)
	   my_compare (current_array.(0).south) (current_array.(3).north) &&
	   my_compare (current_array.(3).south) (current_array.(6).north) &&  
       (* %%% *)
       my_compare (current_array.(1).south) (current_array.(4).north) &&
	   my_compare (current_array.(4).south) (current_array.(7).north) &&
	   (* %%% *)
       my_compare (current_array.(2).south) (current_array.(5).north) && 
	   my_compare (current_array.(5).south) (current_array.(8).north) 
	  
(* ############################### *)
(* ############################### *)
(* ############################### *)
(* ############################### *)
(* ############################### *)
(* Exploration of the state space. *)


let rec get lst  n = match lst with 
                     [] -> failwith "This should not happen"
              | h::tail -> if n = 1 then h else get tail (n-1)

             

let card b = (if b = 0 then card_index0 else
              if b = 1 then card_index1 else 
			  if b = 2 then card_index2 else 
              if b = 3 then card_index3 else 
			  failwith "Error 1")
			  
let rec instantiate comb = match comb with [] -> () | 
          ((a,b),c)::tail -> 
		     current_array.(a-1) <-  (get (card b) c);  			 
		     instantiate tail 
         

let rec this_try combine = 
         instantiate combine;
         try_this_permutation_real () 
							   
let rec try_a perm rotation_list = match rotation_list with 
                          [] -> false | 
					 rotation::tail ->  if this_try (List.combine rotation perm)
					                    then (print_string "Cards: "; print_permutation perm; print_string "\n";
                                              print_string "Directions: "; print_rotation rotation; print_string "\n"; 
											  print_permutation_matrix ();true)
										else try_a perm tail
										
					 
let rec try_permutations lst n = match lst with [] -> (print_string "\n";
                                                       failwith "Tried all possible permutations.") 
       |   permutation::tail -> if try_a permutation rotations 
		                        then print_string "Done.\n"
					            else (if (n mod 10) = 0 then print_string ((string_of_int n) ^ "\n");
									  flush stdout; 
									  try_permutations tail (n+1))
			
(* ############################### *)
(* ############################### *)

let _ = print_string "Trying...\n";
        flush stdout;
	    try_permutations all_perms 1



