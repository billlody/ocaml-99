(* Ocaml 99 problems by ZHENG Yan *)

(* Unsolved problems: 90, 99 *)

(* Working with lists *)
(* 1 write a function that returns the last element of a list *)
let rec last = function
    [] -> None
  | [elem] -> Some elem
  | h :: t -> last t;;
(* 2 find the last but one elements of a list *)
let rec last_two = function
    [] | [_] -> None
  | t :: _ :: [] -> Some t
  | _ :: (t :: t2 as tl) -> last_two tl;;
(* 3 find the k'th element of a list *)
let rec at n = function
    [] -> None
  | t :: l -> if n = 1 then Some t
  else at (n-1) l;;
(* 4 find the number of elements of a list *)
let rec length = function
    [] -> 0
  | t :: l -> 1 + length l;;
(* 5 reverse a list *)
let rev l = 
  let rec aux acc = function
      [] -> acc
    | h :: t -> aux (h :: acc) t
  in aux [] l;;
(* 6 find out whether a list is a palindrome *)
let is_palindrome l = l = rev l;;
(* 7 flatten a newsted list structure *)
type 'a node =
  | One of 'a
  | Many of 'a node list
let rec flatten l = 
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] l);;
(* 8 eliminate consecutive duplicates of list elements *)
let rec compress = function
    a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | s -> s;;
(* 9 pack consecutive duplicates of list elements into sublists *)
let pack l =
  let rec aux current acc = function
      h :: (t :: _ as m) when h = t -> aux (h :: current) acc m
    | h :: t -> aux [] ((h :: current) :: acc) t
    | [] -> acc
  in
  List.rev (aux [] [] l);;
(* 10 run-length encoding of a list *)
let encode l =
  let rec aux (x, y) acc = function
      h :: (t :: _ as m) when h = t -> aux ((x+1), h) acc m
    | h :: t -> aux (0, h) ((x + 1, h) :: acc) t
    | [] -> acc
  in
  List.rev (aux (0, (List.hd l)) [] l);;
(* 11 modified run-length encoding *)
type 'a rle =
  | One of 'a
  | Many of (int * 'a);;
let encode2 l =
  let rec aux (x, y) acc = function
      h :: (t :: _ as m) when h = t -> aux ((x+1), h) acc m 
    | h :: t -> if x = 0 then aux (0, h) ((One h)::acc) t
    else aux (0, h) (Many(x+1, h)::acc) t
    | [] -> acc
  in
  List.rev (aux (0, (List.hd l)) [] l);;
(* 12 decode a run-length encoded list *)
let decode l =
  let rec aux acc = function
      Many(m, n)::t when m = 1 -> aux (n::acc) t
    | Many(m, n)::t -> aux (n::acc) (Many(m-1, n)::t)
    | One(n)::t -> aux (n::acc) t
    | [] -> acc
  in
  List.rev (aux [] l);;
(* 13 run-length encoding of a list *)
let encode3 l =
  let rle count x = if count = 0 then One x else Many(count, x) in
  let rec aux count acc = function
      [] -> acc
    | [x] -> (rle count x) :: acc
    | a::(b::_ as t) -> if a = b then aux (count+1) acc t
    else aux 0 ((rle (count+1) a)::acc) t
  in
  List.rev (aux 0 [] l);;
(* 14 duplicate the elements of a list *)
let duplicate l =
  let rec aux acc = function
      [] -> acc
    | h :: t -> aux (h::h::acc) t
  in
  List.rev (aux [] l);;
(* 15 replicate the elements of a list a given number of times *)
let replicate l n =
  let rec aux count acc = function
      (h :: t) as x -> if count = 0 then aux n acc t
      else aux (count-1) (h::acc) x
    | [] -> acc
  in
  List.rev (aux n [] l);;
(* 16 drop every N'th element from a list *)
let drop l n =
  let rec aux count acc = function
      h :: t -> if count = 1 then aux n acc t
      else aux (count-1) (h::acc) t
    | [] -> acc
  in
  List.rev (aux n [] l);;
(* 17 split a list into two parts; the length of the first part is given *)
let split l n =
  let rec aux count acc acc2 = function
      h :: t -> if count = 0 then aux 0 acc (h::acc2) t
      else aux (count-1) (h::acc) acc2 t
    | [] -> [(List.rev acc); (List.rev acc2)]
  in
  aux n [] [] l;;
(* 18 extract a slice from a list *)
let slice l n1 n2 =
  let rec aux count acc = function
      h :: t -> if count > n1 && count < n2 then aux (count+1) (h::acc) t
      else aux (count+1) acc t
    | [] -> List.rev acc
  in
  aux 1 [] l;;
(* 19 rotate a list N places to the left *)
let split2 list n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
    else aux (i-1) (h :: acc) t
  in
  aux n [] list
let rotate list n =
  let len = List.length list in
  let n = if len = 0 then 0 else (n mod len + len) mod len in
  if n = 0 then list
  else let a, b = split2 list n in a @ b;;
(* 20 remove the K'th element from a list *)
let remove_at n l =
  let rec aux count acc = function
      h :: t -> if count = 0 then aux (count-1) acc t
      else aux (count-1) (h::acc) t
    | [] -> List.rev acc
  in
  aux n [] l;;
(* 21 insert an element at a given position into a list *)
let insert_at elem n l =
  let rec aux count acc = function
      h :: t -> if count = 0 then aux (count-1) (h::elem::acc) t
      else aux (count-1) (h::acc) t
    | [] -> List.rev acc
  in
  aux n [] l;;
(* 22 create a list containing all integers within a given range *)
let range (n1:int) (n2:int) =
  let rec aux count mark acc = 
    if count >= mark then aux (count-1) mark (count::acc)
    else acc
  in
  if n1 > n2 then List.rev (aux n1 n2 [])
  else aux n2 n1 [];;
(* 23 extract a given number of randomly selected elements from a list *)
let rand_select l num =
  let length = List.length l in
  let rec aux n count acc = function
      h :: t when n != 0 -> if count = 0 then aux (n-1) (Random.int length) (h::acc) l
      else aux n (count-1) acc t
    | [] -> raise Not_found
    | h :: t -> List.rev acc
  in
  aux num (Random.int length) [] l;;
(* 24 Lotto: Draw N different random numbers from the set 1...M *)
let lotto_select n m = 
  let rec aux count acc = 
    let x = Random.int m in
    if count != 0 then aux (count-1) (x::acc)
    else acc
  in
  aux n [];;
(* 25 generate a random permutation of the elements of a list*)
let permutation list = 
  let split l = 
    let rec aux acc n = function
      | [] -> raise Not_found
      | h :: t -> 
	  if n = 0 then (h, (List.rev acc) @ t)
	  else aux (h::acc) (n-1) t
    in
    let len = Random.int (List.length l) in
    aux [] len l
  in
  let rec aux2 acc2 l2 =
    match l2 with
      [] -> acc2
    | h :: t -> 
	let elem, tl = split l2 in
	aux2 (elem::acc2) tl
  in aux2 [] list;;
(* 26 generate the combinations of K distinct objects chosen from the N elements of a list*)
let extract n l =
  let rec merge elem acc1 l =
    match l with
    | [] -> List.rev acc1
    | h :: t -> merge elem ((elem::h)::acc1) t
  in
  let rec aux acc count = function
    | h :: t ->
	if count = 1 then aux ([h]::acc) 1 t
	else 
	  let l1 = aux [] (count-1) t in
	  let l2 = aux [] count t in
	  (merge h [] l1) @ l2
    | [] -> List.rev acc
  in
  aux [] n l;;
(* 27 group the elements of a set into disjoint subsets *)
(* my version *)
let remove_elem elem list =
  let rec aux acc = function
    | h::t -> 
	if elem = h then (List.rev acc)@t
	else aux (h::acc) t
    | [] -> List.rev acc
  in
  aux [] list
;;
let remove_elems elems list =
  List.fold_left (fun a x -> remove_elem x a) list elems
;;
let group list sizes = 
  let transfer n (selected, rest) = 
    let s = extract n rest in
    let r = List.map (fun x -> remove_elems x rest) s in
    let new_selected = List.map (fun x -> x::selected) s in
    List.combine new_selected r
  in
  let rec aux acc = function
    | h::t -> 
	let new_acc = List.map (fun x -> transfer h x) acc in
	aux (List.concat new_acc) t
    | [] -> acc
  in
  match sizes with
  | h::t ->
      let l = extract h list in
      let rest = List.map (fun x -> remove_elems x list) l in
      let initial_acc = List.combine (List.map (fun x -> [x]) l) rest in
      List.map (fun x -> List.rev (fst x)) (aux initial_acc t)
  | [] -> [[list]]
;;
(* answer *)
let group_standard list sizes =
  let initial = List.map (fun size -> size, []) sizes in
  let prepend p list =
    let emit l acc = l :: acc in
    let rec aux emit acc = function
      | [] -> emit [] acc
      | (n,l) as h :: t ->
	  let acc = if n > 0 then emit ((n-1, p::l) :: t) acc else acc in
	  aux (fun l acc -> emit (h :: l) acc) acc t
    in
    aux emit [] list
  in
  let rec aux = function
    | [] -> [initial]
    | h :: t -> List.concat (List.map (prepend h) (aux t))
  in
  let all = aux list in
  let complete = List.filter (List.for_all (fun (x,_) -> x = 0)) all in
  List.map (List.map snd) complete;;
(* 28 sorting a list of lists according to length of sublists *)
let length_sort l =
  let choose list = 
    match list with
      h :: t -> (h, t)
    | [] -> raise Not_found
  in
  let quicksort list = 
    let pivot, rest = choose list
    in
    let rec aux acch acct = function
	h :: t ->
	  if List.length pivot > List.length h then aux (h::acch) acct t
	  else aux acch (h::acct) t
      | [] -> acch @ (pivot::acct)
    in aux [] [] rest
  in
  quicksort l;;
let frequency_sort list =
  let first (x, _, _) = x
  in
  let second (_, x, _) = x
  in
  let statistic l = 
    let search elem l1 = 
      let rec aux elem acc = function
	  (freq, subl, len) as h :: t -> 
	    if len = List.length elem then acc@((freq+1, elem::subl, len)::t)
	    else aux elem (h::acc) t
	| [] -> (1, [elem], List.length elem)::acc
      in
      aux elem [] l1
    in
    let rec aux acc = function
	[] -> []
      | [x] -> search x acc
      |	h :: t -> aux (search h acc) t
    in
    aux [] l
  in
  let all = statistic list
  in
  List.concat (List.map second (List.sort (fun a b -> first a - first b) all))
;;
(* Arithmetic *)
(* with the help of mathematics *)
(* 31 determine whether a given integer number is prime *)
let is_prime n =
  if n = 1 then false
  else 
    let rec aux d =
      if d * d > n then true
      else (n mod d <> 0 && aux (d+1)) in
    aux 2;;
(* 32 determine the greatest common divisor of two positive integer numbers *)
let rec gcd a b =
  if b = 0 then a else gcd b (a mod b);;
(* 33 determine whether two positive integer numbers are coprime *)
let coprime a b = gcd a b = 1;;
(* 34 calculate Euler's totient function phi(m) *)
let phi n =
  let rec aux count d =
    if d < n then
      if coprime d n then aux (count+1) (d+1)
      else aux count (d+1)
    else count
  in
  aux 0 1;;
(* 35 determine the prime factors of a given positive integer *)
let factors n =
  let rec aux acc d x =
    if d <= x then
      if x mod d = 0 then aux (d::acc) 2 (x / d) 
      else aux acc (d+1) x
    else acc
  in
  aux [] 2 n
;;
(* 36' determine the prime factors of a given positive integer(2) *)
(* with the help of answer *)
let factors2 n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then
        match aux d (n / d) with
          | (h, m)::t when h = d -> (h, m+1)::t
          | l -> (d, 1)::l
      else aux (d+1) n
  in
  aux 2 n;;
(* 37 calculate Euler's totient function phi(m) (improved) *)
let power x y = 
  let rec aux acc n =
    if n = 0 then acc 
    else if n > 0 then
      aux (x * acc) (n-1)
    else
      aux (acc / x) (n+1)
  in
  aux 1 y;;
let phi_improved n =
  let l = factors2 n
  in
  let rec aux acc = function
    | (p, m) :: t ->
      aux ((p-1) * (power p (m-1)) * acc) t
    | [] -> acc
  in
  aux 1 l
;;
(* 38 compare the two methods of calculating Euler's totient function *)
let timeit f n =
  let t = Sys.time () in
  let fx = f n in
  Printf.printf "Execution time : %f\n" (Sys.time () -. t);
  fx
;;
(* 39 a list of prime numbers *)
let all_primes a b =
  let rec aux acc d =
    if d = b then List.rev acc
    else
      if is_prime d then aux (d::acc) (d+1)
      else
        aux acc (d+1)
  in
  aux [] a
;;
(* 40 Goldbach's conjecture *)
let goldbach n =
  let rec aux d = 
    if is_prime d then 
      if is_prime (n-d) then (d, (n-d))
      else aux (d+1)
    else aux (d+1)
  in
  aux 2
;;
(* 41 a list of Goldbach composition *)
let goldbach_list a b =
  let rec aux acc d =
    if 2 * d > b then List.rev acc
    else
      if 2 * d < a then aux acc (d+1)
      else aux ((2*d, goldbach (2*d))::acc) (d+1)
  in
  aux [] (a / 2)
;;
let goldbach_limit a b limit =
  let l = goldbach_list a b
  in
  let rec aux acc = function
    | h :: t -> 
      if fst (snd h) > limit then aux (h::acc) t
      else
        aux acc t
    | [] -> List.rev acc
  in
  aux [] l
;;
(* Logic and Codes *)
type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
;;
(* 46 truth tables for logical expressions(2 variables) *)
let table2 s1 s2 expr =
  let rec eval a a_v b b_v = function
    | Var(s) -> 
	if a = s then a_v
	else
	  b_v
    | Not(x) -> not (eval a a_v b b_v x)
    | And(x, y) -> eval a a_v b b_v x && eval a a_v b b_v y
    | Or(x, y) -> eval a a_v b b_v x || eval a a_v b b_v y
  in
  [(true, true, eval s1 true s2 true expr);
   (true, false, eval s1 true s2 false expr);
   (false, true, eval s1 false s2 true expr);
   (false, false, eval s1 false s2 false expr)]
;;
(* 47 truth tables for logical expressions *)
let table variables expr =
  let rec eval l = function
    | Var s ->
	List.assoc s l
    | Not s -> not(eval l s)
    | And(x, y) -> eval l x && eval l y
    | Or(x, y) -> eval l x || eval l y
  in
  let rec addl x = function
      h :: t -> (x::h)::(addl x t)
    | [] -> []
  in
  let rec transform acc = function
    | [] -> acc
    | h :: t ->    	
        transform (addl (h, true) acc) t @ 
	transform (addl (h, false) acc) t
  in
  match variables with
    h :: t ->
      let list = transform [[(h, true)]; [(h, false)]] t in
      List.map (fun x -> (x, eval x expr)) list
  | [] -> raise Not_found
;;
(* 48 gray code *)
let gray n =
  let rec aux acc d =
    if d = n then acc
    else if d = 0 then aux ["1";"0"] 1
    else
      aux (List.map (fun x -> "1" ^ x) acc) (d+1) @
      aux (List.map (fun x -> "0" ^ x) acc) (d+1)
  in
  aux [] 0
;;
(* 49 Huffman code *)
type 'a huffman_input = 
  | Fr of 'a * int
;;
type 'a huffman_code =
  | Hc of 'a * string
;;
let huffman fs =
  let l = List.map (fun x -> match x with Fr(elem, freq) -> ([(elem,"")],freq)) fs in
  let sortbyfreq l =
    List.sort (fun (m,freq1) (n,freq2) -> if freq1>freq2 then 1 else 0) l
  in
  let appendtocode x = List.map (fun (elem, code) -> (elem, x^code)) in
  let rec combine_rare l =
    match (sortbyfreq l) with
    | (m,freq1)::(n,freq2)::t ->
	combine_rare (((appendtocode "0" m)@(appendtocode "1" n),freq1+freq2)::t)
    | _ as t -> t
  in
  let result = (fst (List.hd (combine_rare l))) in
  List.map (fun (elem, code) -> Hc(elem, code)) result
;;
let fs = [Fr('a',45);Fr('b',13);Fr('c',12);Fr('d',16);Fr('e',9);Fr('f',5)];;
(* answer from christiankissig *)
let huffmann_ans l =
  (* make a list of freqs and lists of elem code pairs *)
  let l = List.map (fun (f,e) -> (f,[(e,"")])) l in
  (* fun to sort by freq, ascending *)
  let sortbyfreq =
    List.sort (fun (f,m) (g,n) -> if f>g then 1 else 0)
  in
  (* fun to append character x to code c of each pair in list of elem code pairs *)
  let appendtocode x = List.map (fun (e,c) -> (e,x^c)) in
  let rec combine_rare l =
    (* sort by freq, a 1-step bubblesort would be better here *)
    let l = sortbyfreq l in
    match l with
      (* take two least frequent elements from list *)
      (f,m)::(g,n)::t ->
	combine_rare ((f+g,(appendtocode "0" m)@(appendtocode "1" n))::t)
      (* if list contains less than two elements *)
    | _ -> l
  in
  (* return list of elem and code pairs *)
  match (List.hd (combine_rare l)) with (f,m) -> m
;;

(* Binary Trees *)
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;
(* eample tree for test *)
let example_t =
  Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)),
       Node(3, Empty, Node(6, Node(7, Empty, Empty), Empty)));;
(* 54 construct completely balanced binary tree *)
let cbal_tree num =
  let transform left right all =
    let add_right all l =
      List.fold_left (fun a r -> Node('x', l, r)::a) all right in
    List.fold_left add_right all left
  in
  let rec aux n =
    if n = 1 then [Node('x', Empty, Empty)]
    else if n = 2 then [Node('x', Node('x', Empty, Empty), Empty); 
			Node('x', Empty, Node('x', Empty, Empty))] 
    else
      let a, b = ((n-1)/2, n-1-(n-1)/2) in
      transform (aux a) (aux b) []
  in
  aux num
;;
(* 55 symmetric binary tree *)
let is_symmetric t =
  let rec is_mirror t1 t2 = 
    match t1, t2 with
    | (Node(_, l1, r1), Node(_, l2, r2)) ->
	true && (is_mirror l1 r2) && (is_mirror r1 l2)
    | (Empty, Empty) -> true
    | _ -> false
  in
  match t with
    Node(_, l, r) ->
      is_mirror l r
  | Empty -> true
;;
(* 56 binary search trees(dictionaries) *)
let construct l =
  let rec addelem elem = function
  | Node(x, left, right) -> 
      if elem > x then Node(x, addelem elem left, right)
      else Node(x, left, addelem elem right)
  | Empty -> Node(elem, Empty, Empty)
  in
  let rec aux acc = function
    | h :: t -> aux (addelem h acc) t
    | [] -> acc
  in
  aux Empty l
;;
(* 57 generate-and-test paradigm *)
let sym_cbal_trees n = 
  List.filter is_symmetric (cbal_tree n)
;;
(* 58 construct height-balanced binary tree *)
let hbal_tree n =
  let transform left right all =
    let addright all l = 
      List.fold_left (fun a x -> Node('x', l, x)::a) all right in
    List.fold_left addright all left
  in
  let rec aux h =
    if h = 0 then [Empty]
    else if h = 1 then [Node('x', Empty, Empty)]
    else
      transform (aux (h-1)) (aux (h-1)) [] @
      transform (aux (h-1)) (aux (h-2)) [] @
      transform (aux (h-2)) (aux (h-1)) []
  in aux n
;;
(* 59 construct height-balanced binary trees with a given number of nodes *)
let rec minNodes h =
  if h = 1 then 1
  else if h = 2 then 2
  else
    1 + (minNodes (h-1)) * 2 - 1
;;
let maxHeight nodes = 
  let rec aux h =
    if minNodes h >= nodes then h
    else
      aux (h+1)
  in
  aux 1
;;
let countnode tree =
  let rec aux = function
      Node(_, left, right) -> 1 + aux left + aux right
    | Empty -> 0
  in
  aux tree
;;
let rec minHeight nodes count =
  if power 2 count >= nodes + 1 then count 
  else minHeight nodes (count+1)
;;
let hbal_tree_nodes n =
  let minh = minHeight n 1 in
  let maxh = maxHeight n in
  let rec aux h =
    if h <= maxh then
      (hbal_tree h) @ aux (h+1)
    else
      []
  in List.filter (fun x -> countnode x = n) (aux minh)
;;
(* 60 count the leaves of a binary tree *)
let count_leaves t =
  let rec aux tree = 
    match tree with
    | Node(x, left, right) -> 1 + aux left + aux right
    | Empty -> 0
  in aux t
;;
(* 61 collect the leaves of a binary tree in a list *)
let leaves tree =
  let rec aux = function
    | Empty -> []
    | Node(x, Empty, Empty) -> [x]
    | Node(_, left, right) -> (aux left) @ (aux right)
  in
  aux tree
;;
(* 62 collect the internal nodes of a binary tree in a list *)
let internals tree =
  let rec aux = function
    | Empty
    | Node(_, Empty, Empty) -> []
    | Node(x, left, right) -> [x] @ (aux left) @ (aux right)
  in
  aux tree
;;
(* 63 collect the nodes at a given level in a list *)
let at_level tree n =
  let rec aux count = function
    | Empty -> []
    | Node(x, left, right) ->
	if count = 1 then [x]
	else (aux (count-1) left) @ (aux (count-1) right)
  in
  aux n tree
;;
(* 64 construct a complete binary tree *)
let is_complete_binary_tree n t =
  countnode t = n &&
  let h = minHeight n 1 in
  let rec transform addr = function
    | Node(x, left, right) ->
	Node((x, addr), transform (2*addr) left, transform (2*addr+1) right)
    | Empty -> Empty
  in
  let ntree = transform 1 t
  in
  let restaddr = n / 2 
  in
  let rec aux counth tree =
    match tree with
    | Node(_, Node(x, Empty, Empty), Empty) 
      when counth = h-1 && snd x = n ->
	true
    | Node((_, addr), Empty, Empty) 
      when (counth = h && addr > power 2 (counth-1) - 1) ||
      (counth = h-1 && addr > restaddr) ->
	true
    | Node(_, left, right) ->
	(aux (counth+1) left) && (aux (counth+1) right)
    | _ -> false
  in
  let rec aux2 counth tree =
    if counth < h then
      List.length (at_level tree counth) = power 2 (counth-1) &&
      aux2 (counth+1) tree
    else true
  in (aux 1 ntree) && (aux2 1 ntree)
;;
(* 65 layout a binary tree(1) *)
 type 'a pos_binary_tree =
   | E
   | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree;;
let r_count = function
    Node(_, _, r) -> countnode r
  | Empty -> 0
;;
let l_count = function
    Node(_, l, _) -> countnode l
  | Empty -> 0
;;
let layout_binary_tree t =
   let rec aux x y = function
     | Node(w, l, r) -> 
	 N(w,x,y,
	   aux (x - 1 - r_count l) (y+1) l,
	   aux (x + 1 + l_count l) (y+1) r)
     | Empty -> E
   in
   match t with
     Node(_, left, _) ->
       aux (countnode left + 1) 1 t
   | Empty -> raise Not_found
;;
(* 66 layout a binary tree(2) *)
let example_t2 = 
  Node('n', Node('k', Node('c', 
			   Node('a', Empty, Empty), 
			   Node('e', Node('d', Empty, Empty),
				Node('g', Empty, Empty))), 
		 Node('m', Empty, Empty)), Node('u', 
						Node('p', Empty,
						     Node('q', Empty, Empty)),
						Empty));;
let rec depth = function
  | Node(_, left, right) -> 1 + max (depth left) (depth right)
  | Empty -> 0
;;
let layout_binary_tree2 t =
  let d = depth t
  in
  let rec initial_x count = function
    | Node(_,Empty,_) -> 1
    | Node(_,l,_) -> power 2 (count-1) + initial_x (count-1) l
    | Empty -> 0
  in
  let rec aux x y = function
    | Node(w,l,r) -> 
	N(w,x,y,
	  aux (x - power 2 (d-y-1)) (y+1) l,
	  aux (x + power 2 (d-y-1)) (y+1) r)
    | Empty -> E
  in
  aux (initial_x (d-1) t) 1 t
;;
(* 67 layout a binary tree(3) *)
(* a list of x separation distance between each layer *)
(* move the list to the left or right *)
let leftedgemove dis l = 
  let rec aux acc = function
    | (level, left, right)::t ->
	aux ((level, left-dis, right-dis)::acc) t
    | [] -> List.rev acc
  in
  aux [] l
;;
let rightedgemove dis l =
  let rec aux acc = function
    | (level, left, right)::t ->
	aux ((level, left+dis, right+dis)::acc) t
    | [] -> List.rev acc
  in
  aux [] l
;;
(* (level, left edge, right edge ) list *)
let third (_, _, x) = x;;
let middle (_, x, _) = x;;
let first (x, _, _) = x;;
let evalhd = function
  | [] -> (0, 0, 0)
  | h::_ -> h
;;
let combine l1 l2 =
  let rec aux la lb = 
    match la, lb with
    | ((levela, aleft, _)::ta, (levelb, _, bright)::tb) ->
	(max levela levelb, aleft, bright)::(aux ta tb)
    | (l, [])
    | ([], l) -> l
  in
  let rec max_overlap la lb = 
    match la, lb with
    | ((_, _, aright)::ta, (_, bleft, _)::tb) ->
	max (max_overlap ta tb) (aright - bleft)
    | _ -> 0
  in
  let lev = max (first (evalhd l1)) (first (evalhd l2)) in
  let maxdis = 1 + max_overlap l1 l2 in
  let each_x = (maxdis + 1) / 2 in
  (each_x, (lev+1, 0, 0)::(aux (leftedgemove each_x l1) (rightedgemove each_x l2)))
;;
let rec build_tree = function
  | Empty -> (E, [])
  | Node(w, l, r) ->
      match build_tree l, build_tree r with
      | ((N(_, _, _, _, _) as left, list1), 
	 (N(_, _, _, _, _) as right, list2)) ->
	   let xdis, list = combine list1 list2 in
	   (N(w, xdis, first (evalhd list), left, right), list)
      | ((N(_, _, _, _, _) as left, list1), (E, [])) ->
	  let xdis, list = combine list1 [] in
	  (N(w, 1, first (evalhd list), left, E), list)
      | ((E, []), (N(_, _, _, _, _) as right, list2)) ->
	  let xdis, list = combine [] list2 in
	  (N(w, 1, first (evalhd list), E, right), list)
      | ((E, []), (E, [])) ->
	  (N(w, 0, 1, E, E), [(1, 0, 0)])
      | _ -> failwith "Impossible"
;;
let filter_tree t =
  let rec aux x y tree =
    match tree with
    | N(w, xdis, _, l, r) ->
	N(w, x, y, aux (x-xdis) (y+1) l, aux (x+xdis) (y+1) r)
    | E -> E
  in
  aux 0 1 t
;; 
let rec move_tree init_x t =
  match t with
  | N(w, x, y, l, r) -> 
      N(w, x+init_x, y, move_tree init_x l, move_tree init_x r)
  | E -> E
;;
let layout_binary_tree3 t =
  let expandtree = filter_tree (fst (build_tree t)) in
  let initial_x =
    let rec aux tree = 
      match tree with
      | N(_, x, _, l, r) -> min x (min (aux l) (aux r))
      | E -> 0
    in
    1 - aux expandtree
  in
  move_tree initial_x expandtree
;;
(* 68 a string representation of binary tree *)
type 'a stringlist = 
  | C of char
  | Comma
  | LP
  | RP
;;
let string_to_list s =
  let len = String.length s in
  let rec aux acc i =
    if i = len then acc
    else if s.[i] = ',' then aux (Comma::acc) (i+1)
    else if s.[i] = '(' then aux (LP::acc) (i+1)
    else if s.[i] = ')' then aux (RP::acc) (i+1)
    else aux (C(s.[i])::acc) (i+1)
  in
  List.rev (aux [] 0)
;;
let list_to_string l =
  let rec aux acc = function
    | C(a)::t -> aux ((Char.escaped a)^acc) t
    | Comma::t -> aux (","^acc) t
    | LP::t -> aux ("("^acc) t
    | RP::t -> aux (")"^acc) t
    | [] -> acc
  in
  aux "" (List.rev l)
;;
let tree_to_string t =
  let rec aux = function
    | Node(w, l, r) ->
	(Char.escaped w)^"("^(aux l)^","^(aux r)^")"
    | Empty -> ""
  in
  let str = aux t in
  let len = String.length str in
  let rec remove_empty s i acc =
    if i = len then acc	
    else if s.[i] = '(' && s.[i+1] = ',' && s.[i+2] = ')' then
      remove_empty s (i+3) acc
    else
      remove_empty s (i+1) acc^(Char.escaped s.[i])
  in 
  list_to_string (List.rev (string_to_list (remove_empty str 0 "")))
;;
let string_to_tree str =
  let l = string_to_list str in
  let rec aux mark countp s =
    if mark = 0 then
      match s with
        | C(a)::(LP::t) -> 
          Node(a, aux 0 0 t, aux 1 0 t)
        | C(a)::(Comma::t) -> Node(a, Empty, Empty)
        | Comma::t -> Empty
        | C(a)::(RP::t) -> Node(a, Empty, Empty)
        | RP::t -> Empty
    else
      match s with
        | LP::t -> aux 1 (countp+1) t
        | RP::t -> aux 1 (countp-1) t
        | Comma::t when countp = 0 -> aux 0 0 t
        | h::t -> aux 1 countp t
        | [] -> failwith "error"
  in
  aux 0 0 l
;;
(* 69 preorder and inorder sequences of binary trees *)
let preorder_b b =
  let rec aux = function
    | Node(a, left, right) ->
      (Char.escaped a)^(aux left)^(aux right)
    | Empty -> ""
  in
  aux b
;;
let inorder_b b =
  let rec aux = function
    | Node(a, left, right) ->
      (aux left)^(Char.escaped a)^(aux right)
    | Empty -> ""
  in
  aux b
;;
(* need to know the structure information of the tree, like the one in problem 69 *)
let split_b elem l =
  let rec aux acc = function
    | h::t when h = elem ->
      (List.rev acc, t)
    | h::t -> aux (h::acc) t
    | [] -> raise Not_found
  in
  aux [] l
;;
let tranform elem = 
  match elem with
    | C(a) -> a
    | _ -> failwith "Impossible"
;;
let pre_in_tree s_pre s_in =
  let l_pre = string_to_list s_pre in
  let l_in = string_to_list s_in in
  let rec remove_left len target = 
    match target with
      | h::t when len = 0 -> h::t
      | h::t -> remove_left (len-1) t
      | [] when len = 0 -> []
      | _ -> failwith "impossible"
  in
  let rec aux l_p l_i =
    match l_i with
      | [a] -> Node(tranform a, Empty, Empty)
      | [] -> Empty
      | _ ->
        let first_l, rest_l = (List.hd l_p, List.tl l_p) in
        let temp = split_b first_l l_i in
        Node(tranform first_l, aux rest_l (fst temp),
             aux (remove_left (List.length (fst temp)) rest_l) (snd temp))
  in
  aux l_pre l_in
;;
(* what is difference list *)
(* The content of the node have the same number of characters *)
(* 70 Dotstring representation of binary trees *)
type 'a string_to_list2 =
  | Dot
  | Ch of char
;;
let string_to_list2 s =
  let len = String.length s in
  let rec aux acc i =
    if i = len then List.rev acc
    else if s.[i] = '.' then aux (Dot::acc) (i+1)
    else aux (Ch(s.[i])::acc) (i+1)
  in
  aux [] 0
;;
let list_to_string2 l =
  let rec aux acc = function
    | Dot::t -> aux (acc^".") t
    | Ch(a)::t -> aux (acc^(Char.escaped a)) t
    | [] -> acc
  in
  aux "" l
;;
let tree_dotstring t =
  let rec aux = function
    | Node(a, l, r) -> (Char.escaped a)^(aux l)^(aux r)
    | Empty -> "."
  in 
  aux t
;;

let tree_dotstring_rev s =
  let l = string_to_list2 s in
  let rec aux mark level lr l = 
    if mark = 0 then
      match l with
        | Ch(a)::t -> Node(a, aux 0 level 0 t, aux 2 level 0 t)
        | Dot::_ -> Empty
    else
      match l with
        (* initialize *)
        | Ch(a)::t when lr = 0 && mark = 2 && level = 0 -> aux 1 1 0 t
	| Dot::t when lr = 0 && mark = 2 -> aux 0 0 0 t
        (* left child extension case *)
        | Ch(a)::t when lr = 0 && (not(level = 0)) -> aux 1 (level+1) 0 t
	| Dot::t when lr = 0 && (not(level = 0)) -> aux 1 level 1 t
        (* right child extension case *)
	| Ch(a)::t when lr = 1 && (not(level = 0)) -> aux 1 (level+1-1) 0 t
	| Dot::t when lr = 1 && (not(level = 0)) -> aux 1 (level-1) 1 t
        (* terminated *)
	| _ as t when level = 0 -> aux 0 0 0 t
  in
  aux 0 0 0 l
;;
(* Multiway Trees *)
 type 'a mult_tree = T of 'a * 'a mult_tree list;;
(* example for test *)
let test = 
  T('a', [T('f', [T('g', [])]); T('c', []); T('b', [T('d',[]); T('e',[])])]);;
(* 71 count the nodes of a multiway tree *)
let count_nodes t =
  let rec aux = function
    | T(_, l)::t -> 1 + aux l + aux t
    | [] -> 0
  in 
  match t with
  | T(_, l) -> 1 + aux l
;;
(* 72 tree construction from a node string *)
let string_of_tree t =
  let rec aux acc = function
    | T(x, l)::t ->
	acc ^ (Char.escaped x) ^ aux "" l ^ "^" ^ aux "" t
    | [] -> acc
  in
  match t with
  | T(x, l) -> aux (Char.escaped x) l
;;
(* with the help of the answer *)
let tree_of_string s = 
  let rec aux str i sublist len =
    if i >= len || str.[i] = '^' then List.rev sublist, i + 1
    else
      let l, j = aux str (i+1) [] len in
      aux str j (T(str.[i], l)::sublist) len
  in
  fst (aux s 0 [] (String.length s))
;;
(* 73 determine the internal path length of a tree *)
let ipl t =
  let rec len tree =
    let rec aux = function
      | T(x, l)::t -> 1 + List.length l + aux l + aux t
      | [] -> 0
    in
    match tree with
      T(x, l) ->
	aux l
  in
  len t
;;
(* 74 construct the bottom-up order sequence of the tree nodes *)
let bottom_up t =
  let rec aux = function
    | T(x, l)::t -> ((aux l)@[x])@(aux t)
    | [] -> []
  in
  match t with
  | T(x, l) -> (aux l)@[x]
;;
(* 75 lisp-like tree representation *)
let lispy t =
  let rec aux = function
    | T(x, [])::t -> 
	if t = [] then (Char.escaped x)
	else
	  (Char.escaped x)^" "^(aux t)
    | T(x, l)::t -> 
	if t = [] then "("^(Char.escaped x)^" "^(aux l)^")"
	else
	  "("^(Char.escaped x)^" "^(aux l)^") "^(aux t)
    | [] -> raise Not_found
  in
  match t with
    T(x, l) ->
      "("^(Char.escaped x)^" "^(aux l)^")"
;;
(* Graph *)
type 'a graph_term = { nodes : 'a list; edges : ('a * 'a) list };;
(* example for test *)
let edge_clause = 
  ['h', 'g'; 'k', 'f'; 'f', 'b'; 'f', 'c'; 'c', 'b'];;
let graphterm =
  { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
    edges = ['h', 'g'; 'k', 'f'; 'f', 'b'; 'f', 'c'; 'c', 'b'] };;
let adjacent = 
  ['b', ['c';'f']; 'c', ['b';'f']; 'd', []; 'f', ['b';'c';'k']; 'g', ['h'];
   'h', ['g']; 'k', ['f']];;
let human_fri = "b-c f-c g-h d f-b k-f h-g";;
(* 80 conversions *)
let two_one g = g.edges;;

let rec transformgI acc = function
  | (m, n)::t ->
      transformgI (m::(n::acc)) t
  | [] -> acc
;;
let one_two g = 
  let rec aux acc = function
    | h::t -> 
	if List.mem h acc then 
	  aux acc t
	else
	  aux (h::acc) t
    | [] -> acc
  in
  let l = transformgI [] g
  in
  { nodes = aux [] l;
    edges = g }
;;

let filterg l = 
  let rec aux acc = function
    | ((m, n) as h)::t -> 
	if List.mem (n, m) acc || List.mem h acc then 
	  aux acc t
	else
	  aux (h::acc) t
    | [] -> acc
  in
  aux [] l
;;
let transformgII (m, l) =
  let rec aux acc = function
    | h::t -> aux ((m, h)::acc) t
    | [] -> acc
  in
  aux [] l
;;
let three_two g =
  let rec aux acc = function
    | ((m, l) as h)::t -> 
	aux ((transformgII h)@acc) t
    | [] -> acc
  in
  { nodes = List.map fst g;
    edges = filterg (aux [] g)}
;;

let findgraphterm elem list = 
  let rec aux acc = function
    | (m, n)::t ->
	if elem = m then 
	  aux (n::acc) t
	else if elem = n then
	  aux (m::acc) t
	else
	  aux acc t
    | [] -> acc
  in
  aux [] list
;;
let two_three g =
  let rec aux acc = function
    | h::t -> 
	aux ((h, (findgraphterm h g.edges))::acc) t
    | [] -> acc
  in
  List.rev (aux [] g.nodes)
;;
    
let transformgIII s =
  let rec aux acc temp tempII i =
    if i < String.length s then
      if s.[i] = '-' then aux acc "" (temp::tempII) (i+1)
      else if s.[i] = ' ' && not(tempII = []) then 
	aux ((temp::tempII)::acc) "" [] (i+1)
      else if s.[i] = ' ' && tempII = [] then
	aux ([temp]::acc) "" [] (i+1)
      else aux acc ((Char.escaped s.[i])^temp) tempII (i+1)
    else
      acc
  in
  let rec aux2 acc elems = function
    | [] -> (elems, acc)
    | h::t -> 
	match h with
	| m::[n] -> aux2 ((m, n)::acc) (m::(n::elems)) t
	| [x] -> aux2 acc (x::elems) t	
	| _ -> failwith "Impossible"
  in
  aux2 [] [] (aux [] "" [] 0)
;;
let four_two g =
  let rec aux acc = function
    | h::t -> 
	if List.mem h acc then 
	  aux acc t
	else
	  aux (h::acc) t
    | [] -> acc
  in
  let x, y = transformgIII g
  in
  { nodes = aux [] x;
    edges = y}
;;

let trans_tuple_list l =
  let rec aux acc = function
    | (m, n)::t -> aux (m::(n::acc)) t
    | [] -> acc
  in
  aux [] l
;;
let select_isolated lpart lall =
  let rec aux acc = function
    | h::t -> 
	if List.mem h lpart then aux acc t
	else
	  aux (h::acc) t
    | [] -> acc
  in
  aux [] lall
;;
let two_four g =
  let rec aux acc = function
    | (m, n)::t -> 
	aux ((Char.escaped m)^"-"^(Char.escaped n)^" "^acc^" ") t
    | [] -> acc
  in
  let isolated = select_isolated (trans_tuple_list g.edges) g.nodes
  in
  let rec aux2 acc = function
    | h::t -> aux2 ((Char.escaped h)^" "^acc^" ") t
    | [] -> acc
  in 
  let result = aux2 (aux "" g.edges) isolated
  in
  String.trim result  
;;
(* 81 path from one node to another one *)
(* input is the third form, adjacent *)
let paths g a b =
  let b_path onepath = 
    let edge = List.hd onepath in
    let neighbors = List.assoc edge g in
    List.fold_left 
      (fun all newedge -> 
	if List.mem newedge onepath then 
	  all 
	else 
	  (newedge::onepath)::all) [] neighbors
  in
  let rec list_p paths = 
    List.concat (List.map 
		   (fun x -> match b_path x with
		   | [] -> [x]
		   | _ -> x::(list_p (b_path x)))
		   paths)
  in
  let result = 
    List.filter (fun x -> if List.hd x = b then true else false) (list_p [[a]])
  in
  List.map List.rev result
;;
(* answer *)
let neighbors g a cond =
  let edge l (b,c) = if b = a && cond c then c::l
                     else if c = a && cond b then b::l
		     else l in
  List.fold_left edge [] g.edges
let rec list_path g a to_b = match to_b with
  | [] -> assert false
  | a' :: _ ->
      if a' = a then [to_b]
      else
	let n = neighbors g a' (fun c -> not(List.mem c to_b)) in
	List.concat(List.map (fun c -> list_path g a (c::to_b)) n)
let paths_answer g a b =
  assert(a <> b);
  list_path g a [b];;
(* 82 cycle from a given node *)
let cycles g a = 
  let expand_path onepath =
    let edge = List.hd onepath in
    let neighbors = List.assoc edge g in
    List.fold_left 
      (fun all newedge -> 
	if List.mem newedge onepath && not(newedge = a) then
	  all
	else
	  (newedge::onepath)::all) [] neighbors
  in
  let rec list_path paths =
    List.concat (List.map 
		   (fun x -> if List.hd x = a then
		     [x]
		   else
		     list_path (expand_path x))
		   paths)
  in
  list_path (expand_path [a])
;;
(* 83 construct all spanning trees *)
let g = { nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
	  edges = [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e');
		   ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
		   ('e', 'h'); ('f', 'g'); ('g', 'h')] };;
let is_connected graph =
  let rec aux = function
    | [x] -> true
    | h::t -> (not((paths_answer graph h (List.hd t)) = [])) && aux t
    | [] -> failwith "Empty graph"
  in
  aux graph.nodes
;;
let is_tree graph = 
  let g3 = two_three graph in
  let nodecycle node = 
    List.filter (fun x -> List.length x > 3) (cycles g3 node)
  in
  let l = List.concat (List.map nodecycle graph.nodes)
  in
  (l = []) && (is_connected graph)
;;
let s_tree g =
  let tree_list = List.map 
      (fun x -> { nodes = g.nodes; edges = x }) 
      (extract ((List.length g.nodes) - 1) g.edges)
  in
  List.filter is_tree tree_list
;;
(* no use till now *)
let identical_list l1 l2 =
  (List.for_all (fun x -> List.mem x l1) l2) &&
  (List.for_all (fun x -> List.mem x l2) l1) &&
  (List.length l1 = List.length l2)
;;
(* 84 construct the minimal spanning tree *)
type ('a, 'b) labeled_graph = { nodesII : 'a list; 
				edgesII : ('a * 'a * 'b) list };;
let g_w = { nodesII = ['a';'b';'c';'d';'e';'f';'g';'h'];
	    edgesII = [('a', 'b', 5); ('a', 'd', 3); ('b', 'c', 2); 
		       ('b', 'e', 4); ('c', 'e', 6); ('d', 'e', 7); 
		       ('d', 'f', 4); ('d', 'g', 3); ('e', 'h', 5); 
		       ('f', 'g', 4); ('g', 'h', 1)]};;
(* with the help of book <Introduction to OCaml> *)
type 'a parent =
    Root of int
  | Parent of 'a vertex
and 'a vertex = 'a * 'a parent ref
;;
let rec compress root (_, p) =
  match !p with
    Root _ -> ()
  | Parent v -> p := Parent root; compress root v
let rec simple_find ((_, p) as v) =
  match !p with
    Root _ -> v
  | Parent v -> simple_find v
let find v = 
  let root = simple_find v in
  compress root v;
  root
;;
let union ((_, p1) as u1) ((_, p2) as u2) =
  match !p1, !p2 with
    Root size1, Root size2 when size1 > size2 ->
      p2 := Parent u1;
      p1 := Root (size1 + size2)
  | Root size1, Root size2 ->
      p1 := Parent u2;
      p2 := Root (size1 + size2)
  | _ ->
      raise (Invalid_argument "union: not roots")
;;
let kruskal edges initial_nodes = 
  let spanning_tree = ref [] in
  List.iter (fun ((v1, v2, _) as edge) ->
    let u1 = find (v1, List.assoc v1 initial_nodes) in
    let u2 = find (v2, List.assoc v2 initial_nodes) in
    if u1 != u2 then begin 
      spanning_tree := edge :: !spanning_tree;
      union u1 u2
    end) edges;
  !spanning_tree
;;
let min_spanning_tree g =
  let initial_nodes = List.map (fun x -> (x, ref (Root(1)))) g.nodesII in
  let edges = List.sort (fun x y -> third x - third y) g.edgesII in
  kruskal edges initial_nodes
;;
(* 85 graph isomorphism *)
let split_elem l i = 
  let elem n l = 
    match at n l with 
    | Some(x) -> x
    | None -> failwith "Impossible"
  in
  let rest = remove_at (i-1) l in
  (elem i l, rest)
;;
let perm_seperate li =
  let rec aux acc i l = 
    if i > List.length l then 
      acc
    else
      let temp = split_elem l i in
      aux (temp::acc) (i+1) l
  in    
  match li with
  | [] -> failwith "Impossible"
  | [x] -> [(x, [])]
  | _::_ as templist ->
      aux [] 1 templist
;;
let perm_seperate2 (acc, li) =
  if li = [] then
    [acc, li]
  else
    let templist = perm_seperate li in
    List.map (fun (x, y) -> (x::acc, y)) templist
;;
let rec perm_aux (li : ('a list * 'a list) list) =
  let l = List.concat (List.map (fun x -> perm_seperate2 x) li)
  in
  if snd (List.hd l) = [] then
    l
  else
    perm_aux l
;;
(* list can't be > 1 *)
let permutation_all list = 
  let initial_l = List.map (fun (x, y) -> ([x], y)) (perm_seperate list) in
  List.map (fun (x, _) -> x) (perm_aux initial_l)
;;

let g2 = { nodes = ['m'; 'b'; 'c'; 's'; 'e'; 'x'; 'g'; 'h'];
	   edges = [('m', 'b'); ('m', 's'); ('b', 'c'); ('b', 'e');
		   ('c', 'e'); ('s', 'e'); ('s', 'x'); ('s', 'g');
		   ('e', 'h'); ('x', 'g'); ('g', 'h')] };;
let graph_isomor g1 g2 =
  let rec aux bindl edgeI edgeII = 
    match edgeI with
      (x, y)::t -> 
	let (x', y') = (List.assoc x bindl, List.assoc y bindl) in
	((List.mem (x', y') edgeII) || (List.mem (y', x') edgeII)) && 
	aux bindl t edgeII
      | [] -> true
  in
  if List.length g1.nodes = List.length g2.nodes then
    let bind = List.map (List.combine g1.nodes) (permutation_all g2.nodes) in
    List.exists (fun x -> aux x g1.edges g2.edges) bind
  else
    failwith "impossible num"
(* 86 node degree and graph coloration *)
let degree graph node =
  List.fold_left (fun num edge -> 
    if fst edge = node || snd edge = node then
      num+1
    else
      num
  ) 0 graph.edges
;;
let sort_graph_degree g =
  let l = List.map (fun x -> (x, degree g x)) g.nodes in
  let l2 = List.sort (fun x y -> snd y - snd x) l in
  List.map fst l2
;;
let check_color node1 node2 =
  snd node1 = snd node2
;;
let welsh_powell g =
  let nodes = sort_graph_degree g in
  let rec add_color color node l_nodes =
    let n_l = neighbors g node (fun x -> List.mem x (List.map fst l_nodes)) in
    let node_color = (node, color) in
    let node_l_color = List.filter (fun x -> List.mem (fst x) n_l) l_nodes in
    if List.for_all (fun x -> not(check_color x node_color)) node_l_color then
      node_color::l_nodes
    else
      add_color (color+1) node l_nodes
  in
  List.fold_left (fun l x -> add_color 1 x l) [] nodes

;;
(* 87 depth-first order graph traversal *)
let neighbor_edge node g =
  let f n edge = (fst edge = n) || (snd edge = n) in
  List.filter (fun x -> f node x) g.edges
;;
let other_end edge node =
  if fst edge = node then
    snd edge
  else if snd edge = node then
    fst edge
  else
    failwith "impossible edge"
;;
let tree_graph = 
  { nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'];
    edges = ['a','b'; 'b','c'; 'a','d'; 'a','e'; 'd','f'; 'd','g']
  }
;;
let depth_first_search g =
  let start_p = List.hd g.nodes in
  let v_nodes = ref [start_p] in
  let rec aux node =
    let edges = neighbor_edge node g in
    let rec aux_edge = function
      | h::t ->
        let p = other_end h node in
        if not(List.mem p !v_nodes) then begin
          v_nodes := p :: !v_nodes;
          aux p
        end;
        aux_edge t
      | [] -> ()
    in
    aux_edge edges
  in
  aux start_p;
  List.rev !v_nodes
;;
(* 88 connected components *)
let remove_elem_l elem l =
  let rec aux acc = function
    | h::t when h = elem -> aux acc t
    | h::t -> aux (h::acc) t
    | [] -> acc
  in
  List.rev (aux [] l)
;;
let connected_comp g =
  let initial_comp = (List.map (fun x -> [x]) g.nodes) in
  let rec aux all edges =
    match edges with
      | (m, n)::t ->
        let a = List.find (fun x -> List.mem m x) all in
        let b = List.find (fun x -> List.mem n x) all in
        if not(a = b) then
          let all' = (a@b)::(remove_elem_l b (remove_elem_l a all)) in
          aux all' t
        else
          aux all t
      | [] -> 
        all
  in
  aux initial_comp g.edges
;;
(* 89 bipartite graphs *)
let g_simple = 
  { nodes = ['a';'b';'c';'d'];
    edges = ['a','b';'b','c';'a','d']
  }
;;
let bipartite g =
  let u = ref [List.hd g.nodes] in
  let v = ref [] in
  let find_uv elem =
    if List.mem elem !u then u
    else if List.mem elem !v then v
    else failwith "impossible"
  in
  let cond_b node nodes_o =
    let set = find_uv node in
    List.exists (fun x -> List.mem x !set) nodes_o
  in
  let rec aux visited = function
    | h::t ->
      let nodes_o = neighbors g h (fun x -> true) in
      if cond_b h nodes_o then false
      else
        let nodes = List.filter (fun x -> not(List.mem x visited)) nodes_o 
        in        
        let rest = List.fold_left (fun l x -> remove_elem_l x l) t nodes in
        if List.mem h !u then begin
          v := nodes@(!v);
          aux (h::visited) (nodes@rest)
        end
        else
          begin
          u := nodes@(!u);
          aux (h::visited) (nodes@rest)
          end
    | [] -> true
  in
  if is_connected g then begin
    let x = aux [] g.nodes in
    (x, (!u, !v))
  end
  else
    (false, ([], []))
;;
(* 90 generate K-regular simple graphs with N nodes *)

(* Miscellaneous problems *)
(* 91 eight queens problem *)
let count_nth elem list =
  let rec aux count = function
    | h::t -> 
	if h = elem then count
	else
	  aux (count+1) t
    | [] -> raise Not_found
  in
  aux 0 list
;;
let eight_queen n =
  let cond y l =
    List.exists (fun m -> (abs (m-y) = 1 + count_nth m l)) l
  in
  let rec fill all acc i = 
    if acc = [] && i > n then 
      all
    else if List.length acc = n then
      fill (acc::all) (List.tl acc) (List.hd acc + 1)
    else if i > n then
      fill all (List.tl acc) (List.hd acc + 1)
    else if List.mem i acc || cond i acc then
      fill all acc (i+1)
    else
      fill all (i::acc) 1
  in
  fill [] [1] 1
;;
(* 92 knight's tour *)
let jump n (x,y) =
  let jumpstep = 
    [(1,2);(2,1);(-1,2);(-2,1);(-1,-2);(-2,-1);(1,-2);(2,-1)]
  in
  let l = List.map (fun (dx, dy) -> (x+dx, y+dy)) jumpstep in
  List.filter (fun (x,y) -> (x <= n && y <= n && x > 0 && y > 0)) l
;;
let knight_tour n = 
  let rec aux all acc =
    let l = jump n (List.hd acc) in
    let acclist = List.filter (fun x -> not(List.mem x acc)) l in
    if acclist = [] then
      []
    else if List.length acc = n * n - 1 then
      List.map (fun x -> x::acc) acclist
    else
      List.concat (List.map (fun x -> aux all (x::acc)) acclist)
  in
  aux [] [(1,1)]
;;
(* 93 Von Koch's conjecture *)
let example_graphI = 
  { nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'];
    edges = ['a', 'd'; 'a', 'g'; 'a', 'b'; 'b', 'e'; 'e', 'f'; 'b', 'c'] }
;;
(* tuple (elem, rest) list *)
let int_range n = 
  let rec aux acc i =
    if i > n then
      List.rev acc
    else
      aux (i::acc) (i+1)
  in
  aux [] 1
;;
type ('a, 'b) labeled_graphII = { nodesIII : ('a * 'b) list; 
				  edgesIII : (('a * 'a) * 'b) list };;
let von_koch g =
  let num = List.length g.nodes in
  let bind nodeslist lnum =
    List.map (fun x -> List.combine nodeslist x) (permutation_all lnum)
  in
  let bind2 edgeslist lnum =
    List.map (fun x -> List.combine edgeslist x) (permutation_all lnum)
  in
  (* Von Koch predicate *)
  let cond_von_koch (((x, y), w) : (char * char) * int) 
      (l : (char * int) list) =
    let m = List.assoc x l in
    let n = List.assoc y l in
    abs (m-n) = w
  in
  let aux g' =
    let t_nodes = g'.nodesIII in
    let t_edges = g'.edgesIII in
    List.for_all (fun x -> cond_von_koch x t_nodes) t_edges
  in
  (* construct all possibilities *)
  let construct nodesl edgesl = 
    List.concat (List.map (fun x -> List.map 
	(fun y -> { nodesIII = x; edgesIII = y}) edgesl) nodesl)
  in
  let initial_all = 
    construct (bind g.nodes (int_range num)) (bind2 g.edges (int_range (num-1)))
  in
  List.filter aux initial_all
;; 
(* 94 an arithmatic puzzle *)
(* need extract, remove_elems *)
let arith_two x y =
  [(x+y, [('|', x);('+', y)]);
   (x-y, [('|', x);('-', y)]);
   (x*y, [('|', x);('*', y)]);
   (x/y, [('|', x);('/', y)])]
;;
let rec arith_calc (l, history) =
  match l with
  | h::(t::[]) -> 
      let list = arith_two h t in
      List.map (fun (v, his) -> ([v], (List.rev his)@history)) list
  | h::(t::r) ->
      let list = arith_two h t in
      let templ = List.map (fun (v, his) -> (v::r, (List.rev his)@history)) list in
      List.concat (List.map arith_calc templ)
  | _ -> failwith "Impossible"
;;
let arith_puzzle lnum =
  let l = permutation_all lnum in
  let result_all = List.concat (List.map (fun x -> arith_calc (x, [])) l) in
  let result = List.filter (fun ([x], l) -> x = 0) result_all in
  let pres_l = List.map snd result in
  List.map (fun x -> List.rev x) pres_l
;;
(* 95 English number words *)
let hash_words =
  [(1, "one"); (2, "two"); (3, "three"); (4, "four"); (5, "five");
   (6, "six"); (7, "seven"); (8, "eight"); (9, "nine"); (0, "zero")]
;;
let full_words n = 
  let rec aux acc num = 
    let i = num mod 10 in
    let word = List.assoc i hash_words in
    if num < 10 then 
      word^"-"^acc
    else
      aux (word^"-"^acc) (num / 10)
  in
  let s = aux "" n in
  String.sub s 0 ((String.length s) - 1)
;;
(* 96 syntax checker *)
let string_to_list3 s =
  let len = String.length s in
  let rec aux acc i =
    if i >= len then
      List.rev acc
    else
      aux (s.[i]::acc) (i+1)
  in
  aux [] 0
;;
let identifier s list =
  let l = string_to_list3 s in
  let l_all = List.map (fun x -> string_to_list3 x) list in
  let rec aux all = function
    | _ when all = [] -> false
    | h::t -> 
	let rest = List.filter (fun x -> List.hd x = h) all in
	aux (List.map List.tl rest) t
    | [] -> true
  in
  aux l_all l
;;
(* 97 Sudoku *)
let sample_broad =
  [   [ 0; 0; 4; 8; 0; 0; 0; 1; 7];
      [ 6; 7; 0; 9; 0; 0; 0; 0; 0];
      [ 5; 0; 8; 0; 3; 0; 0; 0; 4];
      [ 3; 0; 0; 7; 4; 0; 1; 0; 0];
      [ 0; 6; 9; 0; 0; 0; 7; 8; 0];
      [ 0; 0; 1; 0; 6; 9; 0; 0; 5];
      [ 1; 0; 0; 0; 8; 0; 3; 0; 6];
      [ 0; 0; 0; 0; 0; 6; 0; 9; 1];
      [ 2; 4; 0; 0; 0; 1; 5; 0; 0]    ];;
let construct_list n elem =
  let rec aux acc i elem = 
    if i > n then
      acc
    else
      aux (elem::acc) (i+1) elem
  in
  aux [] 1 elem
;;
let sample n =
  let line = construct_list n 0 in
  construct_list n line
;;
let transpose_m m =
  let rec aux i acc matrix =
    match matrix with
    | h::t -> aux i ((at i h)::acc) t
    | [] -> List.map (fun (Some x) -> x) (List.rev acc)
  in
  let result = ref [] in
  for v = 1 to List.length (List.hd m) do
    result := (aux v [] m)::(!result)
  done;
  List.rev !result
;;
let get_p_matrix m (x,y) =
  List.nth (List.nth m (y-1)) (x-1)
;;
let cond_sqr_out matrix (x',y') =
  let sqrx = ((x'-1) / 3) * 3 in
  let sqry = ((y'-1) / 3) * 3 in
  let l =
    List.fold_left (fun a x -> a@(slice x sqrx (sqrx+4))) [] (slice matrix sqry (sqry+4))
  in
  let rec aux acc i =
    if i > 9 then List.rev acc
    else if List.mem i l then aux acc (i+1)
    else aux (i::acc) (i+1)
  in
  aux [] 1
;;
let check_full m =
  List.for_all (fun x -> not(List.mem 0 x)) m
;;
let cond_line_col matrix (x',y') v =
  not(List.mem v (List.nth matrix (y'-1))) 
  && (List.for_all (fun x -> not(v = List.nth x (x'-1))) matrix)
;;
let su_p_value matrix (x,y) =
  let l = cond_sqr_out matrix (x,y) in
  List.filter (fun v -> cond_line_col matrix (x,y) v) l
;;
let change_p_matrix m (x,y) value =
  let new_l old = 
    let rec aux acc i = function
      | h::t when i = 1 -> (List.rev (value::acc))@t
      | h::t -> aux (h::acc) (i-1) t
      | [] -> failwith "out_of_range"
    in
    aux [] x old
  in
  let rec aux acc i = function
    | h::t when i = 1 -> (List.rev ((new_l h)::acc))@t
    | h::t -> aux (h::acc) (i-1) t
    | [] -> failwith "out_of_range"
  in
  aux [] y m
;;
let sudoku matrix =
  let mark = ref 0 in
  let rec aux m n =
    if n <= 81 then 
      change m n
    else if not(check_full m) && (!mark = 1) then begin
      mark := 0;
      aux m 1
    end
    else if not(check_full m) && (!mark = 0) then
      failwith "need more complex solutions"
    else
      m
  and
  change m num =
    let i = (num mod 9) + 1 in
    let j = ((num-1) / 9)+1 in
    let v = get_p_matrix m (i,j) in
    if v = 0 then
      match (su_p_value m (i,j)) with
        | [x] -> 
          mark := 1;
          let m' = change_p_matrix m (i,j) x in
          aux m' (num+1)
        | _ -> aux m (num+1)
    else
      aux m (num+1)
  in
  aux matrix 1
(* 98 Nonograms *)
let get_line m =
  let rec aux num all = function
    | 1::(1::_ as t) -> aux (num+1) all t
    | 1::t -> aux 0 ((num+1)::all) t
    | 0::t -> aux 0 all t
    | [] -> List.rev all
  in
  List.map (fun x -> aux 0 [] x) m
;;
let cond_nono_col m stand =
  stand = (get_line (transpose_m m))
;;
let fill_line original len =
  let len_ori = List.length original in
  let rec aux acc i =
    let ins = construct_list len 1 in
    if i <= (len_ori - len) then
      let l = (construct_list i 0)@ins in
      aux ((l, construct_list (len_ori-i-len) 0)::acc) (i+1)
    else
      List.rev acc
  in
  aux [] 0
;;
let fill_line_n original lens =
  let rec aux list = function
    | h::t -> 
	let l =
	  List.filter (fun (_,y) -> List.length y >= 1+h) list
	in
	let l2 = List.map (fun (x,y) -> 
	  List.map (fun (m, n) -> (x@(0::m), n)) (fill_line (List.tl y) h)) l
	in
	aux (List.concat l2) t
    | [] -> list
  in
  let l = aux (fill_line original (List.hd lens)) (List.tl lens) in
  List.map (fun (x,y) -> x@y) l
;;
let all_nono_line lines =
  let n = List.length lines in
  let l_blank = construct_list n 0 in
  let rec aux list = function
    | h::t -> 
	let l = fill_line_n l_blank h in
	let l2 = List.map (fun x -> List.map (fun y -> x::y) list) l in
	let l3 = List.concat l2	in
	aux l3 t
    | [] -> List.map List.rev list
  in
  match lines with
  | h::t -> 
      aux (List.map (fun x -> [x]) (fill_line_n l_blank h)) t
;;
let nonograms lines cols =
  List.filter (fun x -> cond_nono_col x cols) (all_nono_line lines)
;;
let s_nono_line = 
  [[3];[1;2];[2;1];[1];[1;2]];;
let s_nono_col =
  [[1];[1;1;1];[4];[2;1];[1;1]];;
(* stack overflow error for the follwing test *)
let test_nono_line =
  [[3];[2;1];[3;2];[2;2];[6];[1;5];[6];[1];[2]];;
let test_nono_col =
  [[1;2];[3;1];[1;5];[7;1];[5];[3];[4];[3]];;
(* 99 Crossword puzzle *)
