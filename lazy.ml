type 'a deferred = Defer of (unit -> 'a)
let defer (f : unit -> 'a) = Defer f
let force (exp : 'a deferred) =
  match exp with
  | Defer(f) -> f ()
;;

type 'a lazy_list =
    Nil
  | Cons of 'a * 'a lazy_list
  | LazyCons of 'a * 'a lazy_list deferred
;;

let nil = Nil;;
let cons a l = Cons(a, l);;
let lazy_cons a (l : unit -> 'a lazy_list) = LazyCons(a, Defer(l));;
let is_nil x = x = Nil;;
let head (l : 'a lazy_list) = 
  match l with
  | Cons(a, _) -> a
  | LazyCons(a, _) -> a
  | Nil -> failwith "hd"
;;
let tail = function
  | Cons(_, l) -> l
  | LazyCons(_, Defer(l)) -> l ()
  | Nil -> failwith "tl"
;;
let (@@) l1 l2 =
  let rec aux acc l =
    match l with
    | Nil -> acc
    | _ ->
	aux (lazy_cons (head l) (fun () -> acc)) (tail l)
  in
  let l = aux (aux Nil l1) l2 in
  aux Nil l
;;

module Queue = struct
  type 'a queue = ('a list * 'a list) ref
  let (empty : 'a queue) = ref ([], [])
  let is_empty (q : 'a queue) = match !q with (a, b) -> a = [] && b = []
  let add (q : 'a queue) elem : 'a queue = 
    match !q with
    | (a, b) -> ref (elem::a, b)
  let rec take (q : 'a queue) : 'a * 'a queue =
    match !q with
    | (front, x::back) ->
	(x, ref (front, back))
    | ([], []) -> raise (Invalid_argument "queue is empty")
    | (front, []) ->
	q := ([], List.rev front);
	take q
end;;

let memo f =
  let table = ref [] in
  let rec find_or_apply entries x =
    match entries with
    | (x', y)::_ when x' = x -> y
    | _::entries -> find_or_apply entries x
    | [] ->
	let y = f x in
	table := (x, y)::!table;
	y
  in
  (fun x -> find_or_apply !table x)
;;

module Memo = struct
  type ('a, 'b) memo = 'a list ref
  let create_memo () : ('a, 'b) memo = ref ['a', 1]
end;;

type expression =
  | Num of int
  | Var of string
  | Let of string * expression * expression
  | Binop of string * expression * expression
;;

let rec eval env = function
  | Num i -> i
  | Var x -> List.assoc x env
  | Let (x, e1, in_e2) ->
      let val_x = eval env e1 in
      eval ((x, val_x)::env) in_e2
  | Binop (op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      eval_op op v1 v2
and eval_op op v1 v2 =
  match op with
  | "+" -> v1 + v2
  | "-" -> v1 - v2
  | "*" -> v1 * v2
  | "/" -> v1 / v2
  | _ -> failwith ("Unknown: " ^ op);;

module Hash = struct
  let random_numbers = 
    [|0x04a123;0x42342;0x9dc492;0x12342;0xf923984;0x123995;0x938284|]
  let random_length = Array.length random_numbers
  type hash_info = {mutable hash_index : int; mutable hash_value : int }
  let hash_char info c =
    let i = Char.code c in
    let index = (info.hash_index + i + 1) mod random_length in
    info.hash_value <- (info.hash_value * 3) lxor random_numbers.(index);
    info.hash_index <- index
  let hash s = 
    let info = { hash_index = 0; hash_value = 0} in
    for i = 0 to String.length s - 1 do
      hash_char info s.[i]
    done;
    info.hash_value
  type 'a hash_entry = { key : string; value : 'a }
  type 'a hash_table = 'a hash_entry list array
  let create () =
    Array.create 101 []
  let add table key value =
    let index = (hash key) mod (Array.length table) in
    table.(index) <- { key = key; value = value }::table.(index)
  let rec find_entry key = function
      { key = key'; value = value}::_ when key' = key -> value
    | _::entries -> find_entry key entries
    | [] -> raise Not_found
  let find table key =
    let index = (hash key) mod (Array.length table) in
    find_entry key table.(index)
end;;
module Insertion = struct
  let insert (a : 'a array) (i : int) = 
    begin
    let x = a.(i) in
    let j = ref (i-1) in
    while !j >= 0 && a.(!j) > x do
      a.(!j+1) <- a.(!j);
      j := !j - 1;
    done;
    a.(!j+1) <- x;
    end
  let insertion_sort a =
    let i = ref 1 in
    while !i < Array.length a do
      insert a !i;
      i := !i + 1
    done;
    i := 0;
    while !i < Array.length a do
      Printf.printf "%d\n" a.(!i);
      i := !i + 1
    done
end;;

(*
type 'a list = Nil | Cons of 'a * 'a list;;
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream);;
(* an infinite stream of 1's *)
let rec (ones : int stream) = Cons (1, fun () -> ones);;
(* the natural numbers *)
let rec from (n : int) : int stream =
  Cons (n, fun () -> from (n + 1))
let naturals = from 0;;
*)
