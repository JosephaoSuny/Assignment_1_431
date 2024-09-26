open Int;;
open List;;

(* Problem 2 Solution *)
(* Author: Joseph *)
let rec rotate list = 
  match list with 
  | [] -> []
  | hd::tl -> rotate tl @ [hd]
;;

(* Problem 4 Solution *)
(* Author: Joseph *)
let rec remove y list =
  match list with
  | [] -> []
  | hd::tl when hd != y -> [hd] @ remove y tl
  | hd::tl -> remove y tl
;;

(* Problem 6 Solution *)
(* Author: Joseph *)
let rec make_pairs x list = 
  match list with 
  | [] -> [] 
  | hd::tl -> [(x, hd)] @ make_pairs x tl
;;

(* Problem 8 Solution *)
(* Author: Joseph *)
let rec dup list = 
  match list with 
  | [] -> [] 
  | hd::tl -> [hd; hd] @ dup tl
;;

(* Problem 9 Solution *)
(* Author: Joseph *)
let rec undup list = 
  match list with 
  | [] ->[]
  | hd::[] -> raise (Failure "Bad input")
  | hd::sc::[] -> if sc = hd then [hd] else raise (Failure "Bad input")
  | hd::sc::tl -> if sc = hd then undup tl else raise (Failure "Bad input")
;;

(* Problem 10 Solution *)
(* Author: Joseph *)
let rec smallest_aux list x = 
  match list with
  | [] -> x
  | hd::tl -> if hd > x then smallest_aux tl x else smallest_aux tl hd
;;

let rec smallest list = 
  match list with
  | [] -> raise (Failure "Empty list")
  | hd::[] -> hd
  | hd::tl -> let tl_smallest = smallest tl in Int.min tl_smallest hd
;;

(* Problem 11 Solution *)
(* Author: Joseph *)
type 'a tree = | Leaf | Node of 'a node and 'a node = { value: 'a; left: 'a tree; right: 'a tree; };;

let tree = Node {
  value = 5.0;
  right = Leaf;
  left = Node {
    value = 3.0;
    right = Leaf;
    left = Node {
      value = 1.0;
      right = Leaf;
      left = Leaf;
    }
  }
};;

(* Problem 12 Solution *)
(* Author: Joseph *)
let rec max_depth tree = 
  match tree with 
  | Leaf -> 0
  | Node { value; left = Leaf; right = Leaf; } -> 1
  | Node { value; left = Leaf; right; } -> 1 + max_depth right
  | Node { value; left; right = Leaf; } -> 1 + max_depth left
  | Node { value; left; right; } -> 
    let left_depth = max_depth left in
    let right_depth = max_depth right in
    if left_depth > right_depth then 1 + left_depth else 1 + right_depth
;;

(* Problem 13 Solution *)
(* Author: Joseph *)
let rec count_nodes tree = 
  match tree with 
  | Leaf -> 0
  | Node { value; left = Leaf; right = Leaf; } -> 1
  | Node { value; left = Leaf; right; } -> 1 + count_nodes right
  | Node { value; left; right = Leaf; } -> 1 + count_nodes left
  | Node { value; left; right; } -> 1 + count_nodes left + count_nodes right
;;

let list = [5; 3; 2; 8; 1];;

let depth_of_tree = smallest list;;

print_int(depth_of_tree);;
print_newline();;
