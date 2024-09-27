(* Author: Sean Lacson Lee *)

(* Problem 1 Solution *)
let cardinalityList list =
  match list with
  (* Returns 1 if list is empty *)
  | [] -> 1

  (* Returns 2 if list has ONLY ONE element *)
  | [_] -> 2

  (* Returns 0 if list has TWO OR MORE elements *)
  | _ -> 0

  
(* Problem 3 Solution *)
let removeLast list =
  let rec processList listElements = function
    (* Returns an empty list if list is empty *)
    | [] -> []

    (* Returns list in reverse due to last element
     being ignored using the List.rev function *)
    | [element] -> List.rev listElements 

    (* Processes all elements EXCEPT the last element *)
    | element :: lastElement -> processList (element :: listElements) lastElement
  in
  processList [] list


(* Problem 5 Solution *)
let countBinary list =
  let rec processList [] list countZero countOne = function
    (* Returns current variables if list is empty *)
    | [] -> (countZero, countOne)
    | element :: nextElements ->
        match element with
        (* Updates count zero, remaining elements, and recursively calls the function *)
        | 0 -> processList (countZero + 1) countOne nextElements

        (* Update count one, remaining elements, and recursively calls the function *)
        | 1 -> processList countZero (countOne + 1) nextElements

        (* Neither zero or one, no changes, recursively calls the function *)
        | _ -> processList countZero countOne nextElements
  in
  processList 0 0 list (* Starts the recursive counting process as the helper function *)


(* Problem 7 Solution *)
let rec binomial n k =
  match (n, k) with
  (* If k is zero the function returns 1 *)
  | (_, 0) -> 1

  (* If n is zero and k is greater than zero, the function returns zero *)
  | (0, _) -> 0

  (* If k is greater than n, the function returns zero *)
  | (n, k) when k > n -> 0

  (* Formula to calculate binomial coefficient if previous conditons do not apply *)
  | (n, k) -> binomial (n - 1) (k - 1) + binomial (n - 1) k


(* Problem 9 Solution *)
let undup list =
  let rec processList = function
    (* If list is empty, returns empty list *)
    | [] -> []

    (* If ONLY one element in list, returns that element in a list *)
    | [element] -> [element]

    (* Checks equality between head element with the next element in the list *)
    | element :: (nextElement :: _ as otherElements) ->
        (* If head element is equal to the next element then raise bad input *)
        if element = nextElement then raise (Failure "bad input")

        (* Processes the rest of the list *)
        else element :: processList otherElements
  in
  match list with
  (* If list is empty, raise bad input *)
  | [] -> raise (Failure "bad input")

  (* For anything else, calls to process the list *)
  | _ -> processList list


(* Problem 11 Solution *)
(* Definition of a binary tree of floats *)
type floatTree =
  | Leaf of float
  | Node of floatTree * floatTree

(* Declaration of a binary tree of floats with a height of three *)
let binaryTree =
  (* Root node *)
  Node (
    (* Node height one *)
    Node (Leaf 1.0, 
    (* Node height two *)
    Node (Leaf 2.0, Leaf 3.0) (* Leaves extend the tree to a height of three *)),
    Node (Leaf 4.0, Leaf 5.0))


(* Problem 13 Solution *)
let rec countLeaves tree =
  match tree with
  (* Counts a leaf node *)
  | Leaf _ -> 1

  (* If node, then leaves are checked recursively in the function *)
  | Node (left, right) -> countLeaves left + countLeaves right
