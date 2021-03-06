﻿(*
A fraction like 2/3 can be represented in F# as a pair of type int * int. Define infix operators .+ and .* to do addition and multiplication of fractions:
  > (1,2) .+ (1,3);;
  val it : int * int = (5, 6)
  > (1,2) .+ (2,3) .* (3,7);;
  val it : int * int = (11, 14)
Note that the F# syntax for defining such an infix operator looks like this:
  let (.+) (a,b) (c,d) = ...
Also note that .+ and .* get the same precedences as + and *, respectively, which is why the second example above gives the result it does.
Finally, note that your functions should always return fractions in lowest terms. To implement this, you will need an auxiliary function to calculate the gcd (greatest common divisor) of the numerator and the denominator; this can be done very efficiently using Euclid's algorithm, which can be implemented in F# as follows:

  let rec gcd = function
  | (a,0) -> a
  | (a,b) -> gcd (b, a % b)
*)
let rec gcd = function
| (a, 0) -> a
| (a,b) -> gcd (b, a % b)

let (.+) (a,b) (c,d) = 
    let inter = if b=d then ((a+c), b) else ((a*d + c*b), (b*d))
    let divs = gcd inter
    ((fst inter)/divs, (snd inter)/divs)

(*
Write an F# function revlists xs that takes a list of lists xs and reverses all the sub-lists:
  > revlists [[0;1;1];[3;2];[];[5]];;
  val it : int list list = [[1; 1; 0]; [2; 3]; []; [5]]
*)

let revlists xs = List.map List.rev xs 

(*
Write an F# function interleave(xs,ys) that interleaves two lists:
  > interleave ([1;2;3],[4;5;6]);;
  val it : int list = [1; 4; 2; 5; 3; 6]
Assume that the two lists have the same length.
*)

let rec interleave = function
    | (xs, []) -> []
    | ([], ys) -> []
    | (x::xs, y::ys)-> x::y::interleave( xs, ys)

(*
To implement cut, first define an auxiliary function gencut(n, xs) that cuts xs into two pieces, where n gives the size of the first piece:

  > gencut(2, [1;3;4;2;7;0;9]);;
  val it : int list * int list = ([1; 3], [4; 2; 7; 0; 9])
Paradoxically, although gencut is more general than cut, it is easier to write! (This is an example of Polya's Inventor's Paradox: "The more ambitious plan may have more chances of success.")

Another Hint: To write gencut efficiently, it is quite convenient to use F#'s local let expression (as in the cos_squared example in the Notes).
*)

let rec gencut n xs = 
    match (n, xs) with
    | (0, xs) -> ([], xs)
    | (n, []) -> ([], [])
    | (n, x::xs) -> let (left, right) =
                     gencut (n-1) xs
                    (x::left, right)

let mycut xs = gencut (List.length xs / 2) xs


(*
Write an F# function shuffle xs that takes an even-length list, cuts it into two equal-sized pieces, and then interleaves the pieces:
*)
let shuffle xs = interleave (mycut xs)

(*
Write an F# function countshuffles n that counts how many calls to shuffle on a deck of n distinct "cards" it takes to put the deck back into its original order:
  > countshuffles 4;;
  val it : int = 2
(To see that this result is correct, note that shuffle [1;2;3;4] = [1;3;2;4], and shuffle [1;3;2;4] = [1;2;3;4].) What is countshuffles 52?
*)

let rec countaux = function
    | (d,t) when d = t -> 0
    | (d,t) -> 1 + countaux(d, shuffle t)
   
let countshuffles n =
    let deck = [1..n]
    let target = shuffle deck
    1 + countaux(deck, target)
(*
Write an uncurried F# function cartesian (xs, ys) that takes as input two lists xs and ys and returns a list of pairs that represents the Cartesian product of xs and ys. 
(The pairs in the Cartesian product may appear in any order.) For example,
 > cartesian (["a"; "b"; "c"], [1; 2]);;
  val it : (string * int) list =
  [("a", 1); ("b", 1); ("c", 1); ("a", 2); ("b", 2); ("c", 2)]
*)
let createcoord x y = (x,y)
let rec cartesian (xs, ys) = 
    match (xs, ys) with
    | (xs, []) -> []
    | ([], y::ys) -> []
    | (x::xs, ys) -> (List.map(createcoord x) ys)@cartesian(xs, ys)

(*
An F# list can be thought of as representing a set, where the order of the elements in the list is irrelevant. Write an F# function powerset such that powerset set returns the set of all subsets of set. For example,
  > powerset [1;2;3];;
  val it : int list list
  = [[]; [3]; [2]; [2; 3]; [1]; [1; 3]; [1; 2]; [1; 2; 3]]
*)
let smash x xs = x::xs
let rec powerset = function
| [] -> [[]]
| x::xs -> (powerset xs)@(List.map(smash x) (powerset xs))

(* The transpose of a matrix M is the matrix obtained by reflecting Mabout its diagonal. For example, the transpose of
  / 1 2 3 \
  \ 4 5 6 /
is
  / 1 4 \
  | 2 5 |
  \ 3 6 /
An m-by-n matrix can be represented in F# as a list of m rows, each of which is a list of length n. For example, the first matrix above is represented as the list
  [[1;2;3];[4;5;6]] 
  
   transpose [[1;2;3];[4;5;6]];;
  val it : int list list = [[1; 4]; [2; 5]; [3; 6]]*)

// not really sure this is the best thing, but it works..
let rec transpose xs =
    match xs with
    | [x::xs; y::ys] -> [x;y]::transpose [xs;ys]
    | _ -> []