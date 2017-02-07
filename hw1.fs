(*
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
           