type TERMINAL = IF|THEN|ELSE|BEGIN|END|PRINT|SEMICOLON|ID|EOF|ERROR
type PARSETREE =
| Lf of TERMINAL
| IFBr of PARSETREE*PARSETREE*PARSETREE*PARSETREE*PARSETREE*PARSETREE
| BEGBr of PARSETREE*PARSETREE*PARSETREE
| PrintBr of PARSETREE*PARSETREE
| SemiBR of PARSETREE*PARSETREE*PARSETREE

let E = function
    | [] -> failwith "ERROR @ E"
    | x::xs -> match x with
                | ID -> (Lf(ID), xs)
                | _ -> failwith (sprintf "E: expected ID got %A" x)

let eat token xs =
    match xs with
    | [] -> failwith "No tokens to eat"
    | x::xs -> if x = token then xs else failwith (sprintf "EAT: expected %A got %A" token x)

let rec S = function
    |  [] -> failwith "Permature termination of input"
    |  x::xs -> 
            match x with 
            | IF ->
                let (T_E, remain) = xs |> E
                let remain = remain |> eat THEN
                let (T_S, remain) = remain |> S
                let remain = remain |> eat ELSE
                let (T_S2, remain) = remain |> S
                (IFBr(Lf(IF), T_E, Lf(THEN), T_S, Lf(ELSE), T_S2), remain)
            | BEGIN -> 
                let (T_S, remain) = xs |> S
                let (T_L, remain) = remain |> L
                (BEGBr(Lf(BEGIN), T_S, T_L), remain)
            | PRINT ->
                let (T_E, remain) = xs |> E
                (PrintBr(Lf(PRINT), T_E), remain)
            | _ -> failwith (sprintf "S: Expected IF, BEGIN, PRINT got %A" x)
and L = function
    | [] -> failwith "Premature termination of input"
    | x::xs -> 
            match x with
            | END -> (Lf(END), xs)
            | SEMICOLON -> 
               let (S_tree, remain) = xs |> S
               let (L_tree, remain) = remain |> L
               (SemiBR(Lf(SEMICOLON), S_tree, L_tree), remain)
            | _ -> failwith (sprintf "L: Expected END, SEMICOLON got %A" x)

let buildTree = function
| [] -> Lf(ERROR)
| xs -> let (S_tree, tokens) = xs |> S
        if tokens <> [EOF] then failwith (sprintf "Want [EOF], got %A" tokens)
        else S_tree

let rec interpret_tree tree = 
    match tree with
    | IFBr (iftok, idtok, thentok, ex1, elsetok, ex2) -> 
                                                        let exp1 = interpret_tree ex1
                                                        let exp2 = interpret_tree ex2
                                                        [IF; ID; THEN] @ exp1 @ [ELSE] @ exp2
    | Lf (x) -> [x]
    | PrintBr (x, y) -> let expy = interpret_tree y
                        [PRINT] @ expy
    | SemiBR (x, y, z) -> let expy = interpret_tree y
                          let expz = interpret_tree z
                          [SEMICOLON] @ expy @ expz
    | BEGBr(x,y,z) -> let expy = interpret_tree y
                      let expz = interpret_tree z
                      [BEGIN] @ expy @ expz

let buildandinterp toks = toks |> buildTree |> interpret_tree

// Non recursive fibonacci

let nonrecfib stop =
    let mutable x = 0
    let mutable y = 1
    for n=0 to stop do
        printfn "%A: %A" n x
        let saved = x
        x <- y      
        y <- saved + y

// mutable records

type Student = {
    mutable GPA : float
    mutable Credits : int
 }
let getGpa student = student.GPA
let setGpa student newGpa = student.GPA <- newGpa
let setCredits student newCredits  = student.Credits <- student.Credits + newCredits

// tuple stack
let mkstack =
    let stk = ref ([]:int list)
    ((fun x -> stk := x :: (!stk)), // push
     (fun () -> stk := List.tail (!stk)),// pop
     (fun () -> List.head (!stk)),
     (fun () -> List.isEmpty (!stk)))


// non recursive factorial using mutable stacks
let fax n =
    let (push, pop, peek, isEmpty) = mkstack
    let count = ref n
    while !count > 0 do
        push !count
        count := !count - 1
    let mutable accm = 1
    while not(isEmpty()) do
         accm <- accm * peek()
         pop()
    accm

type Palindrome = ALPHA|BETA|GUARD|ERROR of string|EOS

let rec parse1 = function
    | ("" , len) -> ([EOS], len)
    | (s, len) -> match s.Chars 0 with
                    | 'a' -> let (res, leng) = parse1 (s.Substring 1, len+1)
                             (ALPHA::res, leng)
                    | 'b' -> let (res, leng) = parse1 (s.Substring 1, len+1)
                             (BETA::res, leng)
                    | v -> failwith (sprintf "%A is not in our language." v)
let rec parse2 = function
    | (xs, 0) -> (GUARD::xs, 0)
    | (x::xs, n) -> let (result, d) = parse2(xs, n-1)
                    (x::result, d)
    | ([], _ ) -> failwith "received an empty list."

let parse str = 
    let (inter, len) = parse1 (str, 0)
    let (final, d) = parse2 (inter, len/2)
    (final, len)


let rec pE = function
    | [] -> failwith "Empty list"
    | GUARD::xs -> xs
    | ALPHA::xs -> (pE xs) |> eat ALPHA
    | BETA::xs -> (pE xs) |> eat BETA
    | v -> failwith (sprintf "Didn't expect %A" v)
let rec pO = function
    | [] -> failwith "Empty list"
    | GUARD::x::xs -> xs
    | ALPHA::xs -> (pO xs) |> eat ALPHA
    | BETA::xs -> (pO xs) |> eat BETA
    | v -> failwith (sprintf "Didn't expect %A" v)

let validate str =
    let (parsed, len) = parse str
    if len % 2 = 0 then pE parsed else pO parsed