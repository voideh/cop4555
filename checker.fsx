System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__
#load "PalindromeParser.fsx"
(*
    Our palindrome will fit one of two cases:
    1. The length will be even so it will look like aabbaa
    2. The length will be odd so it will look like aabaa
    In case 1, once we reach the middle of the string ( designated by the GUARD token ),
    we need to return the rest of the tokens to be eaten by the previous recursive calls.

    In case 2, once we reach the GUARD token, we will disregard the token immediately to the right of it
    since it has no matching token to be eaten. We then return the rest of the list to be eaten by the other recursive calls.
*)
open PalindromeParser.PalindromeParser

let eat expected given = 
    match given with
    | x::xs -> if x = expected then xs else [ERROR(sprintf "Got %A was expecting %A" given expected)]
    | [] -> failwith "ERROR:EAT: Got an empty list."

// Handles the even length case
let rec E = function
    | [] -> [ERROR("Unexpected end of input")]
    | [EOS] -> [EOS]
    | GUARD::xs -> xs
    | ALPHA::xs -> xs |> E |> eat ALPHA
    | BETA::xs -> xs |> E |> eat BETA
    | v -> failwith (sprintf "ERROR:E:Unexpected %A received" v)

// Handles the odd length case
let rec O = function
    | [] -> [ERROR("Unexpected end of input")]
    | [EOS] -> [EOS]
    | GUARD::ALPHA::xs -> xs
    | GUARD::BETA::xs -> xs
    | ALPHA::xs -> xs |> O |> eat ALPHA
    | BETA::xs -> xs |> O |> eat BETA
    | v -> failwith (sprintf "ERROR:O:Unexpected %A received" v)

// Input should just be a string of a's and b's
let validate str = 
    let (plist, len) = parse str
    match len % 2 with
    | 0 -> let result = E plist
           if result = [EOS] then sprintf "%A is a palindrome." str else sprintf "%A is not a plaindrome." str
    | n -> let result = O plist
           if result = [EOS] then sprintf "%A is a palindrome." str else sprintf "%A is not a palindrome." str