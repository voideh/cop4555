// Skeleton file for PCF interpreter

// This sets F# to read from whatever directory contains this source file.
System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__;;

#load "parser.fsx"

// This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
// and to constructors like "Parser.Parse.APP" simply as "APP".
open Parser.Parse

// Here I show you a little bit of the implementation of interp. Note how ERRORs
// are propagated, how rule (6) is implemented, and how stuck evaluations
// are reported using F#'s sprintf function to create good error messages.

let rec subst e x t =
    match e with
    | IF(x1, y1, z1) -> IF((subst x1 x t), (subst y1 x t), (subst z1 x t))
    | FUN(param, body) -> FUN(param, if param=x then body else subst body x t)
    | ID(i) -> if i = x then t else ID(i)
    | APP (e1, e2) -> APP (subst e1 x t, subst e2 x t)
    | REC (param, body) -> REC(param, if param=x then body else subst body x t)
    | _ -> e

let rec interp = function
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _)  -> ERROR s        // ERRORs are propagated
    | (_, ERROR s)  -> ERROR s
    | (SUCC, NUM n) -> NUM (n+1)      // Rule (6)
    | (SUCC, v)     -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)
    | (PRED, NUM n) -> if n = 0 then NUM (0) else NUM (n-1)
    | (PRED, v) -> ERROR (sprintf "'pred' needs int argument, not '%A'" v)
    | (ISZERO, NUM n) -> if n = 0 then BOOL (true) else BOOL (false)
    | (ISZERO, v) -> ERROR (sprintf "'iszero' needs int argument, not '%A'" v)
    | (FUN (x,body), v) -> subst body x v |> interp
    | (ID a, ID b) -> APP(ID a, ID b)
    | (a, b) -> ERROR (sprintf "No case for '%A' or '%A'" a b)
| NUM (n) -> NUM (n)
| BOOL (b) -> BOOL (b)
| SUCC -> SUCC
| IF (guard, thenb, elseb) -> match interp guard with
                              | BOOL(true) -> interp thenb
                              | BOOL(false) -> interp elseb
                              | v -> ERROR (sprintf "Expected bool got %A" v)
| PRED -> PRED
| ISZERO -> ISZERO
| ID(s) -> ID(s)
| REC(param, body) -> subst body param (REC(param, body)) |> interp
| FUN(param, body) -> FUN(param, body)
| v -> ERROR (sprintf "Got the follopwing term %A" v)

// Here are two convenient abbreviations for using your interpreter.
let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp