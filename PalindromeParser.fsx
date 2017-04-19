System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__
module PalindromeParser =
    type PALINDROME = ALPHA | BETA | GUARD | EOS | ERROR of string
    // parse the string and also calculate it's length ( ignoring the EOS token )
    let rec firstpass = function
    | ("", n) -> ([EOS], n)
    | (s, n) -> match s.Chars 0 with
                | 'a' -> let (plist, count) = firstpass (s.Substring 1, n+1)
                         (ALPHA::plist, count)
                | 'b' -> let (plist, count) = firstpass (s.Substring 1, n+1)
                         (BETA::plist, count)
                | v -> ([ERROR(sprintf "%A is not in our language." v)], -1)
    // now insert the GUARD token in the middle of the list
    let rec secondpass = function
    | ([], _) -> ([ERROR(sprintf "Parse Error : Unexpected end of input.")], -1)
    | (xs, 0) -> (GUARD::xs, 0)
    | (x::xs, n) -> let (plist, count) = secondpass (xs, n-1)
                    (x::plist, count)
    let parse str = 
        let (initial, len) = firstpass(str, 0)
        let (final, x) = secondpass(initial, len / 2)
        (final, len)