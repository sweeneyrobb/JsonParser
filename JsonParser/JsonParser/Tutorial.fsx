open System

let A_Parser str =
    if String.IsNullOrEmpty(str) then
        (false, "")
    else if str.[0] = 'A' then
        let remaining = str.[1..]
        (true, remaining)
    else
        (false, str)

let input = "ZBC"
A_Parser input


type Result<'a> =
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string -> Result<'T * string>)

let pchar charToMatch =
    let innerFn str = 
        if String.IsNullOrEmpty(str) then
            let msg = "No more input"
            Failure msg
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Success (charToMatch, remaining)
            else
                let msg = sprintf "Expect '%c' but got '%c'" charToMatch first
                Failure msg
    Parser innerFn

let run parser input =
    let (Parser innerFn) = parser
    innerFn input

let andThen parser1 parser2 =
    let innerFn input =
        let result = run parser1 input
        match result with
        | Failure msg -> Failure msg
        | Success (charToMatch, remaining) ->
            let result2 = run parser2 remaining
            match result2 with
            | Failure msg -> Failure msg
            | Success (charToMatch2, remaining2) ->
                Success (charToMatch2, remaining2)
    innerFn

let (.>>.) = andThen

let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Success result -> result1
        | Failure _ -> run parser2 input

    innerFn

let (<|>) = orElse


let parseA = pchar 'A'
let parseB = pchar 'B'

let parseAThenB = 
    parseA .>>. parseB

let parseAOrElseB =
    parseA <|> parseB

let inputABC = "ABC"
run parseA inputABC

let inputZBC = "ZBC"
run parseA inputZBC

parseAThenB inputABC

run parseAOrElseB inputABC
run parseAOrElseB inputZBC
