module Cradle

open System

let TAB = '\t'

let getChar () = 
    let x = Console.Read();
    Convert.ToChar x

let error(s: string) = 
    printfn "\n Error: %s" s

let abort(s: string) =
    error s
    failwith s

let expected(s: string) = 
    s + " expected"

let isAlpha(c: char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let isDigit(c: char) = c >= '0' && c <= '9'

let isAlNum c = isAlpha c || isDigit c

let isAddOp look = look = '+' || look = '-'

let isWhiteSpace c = c = ' ' || c = TAB

let emit(s: string) = printfn "%s%s" s (TAB.ToString())

let emitLn(s: string) = 
        emit(s)
        emit("\n")

type Either<'L, 'R> = 
    | Left of 'L
    | Right of 'R

type Either<'L, 'R> with   
    member this.map (f: 'R -> 'T) =
        match this with
        | Right x -> Right (f x)
        | Left err -> Left err

    member this.flatMap (f: 'R -> Either<'L, 'T2>) = 
        match this with
        | Right x -> f x
        | Left err -> Left err

    member this.IO (s, writer) = 
        this.map(fun r -> 
            writer s
            r
        )

