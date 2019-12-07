module Cradle

open System

let TAB = "\t"

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

let emit(s: string) = printfn "%s%s" s TAB

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

    member this.IO s = 
        this.map(fun r -> 
            emitLn s
            r
        )


type ScanState = { mutable look: char }

type ScanState with
    static member init() = { look = getChar() }

    member this.matchNext(next: char) = 
        if this.look = next then
            this.look <- getChar()
            Right this
        else Left (expected(next.ToString()))

    member this.get discriminator () = 
        if not (discriminator this.look) then Left(expected(discriminator.GetType().ToString()))
        else 
            let name = this.look
            this.look <- getChar()
            Right(name, this)

    member this.getName = this.get isAlpha
    member this.getNum = this.get isDigit

