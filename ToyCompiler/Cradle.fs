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

let emit(s: string) = printfn "%s%s" s TAB

let emitLn(s: string) = 
        emit(s)
        emit("\n")

let term(ss: ScanState) =
    ss.getNum().map(fun (c, s) -> 
        emitLn("MOVE #" + c.ToString() + ", D0")
        s
    )

let add(ss: ScanState) = 
    ss.matchNext('+')
        .flatMap(term)
        .map(fun s -> 
            emitLn("ADD D1, D0")
            s
        )
        
let subtract(ss: ScanState) = 
    ss.matchNext('-')
        .flatMap(term)
        .map(fun s -> 
            emitLn("SUB D1, D0")
            emitLn("NEG D0")
            s
        )
        
let expression ss =
    term(ss)
        .flatMap(fun s -> 
            let rec helper (ss: ScanState) = 
                if ss.look = '+' || ss.look = '-' then 
                    emitLn("MOVE D0, D1")
                    let ss2 = 
                        match s.look with
                        | '-' -> subtract(s)
                        | '+' -> add(s)
                        | _ -> Left(expected("addOp"))
                    
                    ss2.flatMap helper
                else Right ss

            helper s
        )


    

