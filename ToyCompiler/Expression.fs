module Expression

open Cradle
open Parser

let ident (ss: ScanState) = 
    ss.getName() 
        .flatMap(fun (name, s) -> 
            let s2 = s.matchNext('(')
            match s2 with
            | Right _ -> 
                s2
                    .flatMap(fun s -> s.matchNext(')'))
                    .IO("BSR " + name.ToString(), s.writer)
            | Left _ ->
                s.writer("MOVE " + name.ToString() + "(PC), D0")
                s2
        )

let rec factor (ss: ScanState) = 
    match ss.look with
    | '(' -> 
        ss.matchNext('(')
            .flatMap(expression)
            .flatMap(fun s -> s.matchNext(')'))
    | c when isAlpha c -> ident ss
    | _ ->
        ss
            .getNum()
            .map(fun (c, s) -> 
                s.writer("MOVE #" + c.ToString() + ", D0") 
                s
            )

and multiply(ss: ScanState) = 
    ss.matchNext('*')
        .flatMap(factor)
        .IO("MULS (SP)+, D0", ss.writer)

and divide(ss: ScanState) = 
    ss.matchNext('/')
        .flatMap(factor)
        .IO("MOVE (SP)+, D1", ss.writer)
        .IO("DIVS D1, D0", ss.writer)

and term(ss: ScanState) =
    factor(ss)
        .flatMap(fun s ->
            let rec helper (ss: ScanState)  =
                if ss.look = '*' || ss.look = '/' then
                    s.writer("MOVE D0, -(SP)")
                    let ss2 = 
                        match ss.look with
                        | '*' -> multiply ss
                        | '/' -> divide ss
                        | _ -> Left(expected("mulOp"))

                    ss2.flatMap helper
                else Right ss

            helper s
        )

and add(ss: ScanState) = 
    ss.matchNext('+')
        .flatMap(term)
        .IO("ADD (SP)+, D0", ss.writer)
        
and subtract(ss: ScanState) = 
    ss.matchNext('-')
        .flatMap(term)
        .IO("SUB (SP)+, D0", ss.writer)
        .IO("NEG D0", ss.writer)
        
and expression (ss: ScanState): Either<string, ScanState> =

    let ss2 = 
        if isAddOp ss.look then
            ss.writer("CLR D0")
            Right ss
        else term(ss)
    
    ss2
        .flatMap(fun s -> 
            let rec helper (ss: ScanState) = 
                if isAddOp ss.look then 
                    ss.writer("MOVE D0, -(SP)")
                    let ss2 = 
                        match s.look with
                        | '-' -> subtract ss
                        | '+' -> add ss
                        | _ -> Left(expected("addOp"))
                    
                    ss2.flatMap helper
                else Right ss

            helper s
        )

let assignment (ss: ScanState) =
    ss.getName()
        .flatMap(fun (name, s) -> 
            s.matchNext('=') 
                .flatMap(expression)
                .IO(("LEA " + name.ToString() + "(PC), A0"), ss.writer)
                .IO("MOVE D0, (A0)", ss.writer)
        )


