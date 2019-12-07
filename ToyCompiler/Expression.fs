module Expression

open Cradle

let rec factor (ss: ScanState) = 
    match ss.look with
    | '(' -> 
        ss.matchNext('(')
            .flatMap(expression)
            .flatMap(fun s -> s.matchNext(')'))
    | _ ->
        ss
            .getNum()
            .map(fun (c, s) -> 
                emitLn("MOVE #" + c.ToString() + ", D0") 
                s
            )

and multiply(ss: ScanState) = 
    ss.matchNext('*')
        .flatMap(factor)
        .IO("MULS (SP)+, D0")

and divide(ss: ScanState) = 
    ss.matchNext('/')
        .flatMap(factor)
        .IO("MOVE (SP)+, D1")
        .IO("DIVS D1, D0")

and term(ss: ScanState) =
    factor(ss)
        .flatMap(fun s ->
            let rec helper (ss: ScanState)  =
                if ss.look = '*' || ss.look = '/' then
                    emitLn("MOVE D0, -(SP)")
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
        .IO("ADD (SP)+, D0")
        
and subtract(ss: ScanState) = 
    ss.matchNext('-')
        .flatMap(term)
        .IO("SUB (SP)+, D0")
        .IO("NEG D0")

        
and expression (ss: ScanState): Either<string, ScanState> =
    term(ss)
        .flatMap(fun s -> 
            let rec helper (ss: ScanState) = 
                if ss.look = '+' || ss.look = '-' then 
                    emitLn("MOVE D0, -(SP)")
                    let ss2 = 
                        match s.look with
                        | '-' -> subtract ss
                        | '+' -> add ss
                        | _ -> Left(expected("addOp"))
                    
                    ss2.flatMap helper
                else Right ss

            helper s
        )


 


