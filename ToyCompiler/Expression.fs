module Expression

open Cradle

let factor (ss: ScanState) = 
    ss
        .getNum()
        .map(fun (c, s) -> 
            emitLn("MOVE #" + c.ToString() + ", D0") 
            s
        )

let multiply(ss: ScanState) = 
    ss.matchNext('*')
        .flatMap(factor)
        .IO("MULS (SP)+, D0")

let divide(ss: ScanState) = 
    ss.matchNext('/')
        .flatMap(factor)
        .IO("MOVE (SP)+, D1")
        .IO("DIVS D1, D0")

let term(ss: ScanState) =
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

let add(ss: ScanState) = 
    ss.matchNext('+')
        .flatMap(term)
        .IO("ADD (SP)+, D0")
        
let subtract(ss: ScanState) = 
    ss.matchNext('-')
        .flatMap(term)
        .IO("SUB (SP)+, D0")
        .IO("NEG D0")

        
let expression ss =
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


 


