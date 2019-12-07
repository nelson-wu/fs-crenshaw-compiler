module Expression

open Cradle

let term(ss: ScanState) =
    ss.getNum().map(fun (c, s) -> 
        emitLn("MOVE #" + c.ToString() + ", D0")
        s
    )

let add(ss: ScanState) = 
    ss.matchNext('+')
        .flatMap(term)
        .map(fun s -> 
            emitLn("ADD (SP)+, D0")
            s
        )
        
let subtract(ss: ScanState) = 
    ss.matchNext('-')
        .flatMap(term)
        .map(fun s -> 
            emitLn("SUB (SP)+, D0")
            emitLn("NEG D0")
            s
        )
        
let expression ss =
    term(ss)
        .flatMap(fun s -> 
            let rec helper (ss: ScanState) = 
                if ss.look = '+' || ss.look = '-' then 
                    emitLn("MOVE D0, -(SP)")
                    let ss2 = 
                        match s.look with
                        | '-' -> subtract(s)
                        | '+' -> add(s)
                        | _ -> Left(expected("addOp"))
                    
                    ss2.flatMap helper
                else Right ss

            helper s
        )


    


