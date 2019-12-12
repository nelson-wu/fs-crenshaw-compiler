module Tester

open Cradle
open Expression
open Parser
open System.IO
open System.Diagnostics

let writeTest testName code expected =
    let getCharFromCode = 
        let mutable counter = 0
        let str: string = code + "\n"
        fun () ->
            counter <- counter + 1
            str.[counter-1]

    let expr = expression(ScanState.init getCharFromCode )
    
    let output: string = 
        match expr with
        | Right ss -> ss.output
        | Left err -> failwith err; ""

    let header = 
        """
    ORG $0010000

START: 
"""
    let footer = 
        sprintf
            """
; Check test results
    CMP #%d, D0
    BEQ.S IsEqual
    MOVE D0, -(SP)

    LEA FAILURE, A1
    MOVE.B #13, D0
    TRAP #15

    JMP End

IsEqual:
    LEA SUCCESS, A1
    MOVE.B #13, D0
    TRAP #15

SUCCESS DC.B    'Test Success', 0
FAILURE DC.B    'Test Failed', 0

End:
    END START
            """
            expected

    let testProgram = header + output + footer
    let path = "easy68k/" + testName + ".x68"

    File.WriteAllText(path, testProgram)



