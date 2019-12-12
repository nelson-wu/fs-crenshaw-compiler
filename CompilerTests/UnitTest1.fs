module CompilerTests

open NUnit.Framework
open Tester

[<Test>]
let ``Simple addition results in correct output`` () =
    writeTest
        "addition"
        "1+2+3"
        6
