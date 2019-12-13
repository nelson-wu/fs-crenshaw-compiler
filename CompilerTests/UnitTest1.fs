module CompilerTests

open NUnit.Framework
open Tester

[<Test>]
let ``Simple addition results in correct output`` () =
    writeExprTest
        "addition"
        "1+2+3"
        6

[<Test>]
let ``Simple subtraction results in correct output`` () =
    writeExprTest
        "subtraction"
        "5-2"
        3

[<Test>]
let ``Multiplication results in correct output`` () =
    writeExprTest
        "multi"
        "2*2"
        4

[<Test>]
let ``Division results in correct output`` () =
    writeExprTest
        "div"
        "6/3"
        2

[<Test>]
let ``Multiplication and division has higher precedence``() =
    writeExprTest
        "prec"
        "5+4*2/2"
        9
