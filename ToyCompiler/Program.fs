// Learn more about F# at http://fsharp.org

open Expression
open Cradle
open Parser

[<EntryPoint>]
let main argv =
    let expr = expression(ScanState.init getChar)
    
    match expr with
    | Left err -> failwith err
    | Right ss -> printfn "%s" ss.output; 0
    // return an integer exit code
