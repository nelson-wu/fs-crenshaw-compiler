// Learn more about F# at http://fsharp.org

open Expression
open Cradle


[<EntryPoint>]
let main argv =
    let expr = expression(ScanState.init())
    match expr with
    | Left err -> failwith err
    | Right _ -> 0
    // return an integer exit code
