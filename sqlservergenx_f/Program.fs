open Code

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let dbModel = {statements=[]}
    let randGenerator () = 1
    let dbModel = addTable randGenerator dbModel
    let sql = getSql dbModel
    printfn "%A" sql

    0 // return an integer exit code
