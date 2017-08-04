open Code

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let dbModel = {statements=[]}
    let dbModel = addTable 1 dbModel
    let sql = getSql dbModel
    printfn "%A" sql

    0 // return an integer exit code
