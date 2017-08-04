module Tests

open NUnit.Framework
open Code

[<TestFixture>]
type TestClass() = 

    [<Test>]
    member this.scratchPad() = 
        let dbModel = {statements=[]}
        let dbModel = addTable 1 dbModel
        let sql = getSql dbModel
        printfn "%A" sql