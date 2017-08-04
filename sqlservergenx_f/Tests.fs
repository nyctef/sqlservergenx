module Tests

open NUnit.Framework
open Code

[<TestFixture>]
type TestClass() = 

    [<Test>]
    member this.scratchPad() = 
        let dbModel = {statements=[]}
        let randGenerator () = 1
        let dbModel = addTable randGenerator dbModel
        let sql = getSql dbModel
        printfn "%A" sql