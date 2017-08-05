module Tests

open NUnit.Framework
open FsCheck
open Code

[<TestFixture>]
type TestClass() = 

    [<Test>]
    member this.scratchPad() =
        let dbModel = {statements=[]}
        let randGenerator () = 1
        let dbModel = addTable randGenerator dbModel
        let dbModel = addColumn randGenerator dbModel
        let sql = getSql dbModel
        printfn "%A" sql

    [<Test>]
    member this.``fscheck scratchpad``() =
        let g = gen { return true };
        let r = Random.StdGen (1,2)
        let result = Gen.eval 1 r g
        printfn "%A" result