module Tests

open NUnit.Framework
open FsCheck
open Code

let dump x = printfn "%A" x

[<TestFixture>]
type TestClass() = 
    [<Test>]
    member this.scratchPad() =
        let dbModel = {statements=[]}
        let randGenerator () = 1
        let dbModel = addTable randGenerator dbModel
        let dbModel = addColumn randGenerator dbModel
        let sql = getSql dbModel
        dump sql

    [<Test>]
    member this.``fscheck scratchpad``() =
        let g = gen { return true };
        let seed = Random.StdGen (1,2)
        let result = Gen.eval 1 seed g
        dump result

    [<Test>]
    member this.``get default generator``() =
        let g = Arb.generate<ColumnType>
        dump g
        dump (Gen.sample 1 10 g)