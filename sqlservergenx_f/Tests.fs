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
        let rnd = System.Random(1)
        let randGenerator () = rnd.Next()
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

    [<Test>]
    member this.``random strings``() =
        let randGenerator () = 2
        dump (strGen randGenerator)

    [<Test>]
    member this.``seeded random``() =
        let rnd = System.Random(1)
        let xs = List.map (fun x -> rnd.Next()) [1..10]
        dump xs

        let fscheckRand = Random.StdGen(rnd.Next(), rnd.Next())
        
        //dump xs2
