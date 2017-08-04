type CharLength =
    | Max
    | Length of int

type ColumnType =
    | Bit
    | Int
    | NVarChar of CharLength

type Column = {name:string; ctype:ColumnType}

type Constraint = {name:string}

type TableType =
    | PlainTable
    | TemporalTable
    | FileTable
    | NodeTable
    | EdgeTable

type TableProperties = {name:string; ttype:TableType}

type Statement =
    | CreateTable of TableProperties * List<Column>
    | CreateConstraint of Constraint

type DatabaseModel = {statements: List<Statement>}

let createTableSql props colummns =
    "1234"

let getSql x = 
    List.map (fun statement ->
        match statement with
            | CreateTable (props, columns) -> createTableSql props columns
            | CreateConstraint x -> "hello")
            x.statements

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
