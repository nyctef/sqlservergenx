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
let addStatement model statement = {model with statements=statement::model.statements}

let createTableSql props colummns =
    "1234"

let getSql x = 
    List.map (fun statement ->
        match statement with
            | CreateTable (props, columns) -> createTableSql props columns
            | CreateConstraint x -> "hello")
            x.statements

// given a seed and a database model, produce a more complicated database model
type Complicator = int -> DatabaseModel -> DatabaseModel

let addTable:Complicator = fun seed model ->
    let tp = {name="table"; ttype=PlainTable}
    let statement = CreateTable(tp, [{name="col1"; ctype=NVarChar(Length(5))}])
    addStatement model statement
    

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let dbModel = {statements=[]}
    let dbModel = addTable 1 dbModel
    let sql = getSql dbModel
    printfn "%A" sql
    //let dbModel = DatabaseModel{statements:
    //    CreateTable(TableProperties{name:"hllo
    0 // return an integer exit code
