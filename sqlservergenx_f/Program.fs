type CharLength =
    | Max
    | Length of int

type ColumnType =
    | Bit
    | Int
    | NVarChar of CharLength

type ColumnName = string

type ObjectName = string
let objectNameSql x = x

type Column = {name:ColumnName; ctype:ColumnType}

type ConstraintDefinition = {name:string}

type TableType =
    | PlainTable
    | TemporalTable
    | FileTable
    | NodeTable
    | EdgeTable

type TableDefinition = {name:ObjectName; ttype:TableType; columns:List<Column>}

type Statement =
    | CreateTable of TableDefinition
    | CreateConstraint of ConstraintDefinition

type DatabaseModel = {statements: List<Statement>}
let addStatement model statement = {model with statements=statement::model.statements}


///////////////////////// sql generation

let columnSql column =
    "1234"

let createTableSql table =
    "CREATE TABLE " + objectNameSql table.name + "\n" +
    "(\n" +
    (List.map columnSql table.columns |> String.concat ",\n") +
    ")\n"

let getSql x = 
    List.map (fun statement ->
        match statement with
            | CreateTable table -> createTableSql table
            | CreateConstraint x -> "hello")
            x.statements


/////////////////////////// model generation

// given a seed and a database model, produce a more complicated database model
type Complicator = int -> DatabaseModel -> DatabaseModel

let addTable:Complicator = fun seed model ->
    let tp = {name="table"; ttype=PlainTable; columns = [{name="col1"; ctype=NVarChar(Length(5))}]}
    let statement = CreateTable(tp)
    addStatement model statement
    

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let dbModel = {statements=[]}
    let dbModel = addTable 1 dbModel
    let sql = getSql dbModel
    printfn "%A" sql

    0 // return an integer exit code
