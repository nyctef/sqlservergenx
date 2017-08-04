﻿module Code

type CharLength =
    | Max
    | Length of int

type ColumnType =
    | Bit
    | Int
    | NVarChar of CharLength

type ColumnName = string

type ObjectName = string

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

let columnNameSql x = x
let objectNameSql x = x

let lengthSql l =
    match l with
    | Max -> "MAX"
    | Length x -> string x

let columnTypeSql (coltype:ColumnType) =
    match coltype with
    | Bit -> "bit"
    | Int -> "int"
    | NVarChar length -> "nvarchar(" + lengthSql length + ")"

let columnSql (column:Column) :string =
    columnNameSql column.name + " " + columnTypeSql column.ctype

let createTableSql table =
    "CREATE TABLE " + objectNameSql table.name + "\n" +
    "(\n" +
    (List.map columnSql table.columns |> String.concat ",\n") + "\n" +
    ")\n"

let getSql x = 
    List.map (fun statement ->
        match statement with
            | CreateTable table -> createTableSql table
            | CreateConstraint x -> "hello")
            x.statements


/////////////////////////// model generation

type RandomGenerator = unit -> int

// given a seed and a database model, produce a more complicated database model
type Complicator = RandomGenerator -> DatabaseModel -> DatabaseModel

let pickTables statements :Statement list=
    statements |> List.choose(fun x ->
        match x with
        | CreateTable _ -> Some x
        | _ -> None)

let randMax rand max :int =
    (rand ()) % max

let randChoose rand (list:'a list) =
    List.item (randMax rand list.Length) list

let replaceStatement list oldvalue newvalue =
    list |> List.map (fun x ->
        match x with
        | y when y = oldvalue -> newvalue
        | z -> z)

let findOrCreateTable rand model updateTable =
    // TODO: we want to be able to
    // - find all tables we could add a column to
    // - add a new table if we aren't satisfied with the available options
    // - pick a table at random
    // - call updateTable on that table
    // - return the whole model with the updated table

    // we can probably try writing the massively inefficient version of the above
    // and look into swapping out the datatypes to make it more efficient in the future?
    let tables = pickTables model.statements
    // TODO: do we actually need to handle case of no/too few tables?
    let table = randChoose rand tables
    let updatedTable = updateTable table
    let statements = replaceStatement model.statements table updatedTable
    {model with statements = statements}

let addTable:Complicator = fun rand model ->
    let tp = {name="table"; ttype=PlainTable; columns = [{name="col1"; ctype=NVarChar(Length(5))}]}
    let statement = CreateTable(tp)
    addStatement model statement

let addColumn:Complicator = fun rand model ->
    let updateTable (CreateTable table) =
        let newColumn = {name="col2"; ctype=NVarChar(Length(5))}
        CreateTable({table with columns = newColumn::table.columns})
    findOrCreateTable rand model updateTable