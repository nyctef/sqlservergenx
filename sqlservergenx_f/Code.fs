module Code

open FsCheck

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

let alpha = List.append ['a'..'z'] ['A'..'Z']

let alphan = List.append alpha ['0'..'9']

let randToSeed rand =
    Random.StdGen (rand (), rand ())

let charsToString (chars:char list) =
    System.String.Concat(Array.ofList(chars))

let genAlpha rand length =
    let gen = Gen.elements alpha
    Seq.initInfinite (fun _ ->
            Gen.eval length (randToSeed rand) gen
        )
        |> Seq.take length
        |> Seq.toList
        |> charsToString

let strGen rand =
    Arb.generate<string> |> Gen.eval 10 (randToSeed rand)

let colGen rand =
    let seed = randToSeed rand
    {Column.name="col_" + genAlpha rand 10; ctype=Arb.generate<ColumnType> |> Gen.eval 1 seed}

let addTable:Complicator = fun rand model ->
    let tp = {name="table_" + genAlpha rand 10; ttype=PlainTable; columns = [colGen rand]}
    let statement = CreateTable(tp)
    addStatement model statement

let addColumn:Complicator = fun rand model ->
    let updateTable (CreateTable table) =
        let newColumn = colGen rand
        CreateTable({table with columns = newColumn::table.columns})
    findOrCreateTable rand model updateTable