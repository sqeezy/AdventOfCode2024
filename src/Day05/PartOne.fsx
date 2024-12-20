let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let rulesText = System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}/rules.txt"""
let updatesText =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}/updates.txt"""

type MustBeBefore = { First: int; Second: int }

type Rule = MustBeBefore of MustBeBefore
let parseRule (line: string) =
    line
    |> splitAt '|'
    |> Array.map (fun x -> x.Trim())
    |> (fun x ->
        { First = x.[0] |> int
          Second = x.[1] |> int })

type Update = int list
let parseUpdate (line: string) : Update =
    line
    |> splitAt ','
    |> Array.map (fun x -> x.Trim())
    |> Array.map int
    |> List.ofArray

let satisfiesMustBeBefore (rule: MustBeBefore) (update: Update) =
    let firstIndex = update |> List.tryFindIndex (fun x -> x = rule.First)
    let secondIndex = update |> List.tryFindIndex (fun x -> x = rule.Second)

    match firstIndex, secondIndex with
    | Some firstIndex, Some secondIndex -> firstIndex < secondIndex
    | _ -> true

let validateUpdate (rules: MustBeBefore array) (update: Update) =
    rules
    |> Array.forall (fun rule -> satisfiesMustBeBefore rule update)

let getMiddleNumberOfUpdate (update: Update) =
    update.[update.Length/2]

let rules = rulesText |> Array.map parseRule
let updates = updatesText |> Array.map parseUpdate

let partOne = 
    updates 
    |> Array.filter (validateUpdate rules)
    |> Array.map getMiddleNumberOfUpdate
    |> Array.sum
