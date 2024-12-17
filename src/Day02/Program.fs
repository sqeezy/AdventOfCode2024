open System

type GradientAtPosition = Up | Down | Level

let gradientFromDifference (difference : int) =
    match difference with
    | 0 -> Level
    | x when x > 0 -> Up
    | x when x < 0 -> Down

let splitLine (line: string) = line.Split(' ') |> Array.map int

let isSafe (levels : int []) =

    let differences = 
        levels
        |> Array.pairwise
        |> Array.map (fun (a, b) -> a - b)

    let onlyOneTypeOfGradient = 
        differences
        |> Array.map gradientFromDifference 
        |> Array.distinct
        |> Array.length = 1

    let levelDifferencesAreSafe = // that means they are between one and three
        differences
        |> Array.map (fun x -> x |> abs)
        |> Array.forall  (fun x -> x >= 1 && x <= 3)

    onlyOneTypeOfGradient && levelDifferencesAreSafe

let lines =
    Text
    |> splitAt '\n'
    |> Array.map splitLine
    |> Array.map isSafe
    |> Array.filter id
    |> Array.length