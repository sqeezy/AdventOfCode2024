open System

type GradientAtPosition =
    | Up
    | Down
    | Level

let gradientFromDifference (difference: int) =
    match difference with
    | 0 -> Level
    | x when x > 0 -> Down
    | x when x < 0 -> Up
    | _ -> failwith "This should never happen"

let splitLine (line: string) = line.Split(' ') |> Array.map int

let magnitudeIsSafe (magnitude: int) = magnitude >= 1 && magnitude <= 3

let isSafe (levels: int[]) =

    let differences = levels |> Array.pairwise |> Array.map (fun (a, b) -> a - b)

    let onlyOneTypeOfGradient =
        differences
        |> Array.map gradientFromDifference
        |> Array.distinct
        |> Array.length = 1

    let levelDifferencesAreSafe = // that means they are between one and three
        differences |> Array.map (fun x -> x |> abs) |> Array.forall magnitudeIsSafe

    onlyOneTypeOfGradient && levelDifferencesAreSafe

let partOne =
    Text
    |> splitAt '\n'
    |> Array.map splitLine
    |> Array.map isSafe
    |> Array.filter id
    |> Array.length

let isSafe2 (levels: int[]) =

    let range = [| 0 .. (levels.Length - 1) |]

    range
    |> Array.map (fun index -> Array.removeAt index levels)
    |> Array.map isSafe
    |> Array.contains true

isSafe2 [| 7; 6; 4; 2; 1 |]
isSafe2 [| 1; 2; 7; 8; 9 |]

let partTwo =
    Text
    |> splitAt '\n'
    |> Array.map splitLine
    |> Array.map isSafe2
    |> Array.filter id
    |> Array.length
