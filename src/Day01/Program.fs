open Day01
open System

let distance (a, b) =
    a - b |> Int32.Abs

let ints = 
    Text
    |> splitAt '\n'
    |> Array.map (splitAt ' ')
    |> Array.map (Array.filter (fun s -> s <> ""))
    |> Array.map (fun a -> (int a.[0], int a.[1]))

let left = ints |> Array.map fst |> Array.sort
let right = ints |> Array.map snd |> Array.sort


let partOne = Array.zip left right|> Array.sumBy distance

let rightGrouped = 
    right 
    |> Array.groupBy id 
    |> Array.map (fun (k, v) -> (k, v.Length)) 
    |> Map.ofArray

let partTwo =
    left
    |> Array.map (fun i -> Map.tryFind i rightGrouped |> Option.defaultValue 0 |> (*) i)
    |> Array.sum