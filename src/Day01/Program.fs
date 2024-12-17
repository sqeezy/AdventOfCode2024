open Day01
open System

let ints = 
    Text.Split('\n')
    |> Array.map (fun s -> s.Split(' '))
    |> Array.map (Array.filter (fun s -> s <> ""))
    |> Array.map (fun a -> (int a.[0], int a.[1]))

let firsts = ints |> Array.map fst |> Array.sort
let seconds = ints |> Array.map snd |> Array.sort

let distance (a, b) =
    a - b |> Int32.Abs

let sum = 
    Array.zip firsts seconds
    |> Array.sumBy distance