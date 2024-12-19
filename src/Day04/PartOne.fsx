open System.Text.RegularExpressions

let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let input = 
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""

let myExample = @"MSAMX
AMXSX
MSAMA
XMASA"

let example = @"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"


let mapOfText text = 
    let lines =
        text
        |> splitAt '\n'
    seq {
        // for index line and index char in line
        for i in 0..lines.Length-1 do
            for j in 0..lines.[i].Length-1 do
                yield (i,j), lines.[i].[j]
    } |> Map.ofSeq

mapOfText example
mapOfText myExample

type Direction = 
    | N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW

let movementForDirection = function
    | N -> (-1,0)
    | NE -> (-1,1)
    | E -> (0,1)
    | SE -> (1,1)
    | S -> (1,0)
    | SW -> (1,-1)
    | W -> (0,-1)
    | NW -> (-1,-1)

let move (i,j) direction =
    let iChange, jChange = movementForDirection direction
    (i+iChange, j+jChange)

let getMovesUntilWallIsHit (i,j) direction (map : Map<(int*int), char>) =
    let movement = movementForDirection direction 
    Seq.unfold 
        (fun (i,j) ->
            let i', j' = move (i,j) direction
            if map.ContainsKey (i',j') then
                Some((i',j'), (i',j'))
            else
                None
        )
        (i,j)

