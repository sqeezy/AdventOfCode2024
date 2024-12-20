open System.Text.RegularExpressions

let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""


type Puzzle = {Grid : char [,]; Rows : int; Columns : int}

type Direction =
    | N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW

let movementForDirection =
    function
    | N -> -1, 0
    | NE -> -1, 1
    | E -> 0, 1
    | SE -> 1, 1
    | S -> 1, 0
    | SW -> 1, -1
    | W -> 0, -1
    | NW -> -1, -1

let mapOfText text =
    let lines = 
        text 
        |> splitAt '\n'
        |> Array.map (fun x -> x.Trim())

    let downDimnesion = lines.Length
    let firstLineLength = lines.[0].Length
    
    // verify quadratic shape
    [0..downDimnesion - 1]
    |> Seq.map (fun i -> lines.[i].Length)
    |> Seq.distinct
    |> Seq.iter (fun x -> if x <> firstLineLength then failwith "Not all lines are the same length")

    let grid = Array2D.init downDimnesion firstLineLength (fun i j -> lines.[i].[j])

    {Grid = grid; Rows = downDimnesion; Columns = firstLineLength}

let move (i, j) direction =
    let iChange, jChange = movementForDirection direction
    (i + iChange, j + jChange)

let getMovesUntilWallIsHit (i, j) direction (puzzle: Puzzle) =
    let head = List.singleton (i, j)

    let tail =
        List.unfold
            (fun (i, j) ->
                let i', j' = move (i, j) direction
                if i' < 0 || i' >= puzzle.Rows || j' < 0 || j' >= puzzle.Columns then
                    None
                else
                    Some ((i', j'), (i', j')))
            (i, j)

    List.append head tail

let gridDimensions (grid: Puzzle) =
    let iMax = grid.Rows
    let jMax = grid.Columns
    (iMax, jMax)

let squarePositionsWithX puzzle =

    let iMax, jMax = puzzle |> gridDimensions
    seq {
        for i in [0..iMax-1] do
            for j in [0..jMax-1] do
                (i,j) |> inspect |> ignore
                if puzzle.Grid.[i, j] = 'X' then
                    yield i, j
    }
    |> List.ofSeq



let getMovesUntilWallIsHitInAllDirections (i, j) puzzle =
    [ N; NE; E; SE; S; SW; W; NW ]
    |> List.map (fun direction -> getMovesUntilWallIsHit (i, j) direction puzzle)

let allPathsThroughPuzzle puzzle =
    puzzle
    |> squarePositionsWithX
    |> List.map (fun pos -> getMovesUntilWallIsHitInAllDirections pos puzzle)
    |> List.collect id
    |> List.distinct

let getStringFromPathThroughMap puzzle (path : seq<(int * int)>) =
    let chars =
        path
        |> Seq.map (fun (i, j) -> puzzle.Grid.[i, j])
        |> Seq.toArray
    System.String(chars)

let numberOfXmas (text: string) =
    let matches = Regex.Matches(text, "^XMAS")
    (matches, text) |> inspect |> ignore
    matches.Count

numberOfXmas "XMASSSSXMASXXXXXMAS"

let partOne map =
    let getString = getStringFromPathThroughMap map
    allPathsThroughPuzzle map 
    |> List.map (getString) 
    |> List.map numberOfXmas
    |> List.sum

let example =
    @"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

let tinyExample =
    @"..X...
.SAMX.
.A..A.
XMAS.S
.X...."

let exampleMap = mapOfText example
let tinyMap = mapOfText tinyExample
let inputMap = mapOfText input


partOne tinyMap
partOne exampleMap
partOne inputMap