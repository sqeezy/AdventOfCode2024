let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""


type Puzzle = {Grid : char [,]; Rows : int; Columns : int}

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

let gridDimensions (grid: Puzzle) =
    let iMax = grid.Rows
    let jMax = grid.Columns
    (iMax, jMax)

let squarePositionsWithAInside puzzle =

    let iMax, jMax = puzzle |> gridDimensions
    seq {
        for i in [1..iMax-2] do
            for j in [1..jMax-2] do
                if puzzle.Grid.[i, j] = 'A' then
                    yield i, j
    }
    |> List.ofSeq

let isXmas (i, j) puzzle =
    let grid = puzzle.Grid
    let leftTopAndBottomRight = [grid.[i-1, j-1]; grid.[i+1, j+1]]
    let leftBottomAndTopRight = [grid.[i+1, j-1]; grid.[i-1, j+1]]
    let completesMas pair = List.sort pair = ['M'; 'S']

    leftTopAndBottomRight |> completesMas && leftBottomAndTopRight |> completesMas

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
    @"M.S
.A.
M.S"

let exampleMap = mapOfText example
let tinyMap = mapOfText tinyExample
let inputMap = mapOfText input

let partTwo map =
    squarePositionsWithAInside map
    |> List.filter (fun x -> isXmas x map)
    |> List.length

partTwo tinyMap
partTwo exampleMap
partTwo inputMap