let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""

let example =
    @"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

type Direction =
    | N
    | E
    | S
    | W

let movementForDirection =
    function
    | N -> -1, 0
    | E -> 0, 1
    | S -> 1, 0
    | W -> 0, -1

let turnRight =
    function
    | N -> E
    | E -> S
    | S -> W
    | W -> N

type CellType =
    | Free
    | Blocked
    | Visited

type State = {Grid : Map<(int * int),CellType>
              Direction : Direction
              Position : (int * int)
              Height : int
              Width : int}

let parseMap text =
    let lines = 
        text 
        |> splitAt '\n'
        |> Array.map (fun x -> x.Trim())

    let height = lines.Length
    let width = lines.[0].Length
    let mutable start = (-1,-1)
    
    let grid =
        seq {
            for i in [0..height - 1] do
                for j in [0..width - 1] do
                    let value=
                        match lines.[i].[j] with
                        | '.' -> Free
                        | '#' -> Blocked
                        | '^' -> 
                            start <- (i,j)
                            Visited
                        | _ -> failwith "Invalid character in map"
                    yield (i, j), value
        } |> Map.ofSeq
    
    {Grid = grid; Position=start; Direction = N; Height = height; Width = width}


let rec iterate state =
    let nextPositionCandidate =
        let x, y = state.Position
        let dx, dy = movementForDirection state.Direction
        x + dx, y + dy

    let nextCellType =
        state.Grid.TryFind nextPositionCandidate

    match nextCellType with
    | None -> {state with Grid = state.Grid |> Map.add state.Position Visited}
    | Some Blocked -> iterate {state with Direction = turnRight state.Direction}
    | _ -> iterate {state with Position = nextPositionCandidate; Grid = state.Grid |> Map.add state.Position Visited}

let countVisitedCells state =
    state.Grid.Values |> Seq.filter (fun x -> x = Visited) |> Seq.length

let exampleMap = parseMap example
let exampleRun = exampleMap |> iterate
exampleRun.Grid.[9,7]
exampleRun |> countVisitedCells

let partOne = input |> parseMap |> iterate |> countVisitedCells