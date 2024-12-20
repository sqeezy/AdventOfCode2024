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

type State = { Grid : Map<(int * int),CellType>
               Direction : Direction
               Position : (int * int)
               PastVisits : ((int * int) * Direction) list 
               BlockedCell : (int * int) option
               Looped : bool}

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
    
    {Grid = grid; Position=start; Direction = N; PastVisits = [(start, N)]; BlockedCell = None; Looped = false}


let rec iterate state =
    let nextPositionCandidate =
        let x, y = state.Position
        let dx, dy = movementForDirection state.Direction
        x + dx, y + dy

    let nextCellType =
        state.Grid.TryFind nextPositionCandidate

    let updatedGrid = state.Grid |> Map.add state.Position Visited
    let updatedPastVisits = (state.Position, state.Direction) :: state.PastVisits

    match nextCellType with
    | None -> {state with Grid = updatedGrid}
    | Some Blocked -> iterate {state with Direction = turnRight state.Direction; Grid = updatedGrid; PastVisits = updatedPastVisits}
    | _ -> 
        let loopCandidate = (nextPositionCandidate, state.Direction)
        let willLoop = state.PastVisits |> List.contains loopCandidate
        if willLoop 
        then 
            {state with Looped = true; Grid = updatedGrid}
        else
            iterate {state with Position = nextPositionCandidate; Grid = updatedGrid; PastVisits = updatedPastVisits}

let alterByBlockingCell state (x, y) =
    {state with Grid = state.Grid |> Map.add (x, y) Blocked; BlockedCell = Some (x, y)}

let getPosiblePermutationsOfBlockedFreeCells state =
    state.Grid
    |> Seq.filter (fun kvp -> kvp.Value = Free)
    |> Seq.map (fun kvp -> alterByBlockingCell state kvp.Key)

let inputMap = parseMap input

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let partTwo =
    inputMap
    |> getPosiblePermutationsOfBlockedFreeCells
    |> Array.ofSeq
    |> Array.Parallel.mapi (fun num state -> num |> inspect |> ignore; iterate state)
    |> Seq.filter (fun x -> x.Looped)
    |> Seq.length

stopWatch.Stop()
printfn "Elapsed Seconds: %f %i" stopWatch.Elapsed.TotalSeconds partTwo