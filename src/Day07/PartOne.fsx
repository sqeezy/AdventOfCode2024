let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""

type Equation =
    { TestResult: int64 
      Nums: int64 list }

type Operators = 
    | Add
    | Multiply

let parseEquation (input: string) =
    let parts = 
        input.Split([|':';' '|])
        |> Array.map (fun x -> x.Trim())
        |> Array.filter (fun x -> x <> "")
    let testResult = int64 parts.[0]
    let nums = parts |> Array.skip 1 |> Array.map int64 |> Array.toList
    { TestResult = testResult; Nums = nums }

let parseInput (input: string) =
    input
    |> splitAt '\n'
    |> Array.map parseEquation

let variation operatorCount possibleOperators =
    let rec loop acc remainingOperators =
        match remainingOperators with
        |0 -> [ List.rev acc ]
        |_ -> 
            [
                for item in possibleOperators do
                    yield! loop (item::acc) (remainingOperators - 1)
            ]
            
    loop [] operatorCount        

let calculate (equation: Equation) (operators: Operators list) =
    let mutable acc = equation.Nums.[0]
    for i in 1..equation.Nums.Length - 1 do
        match operators.[i - 1] with
        | Add -> acc <- acc + equation.Nums.[i]
        | Multiply -> acc <- acc * equation.Nums.[i]
    acc

let testEquation equation =
    equation |> inspect |> ignore
    let possibleOperators = [Add; Multiply]
    let operatorCount = equation.Nums.Length - 1
    let variations = variation operatorCount possibleOperators
    variations
    |> List.map (calculate equation)
    |> List.exists (fun x -> x = equation.TestResult)


calculate { TestResult = 190; Nums = [10; 19] } [Add]
calculate { TestResult = 190; Nums = [10; 10; 10] } [Multiply; Add]

testEquation { TestResult = 190; Nums = [10; 19] }
testEquation { TestResult = 190; Nums = [10; 10; 10] }

let example = 
    @"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

let exampleParsed = parseInput example

input |> parseInput |> Array.filter testEquation |> Array.map (fun x -> x.TestResult) |> Array.sum