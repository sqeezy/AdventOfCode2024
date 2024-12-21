let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""

type Equation =
    { TestResult: int64 
      Nums: int64 list }

type Operators = 
    | Add
    | Multiply
    | Concat

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
        | Concat -> acc <- System.Int64.Parse(string acc + string equation.Nums.[i])
    acc

let testEquation operators equation =
    let operatorCount = equation.Nums.Length - 1
    let variations = variation operatorCount operators
    variations
    |> List.map (calculate equation)
    |> List.exists (fun x -> x = equation.TestResult)

let calibrate operators text =
    input 
    |> parseInput 
    |> Array.filter (testEquation operators) 
    |> Array.map (fun x -> x.TestResult) 
    |> Array.sum

let partOneOperators = [Add; Multiply]
let partTwoOperators = [Add; Multiply; Concat]

calculate { TestResult = 190; Nums = [10; 19] } [Add]
calculate { TestResult = 190; Nums = [10; 10; 10] } [Multiply; Add]
calculate { TestResult = 190; Nums = [10; 10; 10] } [Concat; Add]

testEquation partOneOperators { TestResult = 190; Nums = [10; 19] }
testEquation partOneOperators { TestResult = 190; Nums = [10; 10; 10] }

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

let partOne = calibrate partOneOperators input
let partTwo = calibrate partTwoOperators input