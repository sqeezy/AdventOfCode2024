open System.Text.RegularExpressions

let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let input = 
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""

let example = @"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

type Operation = 
    | Mul of int * int
    | Dont
    | Do

let (|StartsWith|_|) (prefix: string) (text: string) =
    if text.StartsWith(prefix) then
        Some(text.Substring(prefix.Length))
    else
        None

let matches text=
    seq {
        for m in Regex.Matches(text, @"mul\((\d{1,3}),(\d{1,3})\)|(don't\(\))|(do\(\))") do
            
            match m.Value with
            | StartsWith "mul(" rest -> 
                let parts = rest.Split([|',';')'|])
                yield Mul(int parts.[0], int parts.[1])
            | StartsWith "don't()" _ -> yield Dont
            | StartsWith "do()" _ -> yield Do
            | _ -> ()
    }

let sumOfMultiples matches =
    matches
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.sum


Seq.fold 
    (fun (sumOfMul, isActive) op ->
        match op with
        | Mul(a, b) when isActive -> (sumOfMul + a * b, isActive)
        | Mul(a, b) -> (sumOfMul, isActive)
        | Dont -> (sumOfMul, false)
        | Do -> (sumOfMul, true)
    )
    (0,true)
    (matches input)
