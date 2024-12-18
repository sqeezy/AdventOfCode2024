open System.Text.RegularExpressions

let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let input = 
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/input.txt"""

let example = @"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let matches text=
    seq {
        for m in Regex.Matches(text, @"mul\((\d{1,3}),(\d{1,3})\)") do
            yield m.Groups.[1].Value |> int, m.Groups.[2].Value |> int
    }

let sumOfMultiples matches =
    matches
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.sum

matches example |> sumOfMultiples

matches input |> sumOfMultiples
