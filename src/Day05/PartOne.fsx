open System.Text.RegularExpressions

let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let rulesText = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/rules.txt"""
let updatesText = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}/updates.txt"""

