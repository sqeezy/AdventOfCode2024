let splitAt (c: char) (string: string) = string.Split(c)

let inspect (x: 'a) =
    printfn "%A" x
    x

let input = 
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}/input.txt"""