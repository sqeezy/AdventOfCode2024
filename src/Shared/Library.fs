[<AutoOpen>]
module Utils
    let splitAt (c:char) (string : string) = string.Split(c)

    let inspect (x: 'a) = printfn "%A" x; x
