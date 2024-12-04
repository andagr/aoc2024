open System
open System.IO

let input = File.ReadAllText("./01.txt")

let findOccurrences (arr: int[]) (n: int)=
    arr
    |> Array.filter ((=) n)
    |> Array.length

input
|> _.Split("\n", StringSplitOptions.RemoveEmptyEntries)
|> Array.map (
    _.Split("   ")
    >> Array.map int
    >> (fun pair -> pair[0], pair[1]))
|> Array.unzip
||> (fun left right -> left |> Array.map (fun l -> (l * findOccurrences right l)))
|> Array.sum