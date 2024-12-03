open System
open System.IO

let input = File.ReadAllText("./01a.txt")

input
|> _.Split("\n", StringSplitOptions.RemoveEmptyEntries)
|> Array.map (
    _.Split("   ")
    >> Array.map int
    >> (fun pair -> pair[0], pair[1]))
|> Array.unzip
|> (fun both -> both |> fst |> Array.sort, both |> snd |> Array.sort)
||> Array.zip
|> Array.map (fun (a, b) -> abs (a - b))
|> Array.sum