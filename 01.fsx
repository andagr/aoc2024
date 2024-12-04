open System
open System.IO

let input = File.ReadAllText("./01.txt")

let part1 =
    input
    |> _.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (
        _.Split("   ")
        >> Array.map int
        >> (fun pair -> pair[0], pair[1]))
    |> Array.unzip
    ||> (fun left right -> left |> Array.sort, right |> Array.sort)
    ||> Array.zip
    |> Array.map (fun (a, b) -> abs (a - b))
    |> Array.sum


let findOccurrences (arr: int[]) (n: int)=
    arr
    |> Array.filter ((=) n)
    |> Array.length

let part2 =
    input
    |> _.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (
        _.Split("   ")
        >> Array.map int
        >> (fun pair -> pair[0], pair[1]))
    |> Array.unzip
    ||> (fun left right -> left |> Array.map (fun l -> (l * findOccurrences right l)))
    |> Array.sum