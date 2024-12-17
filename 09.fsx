open System.Collections.Generic
open System.IO

let input = File.ReadAllText("./09.txt") |> _.Trim()

let block pos nbr =
    Array.init nbr (fun _ -> if pos % 2 = 0 then Some (pos / 2) else None)

let reorg disk =
    let blockStack =
        disk
        |> Array.choose id
        |> Stack

    [| for i in 0 .. blockStack.Count - 1 ->
         match disk[i] with
         | Some b -> b
         | None -> blockStack.Pop() |]

let disk =
    input
    |> _.ToCharArray()
    |> Array.map (string >> int)
    |> Array.mapi block
    |> Array.concat
    |> Array.map (Option.map int64)

let part1 =
    disk
    |> reorg
    |> Array.mapi (fun i b -> int64 i * b)
    |> Array.sum