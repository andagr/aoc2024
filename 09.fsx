open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Core
let input = File.ReadAllText("./09.txt") |> _.Trim()

let block pos nbr =
    Array.init nbr (fun _ -> if pos % 2 = 0 then Some (pos / 2) else None)

let reorgBlocks disk =
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
    |> reorgBlocks
    |> Array.mapi (fun i b -> int64 i * b)
    |> Array.sum

let findRightMostFileId (disk: int64 option array) =
    disk |> Array.rev |> Array.skipWhile Option.isNone |> Array.head |> Option.get

let fileLength (disk: int64 option array) fileId fileIx =
    disk[fileIx..] |> Array.takeWhile (fun b -> b = Some fileId) |> Array.length

let fileIx (disk: int64 option array) fileId =
    disk |> Array.tryFindIndex (fun b -> b = Some fileId)

let findFreeSpaceToLeftIx (disk: int64 option array) (fileIx: int) (fileLength: int)=
    let rec findFreeSpaceIx' start =
        if start + fileLength - 1 >= fileIx then None
        elif disk[start .. start + fileLength - 1] |> Array.forall Option.isNone then Some start
        else findFreeSpaceIx' (start + 1)
    findFreeSpaceIx' 0

let removeFile (disk: int64 option array) (fileIx: int) (fileLength: int) =
    let file = disk[fileIx .. fileIx + fileLength - 1]
    let disk =
        disk
        |> Array.removeManyAt fileIx (Array.length file)
        |> Array.insertManyAt fileIx (Array.replicate fileLength None)
    (disk, file)

let writeFile (disk: int64 option array) (file: int64 option array) (freeSpaceIx: int) =
    disk
    |> Array.removeManyAt freeSpaceIx (Array.length file)
    |> Array.insertManyAt freeSpaceIx file

let reorgFiles (disk: int64 option array) =
    let rec reorgFiles' disk fileId =
        if fileId % 100L = 0L then printfn $"fileId: {fileId}" else ()
        if fileId < 0L then disk
        else
            match fileIx disk fileId with
            | None -> reorgFiles' disk (fileId - 1L)
            | Some fileIx ->
                let fileLength = fileLength disk fileId fileIx
                match findFreeSpaceToLeftIx disk fileIx fileLength with
                | None -> reorgFiles' disk (fileId - 1L)
                | Some freeSpaceIx ->
                    let disk, file = removeFile disk fileIx fileLength
                    let disk = writeFile disk file freeSpaceIx
                    reorgFiles' disk (fileId - 1L)
    let startFileId = findRightMostFileId disk
    reorgFiles' disk startFileId

let part2 =
    disk
    |> reorgFiles
    |> Array.mapi (fun i b ->
        match b with
        | None -> 0L
        | Some b' -> (int64 i) * b')
    |> Array.sum
