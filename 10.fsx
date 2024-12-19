open System
open System.IO

let input = File.ReadAllLines("./10.txt")

let map =
    input
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (fun c -> if Char.IsNumber c then c |> string |> int else -1))

let isTrailStart (map: int array array) (ri, ci) =
    map[ri][ci] = 0

let isTrailEnd (map: int array array) (ri, ci) =
    map[ri][ci] = 9

let trailheads map =
    [| for ri in 0 .. Array.length map - 1 do
        for ci in 0 .. Array.length map[ri] - 1 do
            if (ri, ci) |> isTrailStart map then yield (ri, ci) |]

let nextSteps (map: int array array) (ri, ci) =
    let currentHeight = map[ri][ci]
    [|
        if ri >= 1 && map[ri - 1][ci] - currentHeight = 1 then                        yield (ri - 1, ci)
        if ci <= Array.length map[ri] - 2 && map[ri][ci + 1] - currentHeight = 1 then yield (ri, ci + 1)
        if ri <= Array.length map - 2 && map[ri + 1][ci] - currentHeight = 1 then     yield (ri + 1, ci)
        if ci >= 1 && map[ri][ci - 1] - currentHeight = 1 then                        yield (ri, ci - 1)
    |]

let trails (map: int array array) (thRi, thCi) =
    let rec trails' path (ri, ci) =
        let path = (ri, ci) :: path
        if (ri, ci) |> isTrailEnd map then [| path |> List.rev |> Array.ofList |]
        else
            (ri, ci)
            |> nextSteps map
            |> Array.collect (trails' path)
    trails' [] (thRi, thCi)

let part1 =
    map
    |> trailheads
    |> Array.collect (trails map)
    |> Array.groupBy Array.head
    |> Array.map (fun (th, trails) -> (th, trails |> Array.map Array.last |> Array.distinct |> Array.length))
    |> Array.sumBy snd

let part2 =
    map
    |> trailheads
    |> Array.collect (trails map)
    |> Array.groupBy Array.head
    |> Array.map (fun (th, trails) -> Array.length trails)
    |> Array.sum
