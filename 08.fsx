open System.IO
open Microsoft.FSharp.Core

let input = File.ReadAllLines("./08.txt") |> Array.map _.ToCharArray()

type Antenna = {
    Frequency: char
    Pos: int * int
}

let parse input =
    input
    |> Array.mapi (fun ri row ->
        row
        |> Array.mapi (fun ci col -> { Frequency = col; Pos = (ri, ci) })
        |> Array.filter (fun a -> a.Frequency <> '#' && a.Frequency <> '.'))
    |> Array.concat
    |> List.ofArray

let findAntennaSiblings antennas antenna =
    antennas |> List.filter (fun a' -> a'.Frequency = antenna.Frequency && a' <> antenna)

let getAntinodeFor a b =
    let dRow, dCol = (fst b - fst a, snd b - snd a)
    (fst b + dRow, snd b + dCol)

let findAntinodes boundRow boundCol antennas =
    antennas
    |> List.collect (fun a -> (findAntennaSiblings antennas a) |> List.map _.Pos |> List.map (getAntinodeFor a.Pos))
    |> List.filter (fun (x, y) -> x >= 0 && x <= boundRow && y >= 0 && y <= boundCol)

let antennas = input |> parse
let antinodes =
    antennas
    |> (findAntinodes (Array.length input - 1) (Array.length input[0] - 1))
    |> List.distinct

let part1 = antinodes |> List.length

let findResonantHarmonics boundRow boundCol a b =
    [b.Pos]
    |> List.append
        (List.unfold
            (fun (a, b) ->
                let x, y = getAntinodeFor a b
                if x >= 0 && x <= boundRow && y >= 0 && y <= boundCol then Some ((x, y), (b, (x, y)))
                else None)
            (a.Pos, b.Pos))

let part2 =
    antennas
    |> List.collect (fun a ->
        findAntennaSiblings antennas a
        |> List.collect (fun b -> findResonantHarmonics (Array.length input - 1) (Array.length input[0] - 1) a b))
    |> List.distinct
    |> List.length