open System.IO

let input = File.ReadAllLines("./02.txt")

type LevelDelta =
    | Increasing
    | Decreasing
    | Unsafe

let getLevelDeltas report =
    report
    |> Array.pairwise
    |> Array.map (fun (left, right) ->
        match right - left with
        | d when d >= -3 && d <= -1 -> Decreasing
        | d when d >= 1 && d <= 3 -> Increasing
        | _ -> Unsafe)

let isSafe report =
    getLevelDeltas report
    |> (fun levelDeltas ->
        match Array.distinct levelDeltas with
        | [|Increasing|] -> true
        | [|Decreasing|] -> true
        | _ -> false)

// Part 1

let part1 =
    input
    |> Array.map (_.Split(" ") >> Array.map int)
    |> Array.filter isSafe
    |> Array.length


// Part 2

let dampen report =
    [|0..Array.length report - 1|]
    |> Array.map (fun i -> report |> Array.removeAt i)
    |> Array.append [|report|]

let part2 =
    input
    |> Array.map (_.Split(" ") >> Array.map int)
    |> Array.filter (dampen >> (Array.exists isSafe))
    |> Array.length
