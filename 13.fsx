open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllText("./13.txt") |> _.Split("\n\n")

type Game = {
    Xa: decimal
    Xb: decimal
    X: decimal
    Ya: decimal
    Yb: decimal
    Y: decimal
}

let games =
    input
    |> List.ofArray
    |> List.map (fun ig ->
        let buttonAMatch = Regex.Match(ig, "Button A: X\+(\d+), Y\+(\d+)")
        let buttonBMatch = Regex.Match(ig, "Button B: X\+(\d+), Y\+(\d+)")
        let prizeMatch = Regex.Match(ig, "Prize: X=(\d+), Y=(\d+)")
        {
            Xa = decimal buttonAMatch.Groups[1].Value
            Ya = decimal buttonAMatch.Groups[2].Value
            Xb = decimal buttonBMatch.Groups[1].Value
            Yb = decimal buttonBMatch.Groups[2].Value
            X = decimal prizeMatch.Groups[1].Value
            Y = decimal prizeMatch.Groups[2].Value
        })

let cost game =
    let Xa, Xb, Ya, Yb, X, Y = game.Xa, game.Xb, game.Ya, game.Yb, game.X, game.Y
    let b = (Xa * Y - Ya * X) / (Xa * Yb - Ya * Xb)
    let a = (X - Xb * b) / Xa
    let count = a * 3M + b
    if floor count = count then Some (int64 count)
    else None

let part1 =
    games
    |> List.map cost
    |> List.choose id
    |> List.sum
