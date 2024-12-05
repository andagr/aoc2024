open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllText("./03.txt")

// Part 1

let part1 =
    Regex.Matches(input, "mul\((\d+),(\d+)\)")
    |> List.ofSeq
    |> List.map (fun m -> int m.Groups[1].Value * int m.Groups[2].Value)
    |> List.sum

// Part 2

type Instruction =
    | Do
    | Dont
    | Mul of int * int

let parse instruction =
    match instruction with
    | "do()" -> Do
    | "don't()" -> Dont
    | _ when instruction.StartsWith("mul") ->
        let values = Regex.Match(instruction, "\((\d+),(\d+)\)")
        Mul ((int values.Groups[1].Value), (int values.Groups[2].Value))
    | _ -> failwith instruction

let part2 =
    Regex.Matches(input, "mul\(\d+,\d+\)|do\(\)|don't\(\)")
    |> List.ofSeq
    |> List.map (_.ToString() >> parse)
    |> List.fold
        (fun (currInstruction, sumVal) newInstruction ->
            match newInstruction with
            | Dont
            | Do -> (newInstruction, sumVal)
            | Mul (left, right) ->
                if currInstruction = Do then (currInstruction, sumVal + left * right)
                else (currInstruction, sumVal))
        (Do, 0)
    |> snd
