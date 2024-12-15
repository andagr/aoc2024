open System.IO

let input = File.ReadAllLines("./07.txt")

type Equation = {
    TestValue: int64
    Numbers: int64 list
}

let parseEquations (input: string) =
    {
        TestValue =
            input
            |> _.Split(':')
            |> Array.head
            |> int64
        Numbers =
            input
            |> _.Split(' ')
            |> Array.tail
            |> Array.map int64
            |> Array.toList
    }

let equations = input |> Array.map parseEquations |> Array.toList

let add value a = Some (value |> Option.defaultValue 0L |> (fun v -> v + a))
let multiply value a = Some (value |> Option.defaultValue 1L |> (fun v -> v * a))

let isSolvable operators equation =
    let rec isSolvable' numbers value =
        if value |> Option.map(fun v -> v > equation.TestValue) |> Option.defaultValue false then false
        else
            match numbers with
            | [] -> value |> Option.map (fun v -> v = equation.TestValue) |> Option.defaultValue false
            | a::t ->
                operators
                |> Seq.map (fun operator -> operator value a)
                |> Seq.tryFind (isSolvable' t)
                |> Option.isSome
    isSolvable' equation.Numbers None

let part1 =
    equations
    |> List.filter (isSolvable [add; multiply])
    |> List.sumBy _.TestValue

let concatenate value a =
    Some (value |> Option.map string |> Option.defaultValue "" |> (fun v -> v + string a) |> int64)

let part2 =
    equations
    |> List.filter (isSolvable [concatenate; add; multiply])
    |> List.sumBy _.TestValue
