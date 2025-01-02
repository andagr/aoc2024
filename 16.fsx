open System
open System.IO
open Microsoft.FSharp.Core

// let input =
//     """
//     #####
//     #SE#
//     #####
//     """
//     |> _.Split("\n", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
//     |> Array.map _.ToCharArray()

// let input =
//     """
//     ###############
//     #.......#....E#
//     #.#.###.#.###.#
//     #.....#.#...#.#
//     #.###.#####.#.#
//     #.#.#.......#.#
//     #.#.#####.###.#
//     #...........#.#
//     ###.#.#####.#.#
//     #...#.....#.#.#
//     #.#.#.###.#.#.#
//     #.....#...#.#.#
//     #.###.#.#.#.#.#
//     #S..#.....#...#
//     ###############
//     """
//     |> _.Split("\n", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
//     |> Array.map _.ToCharArray()

let input = File.ReadAllLines("./16.txt") |> Array.map _.ToCharArray()

type Facing =
    | North
    | East
    | South
    | West

type VertexId = {
    Facing: Facing
    Pos: int * int
}

type Edge = {
    Dist: int
    Target: VertexId
}

type Vertex = {
    VertexId: VertexId
    Value: char
    Cost: int
    Visited: bool
    PrevVerticesPos: (int * int) Set
}

let (|Start|Middle|End|Wall|) c =
    match c with
    | 'S' -> Start
    | '.' -> Middle
    | 'E' -> End
    | '#' -> Wall
    | _ -> failwithf $"{c}"

let isVertex c =
    match c with
    | Start | Middle | End -> true
    | _ -> false

let getStartVertex input =
    let startY =
        input
        |> Array.findIndex (fun row ->
            row |> Array.exists (fun c ->
                match c with
                | Start -> true
                | _ -> false))
    let startX =
        input[startY]
        |> Array.findIndex (fun c ->
            match c with
            | Start -> true
            | _ -> false)
    {
        VertexId = { Facing = East; Pos = startY, startX }
        Cost = 0
        Value = input[startY][startX]
        Visited = false
        PrevVerticesPos = Set.empty
    }

let getVertex (input: char array array) { Facing = facing; Pos = (y, x) } =
    {
        VertexId = { Facing = facing; Pos = y, x }
        Cost = Int32.MaxValue
        Value = input[y][x]
        Visited = false
        PrevVerticesPos = Set.empty
    }

let findEdges (input: char array array) { Facing = facing; Pos = (y, x) } =
    [
        if isVertex (input[(y - 1)][x]) then // North
            if facing = North then { Dist = 1; Target = { Facing = North; Pos = y - 1, x } }
            if facing = West || facing = East then { Dist = 1001; Target = { Facing = North; Pos = y - 1, x } }
        if isVertex (input[y][(x + 1)]) then // East
            if facing = East then { Dist = 1; Target = { Facing = East; Pos = y, x + 1 } }
            if facing = North || facing = South then { Dist = 1001; Target = { Facing = East; Pos = y, x + 1 } }
        if isVertex (input[(y + 1)][x]) then // South
            if facing = South then { Dist = 1; Target = { Facing = South; Pos = y + 1, x } }
            if facing = East || facing = West then { Dist = 1001; Target = { Facing = South; Pos = y + 1, x } }
        if isVertex (input[y][(x - 1)]) then // West
            if facing = West then { Dist = 1; Target = { Facing = West; Pos = y, x - 1 } }
            if facing = South || facing = North then { Dist = 1001; Target = { Facing = West; Pos = y, x - 1 } }
    ]

let visit input =
    let updateCost source edge target =
        let newCost = (source.Cost + edge.Dist)
        { target with
            Cost = min target.Cost newCost
            PrevVerticesPos = target.PrevVerticesPos |> Set.union (source.PrevVerticesPos |> Set.add source.VertexId.Pos) }
    let rec visitAll' vertices source =
        let targets =
            findEdges input source.VertexId
            |> List.choose (fun e ->
                match vertices |> Map.tryFind e.Target with
                | Some tv when tv.Visited -> None
                | Some tv -> Some (updateCost source e tv)
                | None ->
                    let tv = getVertex input e.Target
                    Some (updateCost source e tv))
        let vertices =
            targets
            |> List.fold
                (fun vertices tv -> vertices |> Map.add tv.VertexId tv)
                vertices
            |> fun vertices -> vertices |> Map.add source.VertexId { source with Visited = true }
        match vertices |> Map.values |> Seq.filter (_.Visited >> not) |> Seq.sortBy _.Cost |> Seq.tryHead with
        | None -> vertices
        | Some next -> visitAll' vertices next
    let startVertex = getStartVertex input
    visitAll' (Map.ofList [(startVertex.VertexId, startVertex)]) startVertex
    |> Map.values
    |> List.ofSeq


let vertices = visit input
List.length vertices

let endVertex =
    vertices
    |> List.find (fun v ->
        match v.Value with
        | End -> true
        | _ -> false)

let part1 = endVertex.Cost

let part2 =
    endVertex.PrevVerticesPos
    |> Set.add endVertex.VertexId.Pos
    |> Set.count