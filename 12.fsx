open System
open System.IO

let input = File.ReadAllLines("./12.txt") |> Array.map _.ToCharArray()
// let input =
//     """
//     RRRRIICCFF
//     RRRRIICCCF
//     VVRRRCCFFF
//     VVRCCCJFFF
//     VVVVCJJCFE
//     VVIVCCJJEE
//     VVIIICJJEE
//     MIIIIIJJEE
//     MIIISIJEEE
//     MMMISSJEEE
//     """
//     |> _.Split("\n", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
//     |> Array.map _.ToCharArray()

type Region = {
    Plant: char
    Fences: int
    Area: (int * int) list
}

let fences (ri, ci) (plots: char array array) =
    let plant = plots[ri][ci]
    let fn =
        [|
            if ri >= 1 && plots[ri - 1][ci] = plant then yield 1
            if ci <= Array.length plots[ri] - 2 && plots[ri][ci + 1] = plant then yield 1
            if ri <= Array.length plots - 2 && plots[ri + 1][ci] = plant then yield 1
            if ci >= 1 && plots[ri][ci - 1] = plant then yield 1
        |]
        |> Array.length
    4 - fn

let isPartOfArea (ri, ci) (area: (int * int) list) =
    [
       (ri - 1, ci)
       (ri, ci - 1)
       (ri, ci + 1)
       (ri + 1, ci)
    ]
    |> List.exists (fun p -> area |> List.exists (fun a -> p = a))

let isPartOfRegion (ri, ci) (plant: char) (region: Region) =
    plant = region.Plant && isPartOfArea (ri, ci) region.Area

let addOrUpdateRegions (ri, ci) (plant: char) (fences: int) (regions: Region list) =
    let matchingRegions, otherRegions =
        regions
        |> List.partition (isPartOfRegion (ri, ci) plant)
    matchingRegions
        |> List.fold
            (fun mergedRegionBuilder matchingRegion ->
                match mergedRegionBuilder with
                | None -> Some { Plant = matchingRegion.Plant; Fences = matchingRegion.Fences; Area = matchingRegion.Area }
                | Some mergedRegion -> Some { Plant = matchingRegion.Plant; Fences = mergedRegion.Fences + matchingRegion.Fences; Area = matchingRegion.Area @ mergedRegion.Area })
            None
        |> Option.map (fun mergedRegion ->
            let updatedRegion = { mergedRegion with Fences = mergedRegion.Fences + fences; Area = (ri, ci) :: mergedRegion.Area }
            updatedRegion :: otherRegions)
        |> Option.defaultWith (fun () ->
               let updatedRegion = { Plant = plant; Fences = fences; Area = [(ri, ci)] }
               updatedRegion :: otherRegions)


let tryNextPlotIx (ri, ci) (plots: char array array) =
    if ci < Array.length plots[ri] - 1 then Some (ri, ci + 1)
    elif ri < Array.length plots - 1 then Some (ri + 1, 0)
    else None

let plotRegions (plots: char array array) =
    let rec plotRegions' (ri, ci) (regions: Region list) =
        let plant = plots[ri][ci]
        let fences = fences (ri, ci) plots
        let regions = regions |> addOrUpdateRegions (ri, ci) plant fences
        match tryNextPlotIx (ri, ci) plots with
        | None -> regions
        | Some (ri, ci) -> plotRegions' (ri, ci) regions
    plotRegions' (0, 0) []

let regions = plotRegions input

let part1 =
    regions
    |> List.sumBy (fun r -> r.Fences * List.length r.Area)

