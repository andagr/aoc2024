open System.IO

let input = File.ReadAllLines("./05.txt")

let pageSeqs =
    input
    |> Array.filter _.Contains(",")
    |> Array.map _.Split(",")
    |> Array.map (fun ps -> ps |> Array.map int)

let inputOrderingRules =
    input
    |> Array.filter _.Contains("|")
    |> Array.map _.Split("|")
    |> Array.map (fun [| a; b |] -> (int a, int b))

let middlePage (pages: int array) =
    pages[((pages |> Array.length) - 1) / 2]

let isCorrectlyOrdered pageSeq =
    pageSeq
    |> Array.pairwise
    |> Array.forall (fun (a, b) -> inputOrderingRules |> Array.contains (a, b))

pageSeqs
|> Array.filter isCorrectlyOrdered
|> Array.sumBy middlePage