open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Core

// let input =
//     """
//     Register A: 729
//     Register B: 0
//     Register C: 0
//
//     Program: 0,1,5,4,3,0
//     """

let input =
    """
    Register A: 64584136
    Register B: 0
    Register C: 0

    Program: 2,4,1,2,7,5,1,3,4,3,5,5,0,3,3,0
    """

let registerA = Regex.Match(input, "Register A: (\d+)") |> _.Groups[1].Value |> int
let registerB = Regex.Match(input, "Register B: (\d+)") |> _.Groups[1].Value |> int
let registerC = Regex.Match(input, "Register C: (\d+)") |> _.Groups[1].Value |> int
let program = Regex.Match(input, "Program: (.+)") |> _.Groups[1].Value |> _.Split(",") |> Array.map int

type Computer = {
    Program: int array
    Pointer: int
    RegisterA: int
    RegisterB: int
    RegisterC: int
    OutRev: int list
}

let comboOperandValue operand program =
    match operand with
    | 0 | 1 | 2 | 3 -> operand
    | 4 -> program.RegisterA
    | 5 -> program.RegisterB
    | 6 -> program.RegisterC
    | _ -> failwithf $"{operand}"

let execAdv operand program =
    { program with
        RegisterA = program.RegisterA / (pown 2 (comboOperandValue operand program))
        Pointer = program.Pointer + 2 }

let execBxl operand program =
    { program with
        RegisterB = program.RegisterB ^^^ operand
        Pointer = program.Pointer + 2 }

let execBst operand program =
    { program with
        RegisterB = (comboOperandValue operand program) % 8
        Pointer = program.Pointer + 2 }

let execJnz operand program =
    if program.RegisterA = 0 then
        { program with Pointer = program.Pointer + 2 }
    else
        { program with Pointer = operand }

let execBxc _ program =
    { program with
        RegisterB = program.RegisterB ^^^ program.RegisterC
        Pointer = program.Pointer + 2 }

let execOut operand program =
    { program with
        OutRev = (comboOperandValue operand program) % 8 :: program.OutRev
        Pointer = program.Pointer + 2 }

let execBdv operand program =
    { program with
        RegisterB = program.RegisterA / (pown 2 (comboOperandValue operand program))
        Pointer = program.Pointer + 2 }

let execCdv operand program =
    { program with
        RegisterC = program.RegisterA / (pown 2 (comboOperandValue operand program))
        Pointer = program.Pointer + 2 }

let execInstruction opCode =
    match opCode with
    | 0 -> execAdv
    | 1 -> execBxl
    | 2 -> execBst
    | 3 -> execJnz
    | 4 -> execBxc
    | 5 -> execOut
    | 6 -> execBdv
    | 7 -> execCdv
    | _ -> failwithf $"{opCode}"

let init program registerA registerB registerC =
    {
        Program = program
        Pointer = 0
        RegisterA = registerA
        RegisterB = registerB
        RegisterC = registerC
        OutRev = []
    }

let rec exec computer =
    if computer.Pointer >= Array.length computer.Program - 1 then computer
    else
        let computer' = execInstruction computer.Program[computer.Pointer] computer.Program[computer.Pointer + 1] computer
        exec computer'

let computer = init program registerA registerB registerC
let haltedComputer = exec computer

let part1 =
    haltedComputer.OutRev
    |> List.rev
    |> fun out -> String.Join(',', out)