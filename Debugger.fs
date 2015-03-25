module Debugger

open Mmu
open Cpu
open Instruction
open BitLogic
open Register
open Clock
open System.IO
open System.Text.RegularExpressions

let parseAddress str = System.UInt16.Parse(str,System.Globalization.NumberStyles.HexNumber)

type Breakpoint(address) =
    member this.Address = address
    member val Active = true with get, set

type Symbol(address: uint16,name) =
    member this.Address = address
    member this.Name = name

type MapInfo (symbols) =

    member this.SymbolByName name = symbols |> Map.tryPick (fun _ (symbol: Symbol) -> if symbol.Name = name then Some symbol else None)

    member this.SymbolByAddress address = symbols |> Map.tryFind address

    new () = MapInfo(Map.empty)

    new (path: string) =
        let groups = Regex.Matches(File.ReadAllText path ,@"((\S+)\s+([0-9A-F]+))",RegexOptions.Multiline)

        let symbols = 
                groups |> Seq.cast<Match>
                |> Seq.map (fun m -> m.Groups.Item(2).Value, m.Groups.Item(3).Value)
                |> Seq.map (fun (name, address) ->
                    parseAddress address, Symbol(parseAddress address,name))
                |> Map.ofSeq
       
        MapInfo(symbols)
    

type Debugger(cpu: CPU, mmu: MMU, systemClock: Clock, mapInfo: MapInfo) as this =

    let mutable breakpoints = Map.empty<uint16,Breakpoint>

    let mutable stepping = false

    let mutable previousPC = 0us
    
    let registers = cpu.Registers

    let stopWatch = System.Diagnostics.Stopwatch()

    let A = registers.A
    let B = registers.B
    let C = registers.C
    let D = registers.D
    let E = registers.E
    let F = registers.F
    let H = registers.H
    let L = registers.L
    let AF = registers.AF
    let BC = registers.BC
    let DE = registers.DE
    let HL = registers.HL
    let SP = registers.SP
    let PC = registers.PC
    let MasterIE = registers.MasterIE

    do
        match mapInfo.SymbolByName "_fgbc_debugger_break" with
        | Some symbol -> breakpoints <- breakpoints |> Map.add symbol.Address (Breakpoint(symbol.Address))
        | None -> ()

    let rec interactive () =
        printfn "-- DEBUGGER --"
        printfn "CPU paused at instruction 0x%04X = %s" PC.Value (Instruction.decodeOpcode mmu PC.Value |> Instruction.readable) 

        match mapInfo.SymbolByAddress PC.Value with
        | Some symbol -> printfn "Symbol: %s" symbol.Name
        | None -> ()
            
        printfn "continue (c), step (s), print registers (r), print stack (z), print uint8 at address (a8 address), print uint16 at address (a16 address), print summary (x), halt (h)"

        let cmdline = (System.Console.ReadLine ()).Split([|' '|])

        if cmdline.Length > 0 then

            let command = cmdline.[0]

            let parameter = if cmdline.Length > 1 then Some cmdline.[1] else None

            match command with
            | "c" ->
                stepping <- false
            | "s" ->
                stepping <- true
            | "r" ->
                this.PrintRegisters ()
                interactive ()
            | "z" ->
                this.PrintStack 0x20us
                interactive ()
            | "x" ->
                this.PrintSummary ()
                interactive ()
            | "h" ->
                cpu.Stop ()
            | "a8" ->
                match parameter with
                | Some address ->
                    let address = parseAddress address
                    printfn "0x%04X = %02X" address (mmu.Read8 address)
                | None -> ()
                interactive ()
            | "a16" ->
                match parameter with
                | Some address ->
                    let address = parseAddress address
                    printfn "0x%04X = %04X" address (mmu.Read16 address)
                | None -> ()
                interactive ()
            | _ ->
                interactive ()
        else
            interactive ()

    let cpuHook instruction = 

        if stepping || (this.HasBreakpoint PC.Value) then
            interactive ()

        previousPC <- PC.Value

        match instruction with
        | STOP -> cpu.Stop ()
        | _ -> ()  


    member this.Step () = stepping <- true

    member this.AddBreakpoint (breakpoint: Breakpoint) = breakpoints <- breakpoints |> Map.add breakpoint.Address breakpoint 

    member this.HasBreakpoint address = breakpoints |> Map.containsKey address

    member this.Map = mapInfo
    
    member this.Start () =
        stopWatch.Start ()
        try
            cpu.Start ()
        with
        | ex ->
            printfn "DEBUGGER: Unhandled exception: %s" <| ex.ToString ()  
            printfn "Rewinding CPU to last instruction"
            PC.Value <- previousPC
            interactive ()
        stopWatch.Stop ()
        printfn "Debugger: execution halted"

    member this.LoadMap path = ()

    member this.Attach () = cpu.Hook <- Some cpuHook

    member this.Detach () = cpu.Hook <- None

    member this.PrintMemory fromAddress toAddress =
        printfn "Memory dump (0x%04X - 0x%04X):\n\n        0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n" (fromAddress &&& 0xFFF0) (toAddress ||| 0xF)
        [(fromAddress >>> 4)..(toAddress >>> 4)] |> Seq.iter (fun x ->
            printf "0x%03Xx " x
            [0..15] |> Seq.iter (fun y ->
                printf "%02X " (mmu.Read8 (x <<< 4 ||| y |> uint16))
                )
            printfn ""
            )


    member this.PrintRegisters () =
        printfn 
          @"Registers:
            A  = 0x%02X
            B  = 0x%02X
            C  = 0x%02X
            D  = 0x%02X
            E  = 0x%02X
            F  = 0x%02X (Z = %d, N = %d, H = %d, C = %d)
            H  = 0x%02X
            L  = 0x%02X
            AF = 0x%04X
            BC = 0x%04X
            DE = 0x%04X
            HL = 0x%04X
            PC = 0x%04X
            SP = 0x%04X
            IE = %d" A.Value B.Value C.Value D.Value E.Value 
            F.Value (bitStateToValue F.Z) (bitStateToValue F.N) (bitStateToValue F.H) (bitStateToValue F.C)
            H.Value L.Value AF.Value BC.Value
            DE.Value HL.Value PC.Value SP.Value
            (bitStateToValue MasterIE.Value)

    member this.PrintStack depth = 
        printfn "Stack (0x%04X):" SP.Value 
        {(SP.Value)..(SP.Value + depth)} |> Seq.filter (fun a -> a%2us = 0us) |> Seq.iter (fun address ->
            printfn "\t0x%04X = %02X %02X" address (mmu.Read8 address) (mmu.Read8 (address + 1us))
            )


    member this.PrintIORegisters () = ()

    member this.PrintSystemClock () =
        printfn "System clock:\n\tCycles: %d,\tTime: %.4f ms" systemClock.Ticks systemClock.MilliSeconds
        
    member this.PrintRealWorldClock () =
        printfn "Real world clock:\n\t %d ms" (int stopWatch.Elapsed.TotalMilliseconds) 


    member this.PrintSummary () =
        this.PrintRegisters ()

        this.PrintStack 0x20us

        this.PrintSystemClock ()

        this.PrintRealWorldClock ()