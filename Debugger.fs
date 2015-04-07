module Debugger

open Misc
open Mmu
open Cpu
open Instruction
open BitLogic
open Register
open Clock
open System.IO
open Gpu
open Tile
open Interrupts
open System.Text.RegularExpressions

type Breakpoint(address) =
    member this.Address = address
    member val Active = true with get, set

type Symbol(address: uint16,name, _module) =
    member this.Address = address
    member this.Name = name
    member this.Module = _module

type MapInfo (symbols) =

    member this.SymbolByName name = symbols |> Map.tryPick (fun _ (symbol: Symbol) -> if symbol.Name = name then Some symbol else None)

    member this.SymbolByAddress address = symbols |> Map.tryFind address

    member this.Symbols = symbols |> Map.toSeq |> Seq.map snd

    member this.NearestSymbol address =
        symbols
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.sortBy (fun s -> s.Address |> int |> (~-))
        |> Seq.tryFind (fun s -> address >= s.Address)

    new () = MapInfo(Map.empty)

    new (path: string) =
        let groups = Regex.Matches(File.ReadAllText path ,@"^\s+([0-9A-F]+)\s+(\S+)\s+(\w*)\s*$",RegexOptions.Multiline)

        let symbols = 
                groups |> Seq.cast<Match>
                |> Seq.map (fun m ->
                    m.Groups.Item(1).Value, m.Groups.Item(2).Value, m.Groups.Item(3).Value)
                |> Seq.filter (fun (_,name,_module) ->
                    (name.EndsWith("_end") || name.EndsWith("_start") || _module = "") |> not)
                |> Seq.map (fun (address, name, _module) ->
                    let address = parseHex address |> Option.get |> uint16
                    address, Symbol(address, name, _module))
                |> Map.ofSeq
       
        MapInfo(symbols)
    

type Debugger(cpu: CPU,gpu: GPU, mmu: MMU,interrupts: InterruptManager, systemClock: Clock, mapInfo: MapInfo) as this =

    let mutable breakpoints = Map.empty<uint16,Breakpoint>

    let mutable stepping = false

    let mutable previousPC = 0us

    let mutable stepAfterReturn = false
    
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

    do
        match mapInfo.SymbolByName "_fgbc_debugger_break" with
        | Some symbol -> breakpoints <- breakpoints |> Map.add symbol.Address (Breakpoint(symbol.Address))
        | None -> ()

        System.Console.CancelKeyPress.AddHandler
            <| new System.ConsoleCancelEventHandler(fun obj args ->
                if not <| this.IsStepping () then
                    printfn "\n--- Ctrl+C, invoking debugger ---\n"
                    args.Cancel <- true
                    this.Step ()
                ) 

    let formatInstruction pc =
        let instruction = Instruction.decodeOpcode mmu pc
        let readable = Instruction.readable instruction 

        let formatSymbolAt address =
            sprintf "(%s) " (match mapInfo.SymbolByAddress address with |Some symbol -> symbol.Name |None -> "<symbol not found>")

        sprintf "%s %s@ 0x%04X " readable
            (match instruction with
            | JP_A16 (address)
            | JP_F_A16 (_,address)
            | JP_NF_A16 (_,address)
            | CALL_A16 (address) 
            | CALL_NF_A16 (_,address)
            | CALL_F_A16 (_,address) -> formatSymbolAt address
            | _ -> "")
            pc

    let rec interactive () =

        let printWithColor bg fg str =
            System.Console.BackgroundColor <- bg
            System.Console.ForegroundColor <- fg
            printfn "%s" str
            System.Console.ResetColor()           

        let setResultFormatting () = 
            System.Console.BackgroundColor <- System.ConsoleColor.DarkGreen
            System.Console.ForegroundColor <- System.ConsoleColor.White

        let clearFormatting = System.Console.ResetColor

        let printError = printWithColor System.ConsoleColor.DarkRed System.ConsoleColor.White

        let printResult = printWithColor System.ConsoleColor.DarkGreen System.ConsoleColor.White

        printfn ""
        printfn "--------------"
        printfn "-- DEBUGGER --"
        printfn "--------------"
        printfn "CPU paused in (%s) at instruction 0x%04X = %s"
            (match mapInfo.NearestSymbol PC.Value with | Some symbol -> sprintf "module: %s, symbol: %s" symbol.Module symbol.Name | None -> "<symbol not found>")
            PC.Value (formatInstruction PC.Value) 
            
        System.Console.WriteLine(
            "continue (c), " + 
            "step (s), " +
            "step return (w), " +
            "last instruction (i), " +
            "print breakpoints (b), " +
            "add breakpoint (a), " +
            "remove breakpoint (d), " +
            "print interrupt status (q), "+
            "find symbol (y), " +
            "print gpu info (g), "+
            "print tile map0 (m0), "+
            "print tile map1 (m1), "+
            "print tile data 0 (t0), "+
            "print tile data 1 (t1), "+
            "print symbols (u), " +
            "print registers (r), " +
            "print stack (z), " +
            "print uint8 at address (a8 address), " +
            "print uint16 at address (a16 address), " +
            "print summary (x), " +
            "halt (h)")

        if cpu.WaitingForInterrupt then
            printfn "NOTE: CPU is currently halted, waiting for an interrupt, enter (f) to force resume"

        let cmdline = (System.Console.ReadLine ()).Split([|' '|])

        if cmdline.Length > 0 then

            let command = cmdline.[0]

            let parameter = if cmdline.Length > 1 then Some cmdline.[1] else None

            let (|HexParameter|_|) parameter =
                match parameter with
                | Some str -> parseHex str
                | None -> None

            let (|AddressParameter|_|) = (|HexParameter|_|) >> (Option.map uint16)

            match command with
            | "c" ->
                stepping <- false
            | "s" ->
                stepping <- true
            | "w" ->
                stepping <- false
                stepAfterReturn <- true
            | "b" ->
                breakpoints
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.iter (fun b ->
                    printResult <| sprintf "Breakpoint: address = 0x%04X, symbol = (%s)" b.Address
                        (match mapInfo.SymbolByAddress b.Address with Some s -> s.Name | None -> ""))
                interactive ()
            | "i" ->
                printResult <| sprintf "Last executed instruction = %s" (formatInstruction previousPC)
                interactive ()
            | "a" ->
                match parameter with
                | AddressParameter address ->
                    this.AddBreakpoint (Breakpoint(address))
                | Some str ->
                    match mapInfo.SymbolByName str with
                    | Some s -> this.AddBreakpoint (Breakpoint(s.Address))
                    | None -> printResult <| sprintf "No symbol found with name %s" str
                | None -> printError "Invalid command"
                interactive ()
            | "d" ->
                let deleteAtAddress address =
                    match breakpoints |> Map.tryFind address with
                    | Some breakpoint ->
                        this.RemoveBreakpoint breakpoint
                        printResult "Breakpoint removed"
                    | None ->
                        printError "No breakpoint found at address"
                match parameter with
                | AddressParameter address -> deleteAtAddress address
                | Some str ->
                    match mapInfo.SymbolByName str with
                    | Some s ->
                        deleteAtAddress s.Address
                        printResult "Breakpoint removed"
                    | None -> printfn "No symbol found with name %s" str
                | None ->
                    printError "Invalid command"
                interactive ()
            | "g" ->
                let lcdc = gpu.Registers.LCDC
                let lcds = gpu.Registers.LCDS

                let mapToDescription = function |Map0 -> "map0 (0x9800 - 0x9BFF)" |Map1 -> "map1 (0x9C00 - 0x9FFF)"
                
                let tileDataSelectToDescription = function |Tiles0 -> "tiles0 (0x8800 - 0x97FF)" |Tiles1 -> "tiles1 (0x8000 - 0x8FFF)"

                printResult "GPU Status\n"
                printResult "LCDC (0xFF40):"
                printResult <| sprintf "\tRaw value: 0x%02X" lcdc.Value
                printResult <| sprintf "\tDisplay enable (7): %b" lcdc.DisplayEnable
                printResult <| sprintf "\tWindow tile map (6): %s" (mapToDescription lcdc.WindowTileMapSelect)
                printResult <| sprintf "\tWindow enable (5): %b" lcdc.WindowEnabled
                printResult <| sprintf "\tBG & Window tile data select (4): %s" (tileDataSelectToDescription lcdc.BGAndWindowTileDataSelect)
                printResult <| sprintf "\tBG Tile map (3): %s" (mapToDescription lcdc.BGTilemapSelect)
                printResult <| sprintf "\tSprite size (2):  %A" lcdc.SpriteSize
                printResult <| sprintf "\tSprite enable (1): %b" lcdc.SpriteEnable
                printResult <| sprintf "\tBG enable (0): %b" lcdc.BGDisplay
                printResult ""

                printResult "LCDS (0xFF41):"
                printResult <| sprintf "\tRaw value: 0x%02X" lcds.Value
                printResult <| sprintf "\tLYC=LY interrupt enable (6): %b" lcds.LYCLYCoincidenceInterrupt
                printResult <| sprintf "\tOAM interrupt enable (5): %b" lcds.OAMInterrupt
                printResult <| sprintf "\tVBlank interupt enable (4): %b" lcds.VBlankInterrupt
                printResult <| sprintf "\tHBlank interupt enable (3): %b" lcds.HBlankInterrupt
                printResult <| sprintf "\tLYC = LY (2):  %b" (if lcds.Coincidence = LYC_E_LY then true else false)
                printResult <| sprintf "\tMode (1-0): %s" (match lcds.Mode with
                                                            |Mode.VBlank -> "VBlank"
                                                            |Mode.HBlank -> "HBlank"
                                                            |Mode.LCDDriverDataTransfer -> "LCD Data transfer"
                                                            |Mode.SearchingOAMRAM -> "Saerching OAM-RAM")
                printResult ""

                printResult <| sprintf "SCY (0xFF42): %d" gpu.Registers.SCY.Value
                printResult <| sprintf "SCX (0xFF43): %d" gpu.Registers.SCX.Value

                printResult <| sprintf "LY (0xFF44): %d" gpu.Registers.LY.Value
                printResult <| sprintf "LYC (0xFF45): %d" gpu.Registers.LYC.Value

                printResult <| sprintf "BGP (0xFF47): 0x%02X" gpu.Registers.BGP.Value

                interactive ()
            | "m0" ->
                setResultFormatting ()
                this.PrintMemory 0x9800 0x9BFF
                clearFormatting ()
                interactive ()
            | "m1" ->
                setResultFormatting ()
                this.PrintMemory 0x9C00 0x9FFF
                clearFormatting ()
                interactive ()     
            | "t0" ->
                setResultFormatting ()
                this.PrintMemory 0x8800 0x97FF
                clearFormatting ()
                interactive ()
            | "t1" ->
                setResultFormatting ()
                this.PrintMemory 0x8000 0x8FFF
                clearFormatting ()
                interactive ()            
            | "y" ->
                match parameter with
                | AddressParameter address ->
                    match mapInfo.NearestSymbol address with
                    | Some s -> printResult <| sprintf "Symbol (%s) in module (%s) @ 0x%04X" s.Name s.Module s.Address
                    | None -> printError <| sprintf "No symbol at address %04X" address                    
                | Some str ->
                    match mapInfo.SymbolByName str with
                    | Some s -> printResult <| sprintf "Symbol (%s) in module (%s) @ 0x%04X" s.Name s.Module s.Address
                    | None -> printError <| sprintf "No symbol with name %s" str
                | None ->
                    printError "Invalid command"
                interactive ()
            | "u" ->
                printResult <| (mapInfo.Symbols |> Seq.map (fun s -> sprintf "%s @ 0x%04X in module %s, " s.Name s.Address s.Module) |> Seq.fold (+) "")
                interactive ()
            | "r" ->
                setResultFormatting ()
                this.PrintRegisters ()
                clearFormatting ()
                interactive ()
            | "z" ->
                setResultFormatting ()
                this.PrintStack 0x20us
                clearFormatting ()
                interactive ()
            | "x" ->
                setResultFormatting ()
                this.PrintSummary ()
                clearFormatting ()
                interactive ()
            | "h" ->
                cpu.WaitingForInterrupt <- false
                cpu.Stop ()
            | "f" when cpu.WaitingForInterrupt ->
                cpu.WaitingForInterrupt <- false
            | "q" ->
                printResult <| sprintf "Master interrupt enable = %b" (interrupts.Enable)
                printResult ""
                printResult <|
                    sprintf
                        "Interrupt enable register (0xFFFF):\n\tTimer: %b\n\tVBlank: %b\n\tLCDC: %b\n\tSerialIO: %b\n\tP10P13Flip: %b"
                        (interrupts.InterruptEnable.Enabled TimerOverflow)
                        (interrupts.InterruptEnable.Enabled VBlank)
                        (interrupts.InterruptEnable.Enabled LCDC)
                        (interrupts.InterruptEnable.Enabled SerialIO)
                        (interrupts.InterruptEnable.Enabled P10P13Flip)
                printResult ""
                printResult <|
                    sprintf
                        "GPU Interrupts:\n\tLYCLYCoincidence: %b\n\tOAM: %b\n\tVBlank: %b\n\tHBlank: %b"
                        (gpu.Registers.LCDS.LYCLYCoincidenceInterrupt)
                        (gpu.Registers.LCDS.OAMInterrupt)
                        (gpu.Registers.LCDS.VBlankInterrupt)
                        (gpu.Registers.LCDS.HBlankInterrupt)
                printResult ""
                printResult <|
                    sprintf
                        "Current active interrupts (0xFF0F):\n\tTimer: %b\n\tVBlank: %b\n\tLCDC: %b\n\tSerialIO: %b\n\tP10P13Flip: %b"
                        (interrupts.Current.[TimerOverflow])
                        (interrupts.Current.[VBlank])
                        (interrupts.Current.[LCDC])
                        (interrupts.Current.[SerialIO])
                        (interrupts.Current.[P10P13Flip])
                printResult ""
                printResult <|
                    sprintf
                        "Interrupt vectors:\n\tTimer: 0x%04X\n\tVBlank: 0x%04X\n\tLCDC: 0x%04X\n\tSerialIO: 0x%04X\n\tP10P13Flip: 0x%04X"
                        (Interrupts.address TimerOverflow)
                        (Interrupts.address VBlank)
                        (Interrupts.address LCDC)
                        (Interrupts.address SerialIO)
                        (Interrupts.address P10P13Flip)
                interactive ()

            | "a8" ->
                match parameter with
                | AddressParameter address -> printResult <| sprintf "0x%04X = %02X" address (mmu.Read8 address)
                | _ -> printError "Invalid address format"
                interactive ()
            | "a16" ->
                match parameter with
                | AddressParameter address -> printResult <| sprintf "0x%04X = %04X" address (mmu.Read16 address)
                | _ -> printError "Invalid address format"
                interactive ()
            | _ ->
                interactive ()
        else
            interactive ()

    let cpuHook instruction = 
        
        if stepAfterReturn then
            match (decodeOpcode mmu previousPC) with
            |RET |RETI |RET_F _ |RET_NF _ ->
                stepAfterReturn <- false
                stepping <- true 
            | _ ->
                ()

        if stepping || (this.HasBreakpoint PC.Value) then
            interactive ()

        previousPC <- PC.Value

        match instruction with
        | STOP -> cpu.Stop ()
        | _ -> ()  


    member this.Step () =
        if cpu.WaitingForInterrupt then
            interactive ()
        stepping <- true

    member this.IsStepping () = stepping

    member this.AddBreakpoint (breakpoint: Breakpoint) = breakpoints <- breakpoints |> Map.add breakpoint.Address breakpoint 

    member this.HasBreakpoint address = breakpoints |> Map.containsKey address

    member this.RemoveBreakpoint (breakpoint: Breakpoint) = breakpoints <- breakpoints |> Map.remove breakpoint.Address

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
            (if interrupts.Enable then 1 else 0)

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