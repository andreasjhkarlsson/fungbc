module Cpu

open Mmu
open Register
open Instruction

type CPU () =

    let mmu = MMU()
    
    let registers = RegisterSet()

    let mutable debugEnabled = false

    let rec execute pc = 
        
        // Update PC register
        registers.PC.value <- pc

        let instruction = decodeOpcode mmu pc

        printfn "Executing instruction: %s @ 0x%04X" (readable instruction) pc

        // Shorted versions of name -> register lookup functions
        let r8 = registers.from8Name
        let r8r8 r1 r2 = (r8 r1,r8 r2)
        let r16 = registers.from16Name

        // For convinience, precalculate common next PC values
        let oneAhead = pc + 1us
        let twoAhead = pc + 2us
        let threeAhead = pc + 3us

        let next = match instruction with
                    | NOP ->
                        pc + 1us
                    | STOP ->
                        pc
                    | LD_R8_R8 (r1,r2) ->
                        let r1, r2 = r8r8 r1 r2
                        r1.value <- r2.value
                        oneAhead
                    | LD_R8_D8 (r,d) ->
                        (r8 r).value <- d
                        twoAhead
                    | LD_A16_R8 (a,r) ->
                        mmu.write8 a ((r8 r).value)
                        threeAhead
                    | LD_R8_A16 (r,a) ->
                        (r8 r).value <- mmu.read8 a
                        threeAhead
                    | INC_R8 (r) ->
                        let r = r8 r
                        r.value <- r.value + 1uy
                        oneAhead
                    | DEC_R8 (r) ->
                        let r = r8 r
                        r.value <- r.value - 1uy
                        oneAhead
                    | SWAP_R8 (r) ->
                        let r = r8 r
                        r.value <- ((r.value &&& 0xFuy) <<< 4) ||| ((r.value &&& 0xF0uy) >>> 4)
                        twoAhead
                    | _ -> raise (System.Exception(sprintf "opcode <%O> not implemented" instruction))
        
        // TODO: Maybe a program sometimes jump to the same address for some reason (waiting for interrupt)? Investigate!
        if next <> pc then
            execute next

    member this.enableDebug () = debugEnabled <- true

    member this.loadProgram (program: array<uint8>) =
        let baseAddress = 0us
        program |> Array.iteri (fun index b -> mmu.write8 (baseAddress + (uint16 index)) b)
    
    member this.printState () =
        let r = registers
        printfn @"CPU State:
            A  = 0x%02X
            B  = 0x%02X
            C  = 0x%02X
            D  = 0x%02X
            E  = 0x%02X
            F  = 0x%02X
            H  = 0x%02X
            L  = 0x%02X
            AF = 0x%04X
            BC = 0x%04X
            DE = 0x%04X
            HL = 0x%04X
            PC = 0x%04X
            SP = 0x%04X
        " r.A.value r.B.value r.C.value r.D.value r.E.value 
            r.F.value r.H.value r.L.value r.AF.value r.BC.value
            r.DE.value r.HL.value r.PC.value r.SP.value

    member this.printMemory = mmu.printDump
    
    member this.reset () =
        registers.A.value <- 0x00uy
        registers.B.value <- 0x00uy
        registers.C.value <- 0x13uy
        registers.F.value <- 0xB0uy
        registers.DE.value <- 0x00D8us
        registers.HL.value <- 0x014Dus
        registers.SP.value <- 0xFFFEus
        registers.PC.value <- 0us


    member this.start () =
        this.reset()
        execute 0us


