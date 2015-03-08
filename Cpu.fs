module Cpu

open Mmu
open Register
open Instruction
open BitLogic

// Alas, how how I wish that F# had inner types so that this type could be
// contained inside the CPU type. Now it's exposed and dirty and has to take internal
// state of the CPU type as parameters. Gross!
type ALU (registers: RegisterSet) =

    let F = registers.F

    member this.add8 a b =
        let result = (uint16 a) + (uint16 b)
        F.H <- bitStateOf 4 ((lowNibble a) + (lowNibble b))
        F.ZNC <- (setIfZero (uint8 result), CLEAR, bitStateOf 8 result )
        uint8 result

    member this.add16 a b =
        let result = (int a) + (int b)
        F.H <- bitStateOf 12 ((a &&& 0xFFFus) + (b &&& 0xFFFus))
        F.ZNC <- (setIfZero (uint16 result), CLEAR, bitStateOf 16 result)
        uint16 result


    member this.inc8 a =
        let result = a + 1uy
        F.H <- (lowNibble a) = 0xFuy |> setIfTrue
        F.ZH <- (setIfZero result, CLEAR)
        result

    member this.inc16 a = a + 1us // Does not set any flags

    member this.dec8 a =
        let result = a - 1uy
        let halfCarry = (lowNibble a) = 0uy |> setIfTrue
        F.ZNH <- (setIfZero result, SET, halfCarry)
        result

    member this.dec16 a = a - 1us // Does not set any flags (not even subtraction flag!)

    member this.bitNot8 a =
        F.NH <- (SET,SET)
        ~~~ a
    
    member this.swapNibbles a =
        let result = swapNibbles a 
        F.ZNHC <- (setIfZero result, CLEAR, CLEAR, CLEAR)
        result

    member this.rotateLeftWithCarry8 a =
        let highestBit = bitStateOf 7 a
        let result =  (a <<< 1) ||| (bitStateToValue highestBit)
        F.ZNHC <- (setIfZero result, CLEAR, CLEAR, highestBit)
        result

type CPU () =

    let mmu = MMU()
    
    let registers = RegisterSet()

    let alu = ALU(registers)

    let decodeOpcode = decodeOpcode mmu

    let mutable debugEnabled = false

    let rec execute () = 

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

        let instruction = decodeOpcode PC.value

        printfn "Executing instruction: %s @ 0x%04X" (readable instruction) PC.value

        // Shorted versions of name -> register lookup functions
        let r8 = registers.from8Name
        let r8r8 r1 r2 = (r8 r1,r8 r2)
        let r16 = registers.from16Name
        
        match instruction with
        (* 
            Register loads
        *)
        | LD_R8_R8 (r1,r2) ->
            (r8 r1).value <- (r8 r2).value
            PC.advance 1
        | LD_R8_D8 (r,d) ->
            (r8 r).value <- d
            PC.advance 2
        | LD_A16_R8 (a,r) ->
            mmu.write8 a ((r8 r).value)
            PC.advance 3
        | LD_R8_A16 (r,a) ->
            (r8 r).value <- mmu.read8 a
            PC.advance 3
        | LD_R16_D16 (r,value) ->
            (r16 r).value <- value
            PC.advance 3
        (*
            ALU operations
        *)
        | ADD_R8_R8 (r1,r2) ->
            (r8 r1).value <- alu.add8 (r8 r1).value (r8 r2).value
            PC.advance 1
        | ADD_R8_D8 (r,operand) ->
            (r8 r).value <- alu.add8 (r8 r).value operand
            PC.advance 2
        | ADD_R8_AR16 (r,ar) ->
            (r8 r).value <- alu.add8 (r8 r).value (mmu.read8 (r16 ar).value)
            PC.advance 1
        | ADD_R16_R16 (r1,r2) ->
            (r16 r1).value <- alu.add16 (r16 r1).value (r16 r2).value
            PC.advance 1
        | INC_R16 (r) ->
            (r16 r).update alu.inc16
            PC.advance 1
        | DEC_R16 (r) ->
            (r16 r).update alu.dec16
            PC.advance 1
        | INC_R8 (r) ->
            (r8 r).update alu.inc8
            PC.advance 1
        | DEC_R8 (r) ->
            (r8 r).update alu.dec8
            PC.advance 1
        | SWAP_R8 (r) ->
            (r8 r).update alu.swapNibbles
            PC.advance 2
        | SWAP_AR16 (r) ->
            mmu.update8 (r16 r).value alu.swapNibbles
            PC.advance 2
        | CPL ->
            A.update alu.bitNot8
            PC.advance 1
        | RLC_R8 (r) ->
            (r8 r).update alu.rotateLeftWithCarry8
            PC.advance 2
        (*
            Set/Clear/Test bits
        *)
        | SET_R8 (n,r) ->
            (r8 r).update (setBit n)
            PC.advance 2
        | SET_AR16 (n,r) ->
            mmu.update8 (r16 r).value (setBit n)
            PC.advance 2
        | RES_R8 (n,r) ->
            (r8 r).update (clearBit n) 
            PC.advance 2
        | RES_AR16 (n,r) ->
            mmu.update8 (r16 r).value (clearBit n)
            PC.advance 2
        | BIT_R8 (n,r) ->
            F.Z <- bitStateOf n (r8 r).value |> bitStateInvert
            F.NH <- (CLEAR, SET)
            PC.advance 2
        | BIT_AR16 (n,r) ->
            F.Z <- bitStateOf n (mmu.read8 (r16 r).value) |> bitStateInvert
            F.NH <- (CLEAR, SET)
            PC.advance 1
        (*
            Jumps
        *)
        | JP_A16 (address) ->
            PC.value <- address // Easiest instruction ever!
        | JP_AR16 (r) ->
            PC.value <- mmu.read16 (r16 r).value
        | JP_F_A16 (f,address) ->
            match (F.flagFromName f) with
            | SET -> PC.value <- address
            | CLEAR -> PC.advance 1
        | JP_NF_A16 (f,address) ->
            match (F.flagFromName f) with
            | SET -> PC.advance 1
            | CLEAR -> PC.value <- address
        | JR_A8 (offset) ->
            PC.value <- (int16 PC.value) + (int16 offset) |> uint16 
        | JR_F_A8 (f, offset) ->
            match (F.flagFromName f) with
            | SET -> PC.value <- (int16 PC.value) + (int16 offset) |> uint16
            | CLEAR -> PC.advance 1
        | JR_NF_A8 (f, offset) ->
            match (F.flagFromName f) with
            | CLEAR -> PC.value <- (int16 PC.value) + (int16 offset) |> uint16
            | SET -> PC.advance 1
        (*
            Misc
        *)
        | SCF ->
            F.NHC <- (CLEAR, CLEAR, SET)
            PC.advance 1
        | CCF ->
            F.NHC <- (CLEAR, CLEAR, bitStateInvert F.C)
            PC.advance 1
        | NOP ->
            PC.advance 1
        | STOP ->
            PC.advance 0
        | _ -> raise (System.Exception(sprintf "opcode <%O> not implemented" instruction))
        
        if instruction <> STOP then
            execute ()

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
            F  = 0x%02X (Z = %d, N = %d, H = %d, C = %d)
            H  = 0x%02X
            L  = 0x%02X
            AF = 0x%04X
            BC = 0x%04X
            DE = 0x%04X
            HL = 0x%04X
            PC = 0x%04X
            SP = 0x%04X
        " r.A.value r.B.value r.C.value r.D.value r.E.value 
            r.F.value (bitStateToValue r.F.Z) (bitStateToValue r.F.N) (bitStateToValue r.F.H) (bitStateToValue r.F.C)
            r.H.value r.L.value r.AF.value r.BC.value
            r.DE.value r.HL.value r.PC.value r.SP.value

    member this.printMemory = mmu.printDump
    
    member this.reset () =
        registers.A.value <- 0x00uy
        registers.B.value <- 0x00uy
        registers.C.value <- 0x13uy
        registers.DE.value <- 0x00D8us
        registers.HL.value <- 0x014Dus
        registers.SP.value <- 0xFFFEus
        registers.PC.value <- 0us

    member this.start () =
        this.reset()
        execute ()


