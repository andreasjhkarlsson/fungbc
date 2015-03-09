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

    member this.add8 a b carry =
        let result = (uint16 a) + (uint16 b) + (uint16 carry)
        F.H <- bitStateOf 4 ((lowNibble a) + (lowNibble b) + carry)
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

    member this.And8 a b =
        let result = a &&& b
        F.ZNHC <- (setIfZero result,CLEAR,SET,CLEAR)
        result

    member this.Or8 a b =
        let result = a ||| b
        F.ZNHC <- (setIfZero result,CLEAR,CLEAR,CLEAR)
        result

    member this.Xor8 a b = 
        let result = a ^^^ b
        F.ZNHC <- (setIfZero result,CLEAR,CLEAR,CLEAR)
        result

type CPU (mmu) =

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

        let instruction = decodeOpcode PC.Value

        //printfn "Executing instruction: %s @ 0x%04X" (readable instruction) PC.Value

        // Shorted versions of name -> register lookup functions
        let r8 = registers.From8Name
        let r8r8 r1 r2 = (r8 r1,r8 r2)
        let r16 = registers.From16Name
        
        match instruction with
        (* 
            Register loads
        *)
        | LD_R8_R8 (r1,r2) ->
            (r8 r1).Value <- (r8 r2).Value
            PC.Advance 1
        | LD_R8_D8 (r,d) ->
            (r8 r).Value <- d
            PC.Advance 2
        | LD_A16_R8 (a,r) ->
            mmu.Write8 a ((r8 r).Value)
            PC.Advance 3
        | LD_R8_A16 (r,a) ->
            (r8 r).Value <- mmu.Read8 a
            PC.Advance 3
        | LD_R8_AR16 (r, ar) ->
            (r8 r).Value <- mmu.Read8 (r16 ar).Value
            PC.Advance 1
        | LD_R16_D16 (r,value) ->
            (r16 r).Value <- value
            PC.Advance 3
        (*
            ALU operations
        *)
        | ADD_R8_R8 (r1,r2) ->
            (r8 r1).Value <- alu.add8 (r8 r1).Value (r8 r2).Value 0uy
            PC.Advance 1
        | ADD_R8_D8 (r,operand) ->
            (r8 r).Value <- alu.add8 (r8 r).Value operand 0uy
            PC.Advance 2
        | ADD_R8_AR16 (r,ar) ->
            (r8 r).Value <- alu.add8 (r8 r).Value (mmu.Read8 (r16 ar).Value) 0uy
            PC.Advance 1
        | ADD_R16_R16 (r1,r2) ->
            (r16 r1).Value <- alu.add16 (r16 r1).Value (r16 r2).Value
            PC.Advance 1
        | ADC_R8_R8 (r1, r2) ->
            (r8 r1).Value <- alu.add8 (r8 r1).Value (r8 r2).Value (bitStateToValue F.C)
            PC.Advance 1
        | ADC_R8_D8 (r, operand) ->
            (r8 r).Value <- alu.add8 (r8 r).Value operand (bitStateToValue F.C)
            PC.Advance 2
        | ADC_R8_AR16 (r, ar) ->
            (r8 r).Value <- alu.add8 (r8 r).Value (mmu.Read8 (r16 ar).Value) (bitStateToValue F.C)
            PC.Advance 1
        | INC_R16 (r) ->
            (r16 r).Update alu.inc16
            PC.Advance 1
        | DEC_R16 (r) ->
            (r16 r).Update alu.dec16
            PC.Advance 1
        | INC_R8 (r) ->
            (r8 r).Update alu.inc8
            PC.Advance 1
        | DEC_R8 (r) ->
            (r8 r).Update alu.dec8
            PC.Advance 1
        | SWAP_R8 (r) ->
            (r8 r).Update alu.swapNibbles
            PC.Advance 2
        | SWAP_AR16 (r) ->
            mmu.Update8 (r16 r).Value alu.swapNibbles
            PC.Advance 2
        | CPL ->
            A.Update alu.bitNot8
            PC.Advance 1
        | RLC_R8 (r) ->
            (r8 r).Update alu.rotateLeftWithCarry8
            PC.Advance 2
        | AND_R8_R8 (r1,r2) ->
            (r8 r1).Update (alu.And8 (r8 r2).Value)
            PC.Advance 1
        | AND_R8_D8 (r,operand) ->
            (r8 r).Update (alu.And8 operand)
            PC.Advance 2
        | AND_R8_AR16 (r,ar) ->
            (r8 r).Update (alu.And8 <| mmu.Read8 (r16 ar).Value)
            PC.Advance 1
        | OR_R8_R8 (r1,r2) ->
            (r8 r1).Update (alu.Or8 (r8 r2).Value)
            PC.Advance 1
        | OR_R8_D8 (r,operand) ->
            (r8 r).Update (alu.Or8 operand)
            PC.Advance 2
        | OR_R8_AR16 (r,ar) ->
            (r8 r).Update (alu.Or8 <| mmu.Read8 (r16 ar).Value)
            PC.Advance 1
        | XOR_R8_R8 (r1,r2) ->
            (r8 r1).Update (alu.Xor8 (r8 r2).Value)
            PC.Advance 1
        | XOR_R8_D8 (r,operand) ->
            (r8 r).Update (alu.Xor8 operand)
            PC.Advance 2
        | XOR_R8_AR16 (r,ar) ->
            (r8 r).Update (alu.Xor8 <| mmu.Read8 (r16 ar).Value)
            PC.Advance 1
        (*
            Set/Clear/Test bits
        *)
        | SET_R8 (n,r) ->
            (r8 r).Update (setBit n)
            PC.Advance 2
        | SET_AR16 (n,r) ->
            mmu.Update8 (r16 r).Value (setBit n)
            PC.Advance 2
        | RES_R8 (n,r) ->
            (r8 r).Update (clearBit n) 
            PC.Advance 2
        | RES_AR16 (n,r) ->
            mmu.Update8 (r16 r).Value (clearBit n)
            PC.Advance 2
        | BIT_R8 (n,r) ->
            F.Z <- bitStateOf n (r8 r).Value |> bitStateInvert
            F.NH <- (CLEAR, SET)
            PC.Advance 2
        | BIT_AR16 (n,r) ->
            F.Z <- bitStateOf n (mmu.Read8 (r16 r).Value) |> bitStateInvert
            F.NH <- (CLEAR, SET)
            PC.Advance 1
        (*
            Jumps
        *)
        | JP_A16 (address) ->
            PC.Value <- address // Easiest instruction ever!
        | JP_AR16 (r) ->
            PC.Value <- mmu.Read16 (r16 r).Value
        | JP_F_A16 (f,address) ->
            match (F.FlagFromName f) with
            | SET -> PC.Value <- address
            | CLEAR -> PC.Advance 3
        | JP_NF_A16 (f,address) ->
            match (F.FlagFromName f) with
            | SET -> PC.Advance 3
            | CLEAR -> PC.Value <- address
        | JR_A8 (offset) ->
            PC.Value <- (int16 PC.Value) + (int16 offset) |> uint16 
        | JR_F_A8 (f, offset) ->
            match (F.FlagFromName f) with
            | SET -> PC.Value <- (int16 PC.Value) + (int16 offset) |> uint16
            | CLEAR -> PC.Advance 2
        | JR_NF_A8 (f, offset) ->
            match (F.FlagFromName f) with
            | CLEAR -> PC.Value <- (int16 PC.Value) + (int16 offset) |> uint16
            | SET -> PC.Advance 2
        (*
            Misc
        *)
        | SCF ->
            F.NHC <- (CLEAR, CLEAR, SET)
            PC.Advance 1
        | CCF ->
            F.NHC <- (CLEAR, CLEAR, bitStateInvert F.C)
            PC.Advance 1
        | NOP ->
            PC.Advance 1
        | STOP ->
            PC.Advance 0
        | FGBC_PRINT_R8 (r) ->
            printfn "%d" (r8 r).Value
            PC.Advance 1
        | FGBC_PRINTA_R8 (r) ->
            printf "%c" <| char (r8 r).Value 
            PC.Advance 1
        | _ -> raise (System.Exception(sprintf "opcode <%O> not implemented" instruction))

        
        if instruction <> STOP then
            execute ()    

    
    member this.Reset () =
        registers.A.Value <- 0x00uy
        registers.B.Value <- 0x00uy
        registers.C.Value <- 0x13uy
        registers.DE.Value <- 0x00D8us
        registers.HL.Value <- 0x014Dus
        registers.SP.Value <- 0xFFFEus
        registers.PC.Value <- 0us

    member this.Start () =
        this.Reset()
        execute ()

    member this.MMU = mmu

    member this.Registers = registers


