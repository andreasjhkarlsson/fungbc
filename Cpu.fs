module Cpu

open Mmu
open Register
open Instruction
open BitLogic
open Clock
open Interrupts

// Alas, how how I wish that F# had inner types so that this type could be
// contained inside the CPU type. Now it's exposed and dirty and have to take internal
// state of the CPU type as parameters. Gross!
type ALU (registers: RegisterSet) =

    let F = registers.F

    member this.Add8 a b carry =
        let result = (uint16 a) + (uint16 b) + (uint16 carry)
        F.H <- bitStateOf 4 ((lowNibble a) + (lowNibble b) + carry)
        F.ZNC <- (setIfZero (uint8 result), CLEAR, bitStateOf 8 result )
        uint8 result

    member this.Add16 a b =
        let result = (int a) + (int b)
        F.H <- bitStateOf 12 ((a &&& 0xFFFus) + (b &&& 0xFFFus))
        F.ZNC <- (setIfZero (uint16 result), CLEAR, bitStateOf 16 result)
        uint16 result

    member this.Sub8 a b carry =
        let result = (int16 a) - (int16 b) - (int16 carry)
        F.C <- setIfTrue (result < 0s)
        F.H <- setIfTrue ((int8 <| lowNibble a) - (int8 <| lowNibble b) - (int8 carry) < 0y)
        F.ZN <- (setIfZero (uint8 result), SET)
        uint8 result

    member this.Inc8 a =
        let result = a + 1uy
        F.H <- (lowNibble a) = 0xFuy |> setIfTrue
        F.ZN <- (setIfZero result, CLEAR)
        result

    member this.Inc16 a = a + 1us // Does not set any flags

    member this.Dec8 a =
        let result = a - 1uy
        let halfCarry = (lowNibble a) = 0uy |> setIfTrue
        F.Z <- setIfZero result
        F.N <- SET
        F.H <- halfCarry
        result

    member this.Dec16 a = a - 1us // Does not set any flags (not even subtraction flag!)

    member this.Compare8 a b = this.Sub8 a b 0uy |> ignore<uint8> // Compare is just subtraction with the result thrown away.

    member this.BitNot8 a =
        F.NH <- (SET,SET)
        ~~~ a
    
    member this.SwapNibbles a =
        let result = swapNibbles a 
        F.ZNHC <- (setIfZero result, CLEAR, CLEAR, CLEAR)
        result

    member this.RotateLeftWithCarry8 a =
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

type CPU (mmu, timerInterrupt: TimerInterrupt, clock: MutableClock) as this =

    let registers = RegisterSet()

    let alu = ALU(registers)

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

    let decodeOpcode = decodeOpcode mmu

    let mutable debugEnabled = false

    let push16 value =
        SP.Value <- SP.Value - 2us
        mmu.Write16 SP.Value value

    let pop16 () =
        let result = mmu.Read16 SP.Value
        SP.Value <- SP.Value + 2us
        result

    let checkForTimerInterrupt () =
        match MasterIE.Value with
        | SET ->
            if timerInterrupt.Check () then this.RaiseInterrupt timerInterrupt
        | CLEAR ->
            ()

    let rec execute () = 

        let instruction = decodeOpcode PC.Value

        let instructionSize = sizeOf instruction

        let nextInstruction = PC.Value + (uint16 instructionSize)

        // Just one little mutable flag. Sorry purists.
        let mutable longCycle = false

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
            PC.Value <- nextInstruction
        | LD_R8_D8 (r,d) ->
            (r8 r).Value <- d
            PC.Value <- nextInstruction
        | LD_A16_R8 (a,r) ->
            mmu.Write8 a ((r8 r).Value)
            PC.Value <- nextInstruction
        | LD_R8_A16 (r,a) ->
            (r8 r).Value <- mmu.Read8 a
            PC.Value <- nextInstruction
        | LD_R8_AR16 (r, ar) ->
            (r8 r).Value <- mmu.Read8 (r16 ar).Value
            PC.Value <- nextInstruction
        | LD_R16_D16 (r,value) ->
            (r16 r).Value <- value
            PC.Value <- nextInstruction
        | LDI_R8_AR16 (r,ar) ->
            (r8 r).Value <- mmu.Read8 (r16 ar).Value
            (r16 ar).Update ((+) 1us)
            PC.Value <- nextInstruction
        | LDI_AR16_R8 (ar,r) ->
            mmu.Write8 (r16 ar).Value (r8 r).Value
            (r16 ar).Update ((+) 1us)
            PC.Value <- nextInstruction
        | LDD_R8_AR16 (r,ar) ->
            (r8 r).Value <- mmu.Read8 (r16 ar).Value
            (r16 ar).Update ((-) 1us)
            PC.Value <- nextInstruction
        | LDD_AR16_R8 (ar,r) ->
            mmu.Write8 (r16 ar).Value (r8 r).Value
            (r16 ar).Update ((-) 1us)
            PC.Value <- nextInstruction
        | LDH_A8_R8 (offset, r) ->
            mmu.Write8 (0xFF00us + (uint16 offset)) (r8 r).Value
            PC.Value <- nextInstruction
        | LDH_R8_A8 (r, offset) ->
            (r8 r).Value <- mmu.Read8 (0xFF00us + (uint16 offset))
            PC.Value <- nextInstruction
        | LDH_AR8_R8 (ar, r) ->
            mmu.Write8 (0xFF00us + (uint16 (r8 ar).Value)) (r8 r).Value
            PC.Value <- nextInstruction
        | PUSH_R16 (r) ->
            push16 (r16 r).Value
            PC.Value <- nextInstruction
        | POP_R16 (r) ->
            (r16 r).Value <- pop16 ()
            PC.Value <- nextInstruction
        (*
            ALU operations
        *)
        | ADD_R8_R8 (r1,r2) ->
            (r8 r1).Value <- alu.Add8 (r8 r1).Value (r8 r2).Value 0uy
            PC.Value <- nextInstruction
        | ADD_R8_D8 (r,operand) ->
            (r8 r).Value <- alu.Add8 (r8 r).Value operand 0uy
            PC.Value <- nextInstruction
        | ADD_R8_AR16 (r,ar) ->
            (r8 r).Value <- alu.Add8 (r8 r).Value (mmu.Read8 (r16 ar).Value) 0uy
            PC.Value <- nextInstruction
        | ADD_R16_R16 (r1,r2) ->
            (r16 r1).Value <- alu.Add16 (r16 r1).Value (r16 r2).Value
            PC.Value <- nextInstruction
        | ADC_R8_R8 (r1, r2) ->
            (r8 r1).Value <- alu.Add8 (r8 r1).Value (r8 r2).Value (bitStateToValue F.C)
            PC.Value <- nextInstruction
        | ADC_R8_D8 (r, operand) ->
            (r8 r).Value <- alu.Add8 (r8 r).Value operand (bitStateToValue F.C)
            PC.Value <- nextInstruction
        | ADC_R8_AR16 (r, ar) ->
            (r8 r).Value <- alu.Add8 (r8 r).Value (mmu.Read8 (r16 ar).Value) (bitStateToValue F.C)
            PC.Value <- nextInstruction
        | SUB_R8_R8 (r1,r2) ->
            (r8 r1).Value <- alu.Sub8 (r8 r1).Value (r8 r2).Value 0uy
            PC.Value <- nextInstruction
        | SUB_R8_AR16 (r,ar) ->
            (r8 r).Value <- alu.Sub8 (r8 r).Value (mmu.Read8 (r16 ar).Value) 0uy
            PC.Value <- nextInstruction
        | SUB_R8_D8 (r, operand) ->
            (r8 r).Value <- alu.Sub8 (r8 r).Value operand 0uy
            PC.Value <- nextInstruction
        | SBC_R8_R8 (r1, r2) ->
            (r8 r1).Value <- alu.Sub8 (r8 r1).Value (r8 r2).Value (bitStateToValue F.C)
            PC.Value <- nextInstruction
        | SBC_R8_AR16 (r, ar) ->
            (r8 r).Value <- alu.Sub8 (r8 r).Value (mmu.Read8 (r16 ar).Value) (bitStateToValue F.C)
            PC.Value <- nextInstruction
        | SBC_R8_D8 (r, operand) ->
            (r8 r).Value <- alu.Sub8 (r8 r).Value operand (bitStateToValue F.C)
            PC.Value <- nextInstruction
        | CP_R8_R8  (r1,r2) ->
            alu.Compare8 (r8 r1).Value (r8 r2).Value
            PC.Value <- nextInstruction
        | CP_R8_AR16 (r, ar) ->
            alu.Compare8 (r8 r).Value (mmu.Read8 (r16 ar).Value)
            PC.Value <- nextInstruction
        | CP_R8_D8 (r, operand) ->
            alu.Compare8 (r8 r).Value operand
            PC.Value <- nextInstruction
        | INC_R16 (r) ->
            (r16 r).Update alu.Inc16
            PC.Value <- nextInstruction
        | DEC_R16 (r) ->
            (r16 r).Update alu.Dec16
            PC.Value <- nextInstruction
        | INC_R8 (r) ->
            (r8 r).Update alu.Inc8
            PC.Value <- nextInstruction
        | DEC_R8 (r) ->
            (r8 r).Update alu.Dec8
            PC.Value <- nextInstruction
        | SWAP_R8 (r) ->
            (r8 r).Update alu.SwapNibbles
            PC.Value <- nextInstruction
        | SWAP_AR16 (r) ->
            mmu.Update8 (r16 r).Value alu.SwapNibbles
            PC.Value <- nextInstruction
        | CPL ->
            A.Update alu.BitNot8
            PC.Value <- nextInstruction
        | RLC_R8 (r) ->
            (r8 r).Update alu.RotateLeftWithCarry8
            PC.Value <- nextInstruction
        | AND_R8_R8 (r1,r2) ->
            (r8 r1).Update (alu.And8 (r8 r2).Value)
            PC.Value <- nextInstruction
        | AND_R8_D8 (r,operand) ->
            (r8 r).Update (alu.And8 operand)
            PC.Value <- nextInstruction
        | AND_R8_AR16 (r,ar) ->
            (r8 r).Update (alu.And8 <| mmu.Read8 (r16 ar).Value)
            PC.Value <- nextInstruction
        | OR_R8_R8 (r1,r2) ->
            (r8 r1).Update (alu.Or8 (r8 r2).Value)
            PC.Value <- nextInstruction
        | OR_R8_D8 (r,operand) ->
            (r8 r).Update (alu.Or8 operand)
            PC.Value <- nextInstruction
        | OR_R8_AR16 (r,ar) ->
            (r8 r).Update (alu.Or8 <| mmu.Read8 (r16 ar).Value)
            PC.Value <- nextInstruction
        | XOR_R8_R8 (r1,r2) ->
            (r8 r1).Update (alu.Xor8 (r8 r2).Value)
            PC.Value <- nextInstruction
        | XOR_R8_D8 (r,operand) ->
            (r8 r).Update (alu.Xor8 operand)
            PC.Value <- nextInstruction
        | XOR_R8_AR16 (r,ar) ->
            (r8 r).Update (alu.Xor8 <| mmu.Read8 (r16 ar).Value)
            PC.Value <- nextInstruction
        (*
            Set/Clear/Test bits
        *)
        | SET_R8 (n,r) ->
            (r8 r).Update (setBit n)
            PC.Value <- nextInstruction
        | SET_AR16 (n,r) ->
            mmu.Update8 (r16 r).Value (setBit n)
            PC.Value <- nextInstruction
        | RES_R8 (n,r) ->
            (r8 r).Update (clearBit n) 
            PC.Value <- nextInstruction
        | RES_AR16 (n,r) ->
            mmu.Update8 (r16 r).Value (clearBit n)
            PC.Value <- nextInstruction
        | BIT_R8 (n,r) ->
            F.Z <- bitStateOf n (r8 r).Value |> bitStateInvert
            F.NH <- (CLEAR, SET)
            PC.Value <- nextInstruction
        | BIT_AR16 (n,r) ->
            F.Z <- bitStateOf n (mmu.Read8 (r16 r).Value) |> bitStateInvert
            F.NH <- (CLEAR, SET)
            PC.Value <- nextInstruction
        (*
            Jumps
        *)
        | JP_A16 (address) ->
            PC.Value <- address // Easiest instruction ever!
        | JP_AR16 (r) ->
            PC.Value <- mmu.Read16 (r16 r).Value
        | JP_F_A16 (f,address) ->
            PC.Value <-
                match (F.FlagFromName f) with
                | SET ->
                    longCycle <- true
                    address
                | CLEAR ->
                    nextInstruction
        | JP_NF_A16 (f,address) ->
            PC.Value <-
                match (F.FlagFromName f) with
                | SET ->
                    nextInstruction
                | CLEAR ->
                    longCycle <- true
                    address
        | JR_A8 (offset) ->
            PC.Value <- (int16 PC.Value) + (int16 offset) |> uint16 
        | JR_F_A8 (f, offset) ->
            PC.Value <-
                match (F.FlagFromName f) with
                | SET ->
                    longCycle <- true
                    (int16 PC.Value) + (int16 offset) |> uint16
                | CLEAR ->
                    nextInstruction
        | JR_NF_A8 (f, offset) ->
            PC.Value <-
                match (F.FlagFromName f) with
                | CLEAR ->
                    longCycle <- true
                    (int16 PC.Value) + (int16 offset) |> uint16
                | SET ->
                    nextInstruction
        | CALL_A16 (address) ->
            push16 nextInstruction
            PC.Value <- address
        | CALL_F_A16 (flag, address) ->
            PC.Value <-
                match (F.FlagFromName flag) with
                | SET ->
                    push16 nextInstruction
                    longCycle <- true
                    address
                | CLEAR ->
                    nextInstruction
        | CALL_NF_A16 (flag, address) ->
            PC.Value <-
                match (F.FlagFromName flag) with
                | SET ->
                    nextInstruction
                | CLEAR ->
                    push16 nextInstruction
                    longCycle <- true
                    address
        | RET ->
            PC.Value <- pop16 ()
        | RETI ->
            PC.Value <- pop16 ()
            MasterIE.Set
        | RET_F (flag) ->
            match (F.FlagFromName flag) with
            | SET ->
                PC.Value <- pop16 ()
                longCycle <- true
            | CLEAR ->
                PC.Value <- nextInstruction
        | RET_NF (flag) ->
            match (F.FlagFromName flag) with
            | SET ->
                PC.Value <- nextInstruction
            | CLEAR ->
                PC.Value <- pop16 ()
                longCycle <- true
                
        (*
            Misc
        *)
        | SCF ->
            F.NHC <- (CLEAR, CLEAR, SET)
            PC.Value <- nextInstruction
        | CCF ->
            F.NHC <- (CLEAR, CLEAR, bitStateInvert F.C)
            PC.Value <- nextInstruction
        | NOP ->
            PC.Value <- nextInstruction
        | STOP ->
            () // Do nooooothing
        | EI ->
            MasterIE.Set
            PC.Value <- nextInstruction
        | DI ->
            MasterIE.Clear
            PC.Value <- nextInstruction
        | DAA_R8 (r) ->
            let value = (r8 r).Value
            
            let lowBCD = value % 10uy
            let highBCD = ((value % 100uy) - lowBCD) / 10uy
            let result = (highBCD <<< 4) ||| lowBCD 

            F.Z <- setIfZero result
            F.H <- CLEAR
            F.C <- setIfTrue (value >= 100uy)

            (r8 r).Value <- result

            PC.Value <- nextInstruction
        | FGBC_PRINT_R8 (r) ->
            printfn "%d" (r8 r).Value
            PC.Value <- nextInstruction
        | FGBC_PRINTA_R8 (r) ->
            printf "%c" <| char (r8 r).Value 
            PC.Value <- nextInstruction
        | _ -> raise (System.Exception(sprintf "opcode <%O> not implemented" instruction))

        // Update clock
        clock.Tick (cycleCount instruction longCycle |> uint64)

        // Check for interrupts
        checkForTimerInterrupt ()
        
        match instruction with
        | STOP -> ()
        | _ -> execute ()

    member this.RaiseInterrupt (interrupt: Interrupt) =
        registers.MasterIE.Clear
        push16 PC.Value
        PC.Value <- match interrupt.Execute () with | InterruptHandler address -> address
    
    member this.Reset () =
        registers.A.Value <- 0x00uy
        registers.B.Value <- 0x00uy
        registers.C.Value <- 0x13uy
        registers.DE.Value <- 0x00D8us
        registers.HL.Value <- 0x014Dus
        registers.SP.Value <- 0xFFFEus
        registers.PC.Value <- 0x100us
        registers.MasterIE.Set

    member this.Start () =
        this.Reset()
        execute ()

    member this.Registers = registers


