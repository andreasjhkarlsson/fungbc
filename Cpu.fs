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

        PC.Value <- PC.Value + (uint16 instructionSize)

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
        | LD_R8_D8 (r,d) ->
            (r8 r).Value <- d
        | LD_A16_R8 (a,r) ->
            mmu.Write8 a ((r8 r).Value)
        | LD_R8_A16 (r,a) ->
            (r8 r).Value <- mmu.Read8 a
        | LD_R8_AR16 (r, ar) ->
            (r8 r).Value <- mmu.Read8 (r16 ar).Value
        | LD_AR16_R8 (ar, r) ->
            mmu.Write8 (r16 ar).Value (r8 r).Value
        | LD_AR16_D8 (ar, operand) ->
            mmu.Write8 (r16 ar).Value operand
        | LD_R16_D16 (r,value) ->
            (r16 r).Value <- value
        | LDI_R8_AR16 (r,ar) ->
            (r8 r).Value <- mmu.Read8 (r16 ar).Value
            (r16 ar).Update ((+) 1us)
        | LDI_AR16_R8 (ar,r) ->
            mmu.Write8 (r16 ar).Value (r8 r).Value
            (r16 ar).Update ((+) 1us)
        | LDD_R8_AR16 (r,ar) ->
            (r8 r).Value <- mmu.Read8 (r16 ar).Value
            (r16 ar).Value <- (r16 ar).Value - 1us
        | LDD_AR16_R8 (ar,r) ->
            mmu.Write8 (r16 ar).Value (r8 r).Value
            (r16 ar).Value <- (r16 ar).Value - 1us
        | LDH_A8_R8 (offset, r) ->
            mmu.Write8 (0xFF00us + (uint16 offset)) (r8 r).Value
        | LDH_R8_A8 (r, offset) ->
            (r8 r).Value <- mmu.Read8 (0xFF00us + (uint16 offset))
        | LDH_AR8_R8 (ar, r) ->
            mmu.Write8 (0xFF00us + (uint16 (r8 ar).Value)) (r8 r).Value
        | PUSH_R16 (r) ->
            push16 (r16 r).Value
        | POP_R16 (r) ->
            (r16 r).Value <- pop16 ()
        (*
            ALU operations
        *)
        | ADD_R8_R8 (r1,r2) ->
            (r8 r1).Value <- alu.Add8 (r8 r1).Value (r8 r2).Value 0uy
        | ADD_R8_D8 (r,operand) ->
            (r8 r).Value <- alu.Add8 (r8 r).Value operand 0uy
        | ADD_R8_AR16 (r,ar) ->
            (r8 r).Value <- alu.Add8 (r8 r).Value (mmu.Read8 (r16 ar).Value) 0uy
        | ADD_R16_R16 (r1,r2) ->
            (r16 r1).Value <- alu.Add16 (r16 r1).Value (r16 r2).Value
        | ADD_R16_D8 (r, operand) ->
            (r16 r).Value <- alu.Add16 (r16 r).Value (uint16 operand) // No idea if this sets correct flags.
        | LDHL_R16_D8 (r, operand) ->
            HL.Value <- alu.Add16 (r16 r).Value (uint16 operand) // No idea if this sets correct flags.
        | ADC_R8_R8 (r1, r2) ->
            (r8 r1).Value <- alu.Add8 (r8 r1).Value (r8 r2).Value (bitStateToValue F.C)
        | ADC_R8_D8 (r, operand) ->
            (r8 r).Value <- alu.Add8 (r8 r).Value operand (bitStateToValue F.C)
        | ADC_R8_AR16 (r, ar) ->
            (r8 r).Value <- alu.Add8 (r8 r).Value (mmu.Read8 (r16 ar).Value) (bitStateToValue F.C)
        | SUB_R8_R8 (r1,r2) ->
            (r8 r1).Value <- alu.Sub8 (r8 r1).Value (r8 r2).Value 0uy
        | SUB_R8_AR16 (r,ar) ->
            (r8 r).Value <- alu.Sub8 (r8 r).Value (mmu.Read8 (r16 ar).Value) 0uy
        | SUB_R8_D8 (r, operand) ->
            (r8 r).Value <- alu.Sub8 (r8 r).Value operand 0uy
        | SBC_R8_R8 (r1, r2) ->
            (r8 r1).Value <- alu.Sub8 (r8 r1).Value (r8 r2).Value (bitStateToValue F.C)
        | SBC_R8_AR16 (r, ar) ->
            (r8 r).Value <- alu.Sub8 (r8 r).Value (mmu.Read8 (r16 ar).Value) (bitStateToValue F.C)
        | SBC_R8_D8 (r, operand) ->
            (r8 r).Value <- alu.Sub8 (r8 r).Value operand (bitStateToValue F.C)
        | CP_R8_R8  (r1,r2) ->
            alu.Compare8 (r8 r1).Value (r8 r2).Value
        | CP_R8_AR16 (r, ar) ->
            alu.Compare8 (r8 r).Value (mmu.Read8 (r16 ar).Value)
        | CP_R8_D8 (r, operand) ->
            alu.Compare8 (r8 r).Value operand
        | INC_R16 (r) ->
            (r16 r).Update alu.Inc16
        | DEC_R16 (r) ->
            (r16 r).Update alu.Dec16
        | INC_R8 (r) ->
            (r8 r).Update alu.Inc8
        | DEC_R8 (r) ->
            (r8 r).Update alu.Dec8
        | SWAP_R8 (r) ->
            (r8 r).Update alu.SwapNibbles
        | SWAP_AR16 (r) ->
            mmu.Update8 (r16 r).Value alu.SwapNibbles
        | CPL ->
            A.Update alu.BitNot8
        | RLC_R8 (r) ->
            (r8 r).Update alu.RotateLeftWithCarry8
        | AND_R8_R8 (r1,r2) ->
            (r8 r1).Update (alu.And8 (r8 r2).Value)
        | AND_R8_D8 (r,operand) ->
            (r8 r).Update (alu.And8 operand)
        | AND_R8_AR16 (r,ar) ->
            (r8 r).Update (alu.And8 <| mmu.Read8 (r16 ar).Value)
        | OR_R8_R8 (r1,r2) ->
            (r8 r1).Update (alu.Or8 (r8 r2).Value)
        | OR_R8_D8 (r,operand) ->
            (r8 r).Update (alu.Or8 operand)
        | OR_R8_AR16 (r,ar) ->
            (r8 r).Update (alu.Or8 <| mmu.Read8 (r16 ar).Value)
        | XOR_R8_R8 (r1,r2) ->
            (r8 r1).Update (alu.Xor8 (r8 r2).Value)
        | XOR_R8_D8 (r,operand) ->
            (r8 r).Update (alu.Xor8 operand)
        | XOR_R8_AR16 (r,ar) ->
            (r8 r).Update (alu.Xor8 <| mmu.Read8 (r16 ar).Value)
        (*
            Set/Clear/Test bits
        *)
        | SET_R8 (n,r) ->
            (r8 r).Update (setBit n)
        | SET_AR16 (n,r) ->
            mmu.Update8 (r16 r).Value (setBit n)
        | RES_R8 (n,r) ->
            (r8 r).Update (clearBit n) 
        | RES_AR16 (n,r) ->
            mmu.Update8 (r16 r).Value (clearBit n)
        | BIT_R8 (n,r) ->
            F.Z <- bitStateOf n (r8 r).Value |> bitStateInvert
            F.NH <- (CLEAR, SET)
        | BIT_AR16 (n,r) ->
            F.Z <- bitStateOf n (mmu.Read8 (r16 r).Value) |> bitStateInvert
            F.NH <- (CLEAR, SET)
        (*
            Jumps
        *)
        | JP_A16 (address) ->
            PC.Value <- address // Easiest instruction ever!
        | JP_AR16 (r) ->
            PC.Value <- mmu.Read16 (r16 r).Value
        | JP_F_A16 (f,address) ->
            match (F.FlagFromName f) with
            | SET ->
                longCycle <- true
                PC.Value <- address
            | CLEAR ->
                ()
        | JP_NF_A16 (f,address) ->
            match (F.FlagFromName f) with
            | SET ->
                ()
            | CLEAR ->
                longCycle <- true
                PC.Value <- address
        | JR_A8 (offset) ->
            PC.Value <- (int16 PC.Value) + (int16 offset) |> uint16 
        | JR_F_A8 (f, offset) ->
            match (F.FlagFromName f) with
            | SET ->
                longCycle <- true
                PC.Value <- (int16 PC.Value) + (int16 offset) |> uint16
            | CLEAR ->
                ()
        | JR_NF_A8 (f, offset) ->
            match (F.FlagFromName f) with
            | CLEAR ->
                longCycle <- true
                PC.Value <- (int16 PC.Value) + (int16 offset) |> uint16
            | SET ->
                ()
        | CALL_A16 (address) ->
            push16 PC.Value
            PC.Value <- address
        | CALL_F_A16 (flag, address) ->
            match (F.FlagFromName flag) with
            | SET ->
                push16 PC.Value
                longCycle <- true
                PC.Value <- address
            | CLEAR ->
                ()
        | CALL_NF_A16 (flag, address) ->  
            match (F.FlagFromName flag) with
            | SET ->
                ()
            | CLEAR ->
                push16 PC.Value
                longCycle <- true
                PC.Value <- address
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
                ()
        | RET_NF (flag) ->
            match (F.FlagFromName flag) with
            | SET ->
                ()
            | CLEAR ->
                PC.Value <- pop16 ()
                longCycle <- true
                
        (*
            Misc
        *)
        | SCF ->
            F.NHC <- (CLEAR, CLEAR, SET)
        | CCF ->
            F.NHC <- (CLEAR, CLEAR, bitStateInvert F.C)
        | NOP ->
            ()
        | STOP ->
            () // Do nooooothing
        | EI ->
            MasterIE.Set
        | DI ->
            MasterIE.Clear
        | DAA_R8 (r) ->
            let value = (r8 r).Value
            
            let lowBCD = value % 10uy
            let highBCD = ((value % 100uy) - lowBCD) / 10uy
            let result = (highBCD <<< 4) ||| lowBCD 

            F.Z <- setIfZero result
            F.H <- CLEAR
            F.C <- setIfTrue (value >= 100uy)

            (r8 r).Value <- result

        | FGBC_PRINT_R8 (r) ->
            printfn "%d" (r8 r).Value
        | FGBC_PRINTA_R8 (r) ->
            printf "%c" <| char (r8 r).Value 
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


