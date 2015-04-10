namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Cpu
open Instruction
open Mmu
open Register

[<TestClass>]
type InstructionTest() = 

    let runProgramAnd opcodes onComplete =
        let mmu = MMU()
        mmu.LoadBlob 0us (opcodes @ [0x10] |> List.map uint8 |> List.toArray)
        let cpu = CPU(mmu)
        cpu.Start()
        onComplete mmu cpu
        
    let runProgramAndCheckRegister8 opcodes register (expectedValue: int) =
        let checkResult mmu (cpu: CPU) =
            let register = cpu.Registers.From8Name register
            Assert.AreEqual(uint8 expectedValue, register.Value)
        runProgramAnd opcodes checkResult

    let runProgramAndCheckRegister16 opcodes register expectedValue =
        let checkResult mmu (cpu: CPU) =
            let register = cpu.Registers.From16Name register
            Assert.AreEqual(expectedValue, register.Value)
        runProgramAnd opcodes checkResult

    let ld8 name value =
        match name with
        | A -> [0x3E; value]
        | B -> [0x06; value]
        | C -> [0x0E; value]
        | D -> [0x16; value]
        | E -> [0x1E; value]
        | F -> raise (System.Exception("Cannot load F register"))
        | H -> [0x26; value]
        | L -> [0x2E; value]

    [<TestMethod>]
    member x.Test8BitImmediateLoads () = 
        let testLoad register =
            runProgramAndCheckRegister8 (ld8 register 0x22) register 0x22 // LD A, N     
        
        testLoad A
        testLoad B
        testLoad C
        testLoad D
        testLoad E
        testLoad H
        testLoad L

    [<TestMethod>]
    member x.Test8BitRegisterLoads () =
        let testLoad register opcode =
            runProgramAndCheckRegister8 ((ld8 register 0x33) @ [opcode]) register 0x33

        testLoad B 0x78  
        testLoad C 0x79
        testLoad D 0x7A
        testLoad E 0x7B
        testLoad H 0x7C
        testLoad L 0x7D
        testLoad A 0x7F

    
    [<TestMethod>]
    member x.TestInc () =

        let testForRegister register opcode =
            runProgramAndCheckRegister8 (ld8 register 0x9F @ [opcode]) register 0xA0 

        testForRegister A 0x3C
        testForRegister B 0x04
        testForRegister C 0x0C
        testForRegister D 0x14
        testForRegister E 0x1C
        testForRegister H 0x24
        testForRegister L 0x2C

    [<TestMethod>]
    member x.TestDec () =
        let testForRegister register opcode =
            runProgramAndCheckRegister8 (ld8 register 0xA1 @ [opcode]) register 0xA0 

        testForRegister A 0x3D
        testForRegister B 0x05
        testForRegister C 0x0D
        testForRegister D 0x15
        testForRegister E 0x1D
        testForRegister H 0x25
        testForRegister L 0x2D     




        
