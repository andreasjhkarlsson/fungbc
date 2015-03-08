namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Cpu
open Instruction
open Mmu
open Register

[<TestClass>]
type CpuTest() = 

    let runProgramAnd opcodes onComplete =
        let mmu = MMU()
        mmu.LoadBlob 0us (opcodes |> List.map uint8 |> List.toArray)
        mmu.Write8 (uint16 opcodes.Length) 0x10uy // Add stop instruction to halt program
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

    [<TestMethod>]
    member x.Test8BitImmediateLoads () = 
        runProgramAndCheckRegister8 [0x3E; 0x22] A 0x22
        runProgramAndCheckRegister8 [0x06; 0x33] B 0x33
        runProgramAndCheckRegister8 [0x0E; 0x44] C 0x44
        runProgramAndCheckRegister8 [0x16; 0x55] D 0x55
        runProgramAndCheckRegister8 [0x1E; 0x66] E 0x66
        runProgramAndCheckRegister8 [0x26; 0x77] H 0x77
        runProgramAndCheckRegister8 [0x2E; 0x88] L 0x88



        
