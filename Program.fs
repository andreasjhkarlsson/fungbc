
open Cpu
open Mmu

[<EntryPoint>]
let main argv = 
    
    let mmu = MMU()

    let cpu = CPU(mmu)


    let program = [
                    0x3E; 0x00;         // LD A, 0x00
                    0xCB; 0xC7;
                    0xCB; 0xCF;
                    0xCB; 0x6F;
                    0xEA; 0xFA; 0x0;    // LD (0xFA), A
                    0xFA; 0xFA; 0x0;    // LD A, (0xFA)
                    0x37;
                    0x10;               // STOP
                  ]|> List.map uint8 |> List.toArray

    mmu.LoadBlob 0us program

    cpu.Start ()

    cpu.Registers.Print ()

    mmu.PrintDump 0x0 0xFF

    0
