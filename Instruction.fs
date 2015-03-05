module Instruction

open Register
open Mmu
open Misc

// "Syntax" for instruction name:
// NAME_OPERAND1TYPE_OPERAND2TYPE
// Operand types:
// R8   = 8 bit register
// R16  = 16 bit register
// D8   = 8 bit value
// D16  = 16 bit value
// A16  = 16 bit address
// A8   = 8 bit address (offset)
// AR8  = 8 bit address in register (offset)
// AR16 = 16 bit address in register
type Instruction = 
    | NOP                                                       // Skip
    | HALT                                                      // Halt CPU until interrupt
    | STOP                                                      // Halt CPU
    | SWAP_R8        of Register8Name                           // Swap nibbles in register
    | SWAP_AR16      of Register16Name                          // Swap nibbles in address pointed by 16 bit register
    | LD_R8_D8       of Register8Name*uint8                     // Load 8 bit value into 8 bit register                   
    | LD_R8_A16      of Register8Name*MemoryAddress             // Load 8 bit pointed value into 8 bit register
    | LD_A16_R8      of MemoryAddress*Register8Name             // Store value in 8 bit register into address               
    | LD_R8_R8       of Register8Name*Register8Name             // Load value in 8 bit register to another register
    | LD_R16_D16     of Register16Name*uint16                   // Load 16 bit value into 16 bit register 
    | LD_R16_R16     of Register16Name*Register16Name           // Load value in 16 bit register in 16 bit register           
    | LD_R16_A16     of Register16Name*MemoryAddress            // Load 16 bit pointed value into 16 bit register
    | LD_AR16_D16    of Register16Name*uint16                   // Store 16 bit value into address in 16 bit register
    | LD_AR16_R8     of Register16Name*Register8Name            // Store value in 8 bit register into adress in 16 bit register
    | LD_AR16_D8     of Register16Name*uint8                    // Store 8 bit value in address in 16 bit register
    | LD_R8_AR16     of Register8Name*Register16Name            // Load value in address in 16 bit register into 8 bit register
    | LD_A16_R16     of MemoryAddress*Register16Name            // Store value in 16 bit register into address
    | LD_R16_D8      of Register16Name*uint8                    // Load 8 bit value into 16 bit register
    | LD_R16_R8      of Register16Name*Register8Name            // Load value in 8 bit register into 16 bit register
    | LDI_AR16_R8    of Register16Name*Register8Name            // Store value in 8 bit register into address in 16 bit register and then increment 16 bit register
    | LDD_AR16_R8    of Register16Name*Register8Name            // Store value in 8 bit register into address in 16 bit register and then decrement 16 bit register
    | LDI_R8_AR16    of Register8Name*Register16Name            // Load value in address in 16 register into 8 bit register and increment 16 bit register
    | LDD_R8_AR16    of Register8Name*Register16Name            // Load value in address in 16 register into 8 bit register and decrement 16 bit register
    | LDH_R8_A8      of Register8Name*uint8                     // Load value in address (FF00 + 8 bit address) in 8 bit register
    | LDH_A8_R8      of uint8*Register8Name                     // Store value in 8 bit register in address (FF00 + 8 bit address)
    | LDH_AR8_R8     of Register8Name*Register8Name             // Store value in 8 bit register in address (FF00 + 8 bit register)
    | LDHL_R16_D8    of Register16Name*uint8                    // Add signed 8 bit value to SP and copy SP to 16 bit register
    | INC_R8         of Register8Name                           // Inc value in 8 bit register
    | INC_R16        of Register16Name                          // Inc value in 16 bit register
    | INC_AR16       of Register16Name                          // Inc value address in 16 bit register
    | DEC_R8         of Register8Name                           // Dec value in 8 bit register
    | DEC_R16        of Register16Name                          // Dec value in 16 bit register
    | DEC_AR16       of Register16Name                          // Dec value address in 16 bit register
    | SCF                                                       // Set carry flag
    | CCF                                                       // Clear carry flag


let decodeOpcode (mmu: MMU) address =
    
    let int8Operand () = mmu.read8 (address + 1us)
    let int16Operand () = mmu.read16 (address + 1us)

    let opcode = mmu.read8 address
    match int opcode with
    | 0x00 -> NOP
    | 0x01 -> LD_R16_D16    (BC,int16Operand ())
    | 0x02 -> LD_AR16_R8    (BC,A)
    | 0x03 -> INC_R16       (BC)
    | 0x04 -> INC_R8        (B)
    | 0x05 -> DEC_R8        (B)
    | 0x06 -> LD_R8_D8      (B,int8Operand ())
    | 0x0A -> LD_R8_AR16    (A,BC)
    | 0x0B -> DEC_R16       (BC)
    | 0x0C -> INC_R8        (C)
    | 0x0D -> DEC_R8        (C)
    | 0x0E -> LD_R8_D8      (C,int8Operand ())
    | 0x10 -> STOP
    | 0x11 -> LD_R16_D16    (DE,int16Operand ())
    | 0x12 -> LD_AR16_R8    (DE,A)
    | 0x13 -> INC_R16       (DE)
    | 0x14 -> INC_R8        (D)
    | 0x15 -> DEC_R8        (D)
    | 0x16 -> LD_R8_D8      (D,int8Operand ())
    | 0x1A -> LD_R8_AR16    (A,DE)
    | 0x1B -> DEC_R16       (DE)
    | 0x1C -> INC_R8        (E)
    | 0x1D -> DEC_R8        (E)
    | 0x1E -> LD_R8_D8      (E,int8Operand ())
    | 0x21 -> LD_R16_D16    (HL,int16Operand ())
    | 0x22 -> LDI_AR16_R8   (HL,A)
    | 0x23 -> INC_R16       (HL)
    | 0x24 -> INC_R8        (H)
    | 0x25 -> DEC_R8        (H)
    | 0x26 -> LD_R8_D8      (H,int8Operand ())
    | 0x2A -> LDI_R8_AR16   (A,HL)
    | 0x2B -> DEC_R16       (HL)
    | 0x2C -> INC_R8        (L)
    | 0x2D -> DEC_R8        (L)
    | 0x2E -> LD_R8_D8      (L,int8Operand ())
    | 0x31 -> LD_R16_D16    (SP,int16Operand ())
    | 0x32 -> LDD_AR16_R8   (HL,A)
    | 0x33 -> INC_R16       (SP)
    | 0x34 -> INC_AR16      (HL)
    | 0x35 -> DEC_AR16      (HL)
    | 0x36 -> LD_AR16_D8    (HL,int8Operand ())
    | 0x37 -> SCF
    | 0x3A -> LDD_R8_AR16   (A,HL)
    | 0x3B -> DEC_R16       (SP)
    | 0x3C -> INC_R8        (A)
    | 0x3D -> DEC_R8        (A)
    | 0x3E -> LD_R8_D8      (A,int8Operand ())
    | 0x3F -> CCF
    | 0x40 -> LD_R8_R8      (B,B)
    | 0x41 -> LD_R8_R8      (B,C)
    | 0x42 -> LD_R8_R8      (B,D)
    | 0x43 -> LD_R8_R8      (B,E)
    | 0x44 -> LD_R8_R8      (B,H)
    | 0x45 -> LD_R8_R8      (B,L)
    | 0x46 -> LD_R8_AR16    (B,HL)
    | 0x47 -> LD_R8_R8      (B,A)
    | 0x48 -> LD_R8_R8      (C,B)
    | 0x49 -> LD_R8_R8      (C,C)
    | 0x4A -> LD_R8_R8      (C,D)
    | 0x4B -> LD_R8_R8      (C,E)
    | 0x4C -> LD_R8_R8      (C,H)
    | 0x4D -> LD_R8_R8      (C,L)
    | 0x4E -> LD_R8_AR16    (C,HL)
    | 0x4F -> LD_R8_R8      (C,A)
    | 0x50 -> LD_R8_R8      (D,B)
    | 0x51 -> LD_R8_R8      (D,C)
    | 0x52 -> LD_R8_R8      (D,D)
    | 0x53 -> LD_R8_R8      (D,E)
    | 0x54 -> LD_R8_R8      (D,H)
    | 0x55 -> LD_R8_R8      (D,L)
    | 0x56 -> LD_R8_AR16    (D,HL)
    | 0x57 -> LD_R8_R8      (D,A)
    | 0x58 -> LD_R8_R8      (E,B)
    | 0x59 -> LD_R8_R8      (E,C)
    | 0x5A -> LD_R8_R8      (E,D)
    | 0x5B -> LD_R8_R8      (E,E)
    | 0x5C -> LD_R8_R8      (E,H)
    | 0x5D -> LD_R8_R8      (E,L)
    | 0x5E -> LD_R8_AR16    (E,HL)
    | 0x5F -> LD_R8_R8      (E,A)
    | 0x60 -> LD_R8_R8      (H,B)
    | 0x61 -> LD_R8_R8      (H,C)
    | 0x62 -> LD_R8_R8      (H,D)
    | 0x63 -> LD_R8_R8      (H,E)
    | 0x64 -> LD_R8_R8      (H,H)
    | 0x65 -> LD_R8_R8      (H,L)
    | 0x66 -> LD_R8_AR16    (H,HL)
    | 0x67 -> LD_R8_R8      (H,A)
    | 0x68 -> LD_R8_R8      (L,B)
    | 0x69 -> LD_R8_R8      (L,C)
    | 0x6A -> LD_R8_R8      (L,D)
    | 0x6B -> LD_R8_R8      (L,E)
    | 0x6C -> LD_R8_R8      (L,H)
    | 0x6D -> LD_R8_R8      (L,L)
    | 0x6E -> LD_R8_AR16    (L,HL)
    | 0x6F -> LD_R8_R8      (L,A)
    | 0x70 -> LD_AR16_R8    (HL,B)
    | 0x71 -> LD_AR16_R8    (HL,C)
    | 0x72 -> LD_AR16_R8    (HL,D)
    | 0x73 -> LD_AR16_R8    (HL,E)
    | 0x74 -> LD_AR16_R8    (HL,H)
    | 0x75 -> LD_AR16_R8    (HL,L)
    | 0x76 -> HALT
    | 0x77 -> LD_AR16_R8    (HL,A)
    | 0x78 -> LD_R8_R8      (A,B)
    | 0x79 -> LD_R8_R8      (A,C)
    | 0x7A -> LD_R8_R8      (A,D)
    | 0x7B -> LD_R8_R8      (A,E)
    | 0x7C -> LD_R8_R8      (A,H)
    | 0x7D -> LD_R8_R8      (A,L)
    | 0x7E -> LD_R8_AR16    (A,HL)
    | 0x7F -> LD_R8_R8      (A,A)
    | 0xCB ->
        match int <| int8Operand() with
        | 0x30 -> SWAP_R8        (B)
        | 0x31 -> SWAP_R8        (C)
        | 0x32 -> SWAP_R8        (D)
        | 0x33 -> SWAP_R8        (E)
        | 0x34 -> SWAP_R8        (H)
        | 0x35 -> SWAP_R8        (L)
        | 0x36 -> SWAP_AR16      (HL)
        | 0x37 -> SWAP_R8        (A)
        | _ as extended -> raise (System.Exception(sprintf "decoder for extended opcode <%d %d> not implemented" opcode extended)) 
    | 0xE0 -> LDH_A8_R8     (int8Operand (), A)
    | 0xE2 -> LDH_AR8_R8    (C, A)
    | 0xEA -> LD_A16_R8     (uint16 <| int16Operand(), A)
    | 0xF0 -> LDH_R8_A8     (A,int8Operand ())
    | 0xF8 -> LDHL_R16_D8   (SP,int8Operand ())
    | 0xF9 -> LD_R16_R16    (SP,HL)
    | 0xFA -> LD_R8_A16     (A,uint16 <| int16Operand ())
    | _ -> raise (System.Exception(sprintf "decoder for opcode 0x%02X> not implemented" opcode))

let readable instruction = GetUnionCaseName instruction