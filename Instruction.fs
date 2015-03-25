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
    | LD_AR16_R8     of Register16Name*Register8Name            // Store value in 8 bit register into adress in 16 bit register
    | LD_AR16_D8     of Register16Name*uint8                    // Store 8 bit value in address in 16 bit register
    | LD_R8_AR16     of Register8Name*Register16Name            // Load value in address in 16 bit register into 8 bit register
    | LD_A16_R16     of MemoryAddress*Register16Name            // Store value in 16 bit register into address
    | LDI_AR16_R8    of Register16Name*Register8Name            // Store value in 8 bit register into address in 16 bit register and then increment 16 bit register
    | LDD_AR16_R8    of Register16Name*Register8Name            // Store value in 8 bit register into address in 16 bit register and then decrement 16 bit register
    | LDI_R8_AR16    of Register8Name*Register16Name            // Load value in address in 16 register into 8 bit register and increment 16 bit register
    | LDD_R8_AR16    of Register8Name*Register16Name            // Load value in address in 16 register into 8 bit register and decrement 16 bit register
    | LDH_R8_A8      of Register8Name*uint8                     // Load value in address (FF00 + 8 bit address) in 8 bit register
    | LDH_A8_R8      of uint8*Register8Name                     // Store value in 8 bit register in address (FF00 + 8 bit address)
    | LDH_AR8_R8     of Register8Name*Register8Name             // Store value in 8 bit register in address (FF00 + 8 bit register)
    | LDHL_R16_D8    of Register16Name*int8                     // Add signed 8 bit value to SP and copy SP to 16 bit register
    | INC_R8         of Register8Name                           // Inc value in 8 bit register
    | INC_R16        of Register16Name                          // Inc value in 16 bit register
    | INC_AR16       of Register16Name                          // Inc value address in 16 bit register
    | DEC_R8         of Register8Name                           // Dec value in 8 bit register
    | DEC_R16        of Register16Name                          // Dec value in 16 bit register
    | DEC_AR16       of Register16Name                          // Dec value address in 16 bit register
    | SCF                                                       // Set carry flag
    | CCF                                                       // Clear carry flag
    | BIT_R8         of int*Register8Name                       // Test bit n in 8 bit register
    | BIT_AR16       of int*Register16Name                      // Test bit n in address in 16 bit register 
    | RES_R8         of int*Register8Name                       // Clear bit n in 8 bit register
    | RES_AR16       of int*Register16Name                      // Clear bit n in address in 16 bit register 
    | SET_R8         of int*Register8Name                       // Set bit n in 8 bit register
    | SET_AR16       of int*Register16Name                      // Set bit n in address in 16 bit register 
    | CPL                                                       // Bitwise NOT on register A
    | RLCA                                                      // Rotate A left with carry                                                
    | JP_A16         of MemoryAddress                           // Absolute jump to address
    | JP_AR16        of Register16Name                          // Jump to address in address in 16 bit register (erhh)
    | JP_F_A16       of FlagName*MemoryAddress                  // Jump to address if flag is set
    | JP_NF_A16      of FlagName*MemoryAddress                  // Jump to address if flag is not set
    | JR_A8          of int8                                    // Relative jump with signed offset
    | JR_F_A8        of FlagName*int8                           // Relative jump with signed offset if flag is set
    | JR_NF_A8       of FlagName*int8                           // Relative jump with signed offset if flag is not set
    | ADD_R8_R8      of Register8Name*Register8Name             // Add 8 bit register to 8 bit register
    | ADD_R8_D8      of Register8Name*uint8                     // Add 8 bit value to 8 bit register
    | ADD_R8_AR16    of Register8Name*Register16Name            // Add value pointed by 16 bit register to 8 bit register
    | ADD_R16_R16    of Register16Name*Register16Name           // Add 16 bit register to 16 bit register
    | ADD_R16_D8     of Register16Name*int8                     // Add signed 8 bit value to 16 bit register
    | ADC_R8_R8      of Register8Name*Register8Name             // Add 8 bit register to 8 bit register with carry
    | ADC_R8_D8      of Register8Name*uint8                     // Add 8 bit value to 8 bit register with carry
    | ADC_R8_AR16    of Register8Name*Register16Name            // Add value in address in 16 bit register to 8 bit register with carry
    | SUB_R8_R8      of Register8Name*Register8Name             // Subtract value in 8 bit register with 8 bit register
    | SUB_R8_D8      of Register8Name*uint8                     // Subtract 8 bit value from 8 bit register
    | SUB_R8_AR16    of Register8Name*Register16Name            // Subtract value in address in 16 bit register from 8 bit register
    | SBC_R8_R8      of Register8Name*Register8Name             // Subtract value in 8 bit register + carry from 8 bit register 
    | SBC_R8_AR16    of Register8Name*Register16Name            // Subtract value in address in 16 bit register + carry from 8 bit register 
    | SBC_R8_D8      of Register8Name*uint8                     // Subtract 8 bit value + carry from 8 bit register
    | AND_R8_R8      of Register8Name*Register8Name             // Bitwise AND between 8 bit registers
    | AND_R8_D8      of Register8Name*uint8                     // Bitwise AND between 8 bit register and 8 bit value
    | AND_R8_AR16    of Register8Name*Register16Name            // Bitwise AND between 8 bit register and value in address in 16 bit register
    | OR_R8_R8       of Register8Name*Register8Name             // Bitwise OR between 8 bit registers
    | OR_R8_D8       of Register8Name*uint8                     // Bitwise OR between 8 bit register and 8 bit value
    | OR_R8_AR16     of Register8Name*Register16Name            // Bitwise OR between 8 bit register and value in address in 16 bit register
    | XOR_R8_R8      of Register8Name*Register8Name             // Bitwise XOR between 8 bit registers
    | XOR_R8_D8      of Register8Name*uint8                     // Bitwise XOR between 8 bit register and 8 bit value
    | XOR_R8_AR16    of Register8Name*Register16Name            // Bitwise XOR between 8 bit register and value in address in 16 bit register
    | FGBC_PRINT_R8  of Register8Name                           // Print register to STDOUT (FunGBC debug extension) (with newline)
    | FGBC_PRINTA_R8 of Register8Name                           // Print ascii character in register
    | EI                                                        // Enabled interrupts
    | DI                                                        // Disable interrupts
    | CP_R8_R8       of Register8Name*Register8Name             // Compare 8 bit register with 8 bit register
    | CP_R8_AR16     of Register8Name*Register16Name            // Compare 8 bit register with value in address in 16 bit register
    | CP_R8_D8       of Register8Name*uint8                     // Compare 8 bit register with 8 bit value
    | DAA_R8         of Register8Name                           // Converts 8 bit register into packed BCD
    | PUSH_R16       of Register16Name                          // Push 16 bit register onto stack
    | POP_R16        of Register16Name                          // Pop 16 bit value from stack into 16 bit register
    | CALL_A16       of MemoryAddress                           // Call routine at address
    | CALL_F_A16     of FlagName*MemoryAddress                  // Call routine at address if flag is set
    | CALL_NF_A16    of FlagName*MemoryAddress                  // Call routine at address if flag is not set
    | RET                                                       // Return from subroutine
    | RETI                                                      // Return from subroutine and enable interrupts
    | RET_F         of FlagName                                 // Return if flag is set
    | RET_NF        of FlagName                                 // Return if flag is not set
    | RST           of MemoryAddress                            // Restart at address (simple call to address)
    

// Parse opcode at address and extract operands.
let decodeOpcode (mmu: MMU) address =
    
    let mutable consumed = 0

    let int8Operand () = mmu.Read8 (address + 1us)
    let int16Operand () = mmu.Read16 (address + 1us)

    let opcode = mmu.Read8 address

    match int opcode with
    | 0x00 -> NOP
    | 0x01 -> LD_R16_D16    (BC,int16Operand ())
    | 0x02 -> LD_AR16_R8    (BC,A)
    | 0x03 -> INC_R16       (BC)
    | 0x04 -> INC_R8        (B)
    | 0x05 -> DEC_R8        (B)
    | 0x06 -> LD_R8_D8      (B,int8Operand ())
    | 0x07 -> RLCA
    | 0x09 -> ADD_R16_R16   (HL,BC)
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
    | 0x19 -> ADD_R16_R16   (HL,DE)
    | 0x18 -> JR_A8         (int8Operand () |> int8)
    | 0x1A -> LD_R8_AR16    (A,DE)
    | 0x1B -> DEC_R16       (DE)
    | 0x1C -> INC_R8        (E)
    | 0x1D -> DEC_R8        (E)
    | 0x1E -> LD_R8_D8      (E,int8Operand ())
    | 0x20 -> JR_NF_A8      (Z, int8Operand () |> int8)
    | 0x21 -> LD_R16_D16    (HL,int16Operand ())
    | 0x22 -> LDI_AR16_R8   (HL,A)
    | 0x23 -> INC_R16       (HL)
    | 0x24 -> INC_R8        (H)
    | 0x25 -> DEC_R8        (H)
    | 0x26 -> LD_R8_D8      (H,int8Operand ())
    | 0x27 -> DAA_R8        (A)       
    | 0x28 -> JR_F_A8       (Z, int8Operand () |> int8)
    | 0x29 -> ADD_R16_R16   (HL,HL)
    | 0x2A -> LDI_R8_AR16   (A,HL)
    | 0x2B -> DEC_R16       (HL)
    | 0x2C -> INC_R8        (L)
    | 0x2D -> DEC_R8        (L)
    | 0x2E -> LD_R8_D8      (L,int8Operand ())
    | 0x2F -> CPL
    | 0x30 -> JR_NF_A8      (FlagName.C, int8Operand () |> int8)
    | 0x31 -> LD_R16_D16    (SP,int16Operand ())
    | 0x32 -> LDD_AR16_R8   (HL,A)
    | 0x33 -> INC_R16       (SP)
    | 0x34 -> INC_AR16      (HL)
    | 0x35 -> DEC_AR16      (HL)
    | 0x36 -> LD_AR16_D8    (HL,int8Operand ())
    | 0x37 -> SCF
    | 0x38 -> JR_F_A8       (FlagName.C, int8Operand () |> int8)
    | 0x39 -> ADD_R16_R16   (HL,SP)
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
    | 0x80 -> ADD_R8_R8     (A,B)
    | 0x81 -> ADD_R8_R8     (A,C)
    | 0x82 -> ADD_R8_R8     (A,D)
    | 0x83 -> ADD_R8_R8     (A,E)
    | 0x84 -> ADD_R8_R8     (A,H)
    | 0x85 -> ADD_R8_R8     (A,L)
    | 0x86 -> ADD_R8_AR16   (A,HL)
    | 0x87 -> ADD_R8_R8     (A,A)
    | 0x88 -> ADC_R8_R8     (A,B)
    | 0x89 -> ADC_R8_R8     (A,C)
    | 0x8A -> ADC_R8_R8     (A,D)
    | 0x8B -> ADC_R8_R8     (A,E)
    | 0x8C -> ADC_R8_R8     (A,H)
    | 0x8D -> ADC_R8_R8     (A,L)
    | 0x8E -> ADC_R8_AR16   (A,HL)
    | 0x8F -> ADC_R8_R8     (A,A)
    | 0x90 -> SUB_R8_R8     (A,B)
    | 0x91 -> SUB_R8_R8     (A,C)
    | 0x92 -> SUB_R8_R8     (A,D)
    | 0x93 -> SUB_R8_R8     (A,E)
    | 0x94 -> SUB_R8_R8     (A,H)
    | 0x95 -> SUB_R8_R8     (A,L)
    | 0x96 -> SUB_R8_AR16   (A,HL)
    | 0x97 -> SUB_R8_R8     (A,A) 
    | 0x98 -> SBC_R8_R8     (A,B)
    | 0x99 -> SBC_R8_R8     (A,C)
    | 0x9A -> SBC_R8_R8     (A,D)
    | 0x9B -> SBC_R8_R8     (A,E)
    | 0x9C -> SBC_R8_R8     (A,H)
    | 0x9D -> SBC_R8_R8     (A,L)
    | 0x9E -> SBC_R8_AR16   (A,HL)
    | 0x9F -> SBC_R8_R8     (A,A)
    | 0xA0 -> AND_R8_R8     (A,B)
    | 0xA1 -> AND_R8_R8     (A,C)
    | 0xA2 -> AND_R8_R8     (A,D)
    | 0xA3 -> AND_R8_R8     (A,E)
    | 0xA4 -> AND_R8_R8     (A,H)
    | 0xA5 -> AND_R8_R8     (A,L)
    | 0xA6 -> AND_R8_AR16   (A,HL)
    | 0xA7 -> AND_R8_R8     (A,A)    
    | 0xA8 -> XOR_R8_R8     (A,B)
    | 0xA9 -> XOR_R8_R8     (A,C)
    | 0xAA -> XOR_R8_R8     (A,D)
    | 0xAB -> XOR_R8_R8     (A,E)
    | 0xAC -> XOR_R8_R8     (A,H)
    | 0xAD -> XOR_R8_R8     (A,L)
    | 0xAE -> XOR_R8_AR16   (A,HL)
    | 0xAF -> XOR_R8_R8     (A,A)
    | 0xB0 -> OR_R8_R8      (A,B)
    | 0xB1 -> OR_R8_R8      (A,C)
    | 0xB2 -> OR_R8_R8      (A,D)
    | 0xB3 -> OR_R8_R8      (A,E)
    | 0xB4 -> OR_R8_R8      (A,H)
    | 0xB5 -> OR_R8_R8      (A,L)
    | 0xB6 -> OR_R8_AR16    (A,HL)
    | 0xB7 -> OR_R8_R8      (A,A)  
    | 0xB8 -> CP_R8_R8      (A,B)
    | 0xB9 -> CP_R8_R8      (A,C)
    | 0xBA -> CP_R8_R8      (A,D)
    | 0xBB -> CP_R8_R8      (A,E)
    | 0xBC -> CP_R8_R8      (A,H)
    | 0xBD -> CP_R8_R8      (A,L)
    | 0xBE -> CP_R8_AR16    (A,HL)
    | 0xBF -> CP_R8_R8      (A,A)
    | 0xC0 -> RET_NF        (Z)
    | 0xC1 -> POP_R16       (BC)
    | 0xC2 -> JP_NF_A16     (Z,int16Operand ())
    | 0xC3 -> JP_A16        (int16Operand())
    | 0xC4 -> CALL_NF_A16   (Z, int16Operand ())
    | 0xC5 -> PUSH_R16      (BC)
    | 0xC6 -> ADD_R8_D8     (A, int8Operand ())
    | 0xC7 -> RST           (0x0000us)
    | 0xC8 -> RET_F         (Z)
    | 0xC9 -> RET
    | 0xCA -> JP_F_A16      (Z, int16Operand ())
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
        | 0x40 -> BIT_R8         (0,B)
        | 0x41 -> BIT_R8         (0,C)
        | 0x42 -> BIT_R8         (0,D)
        | 0x43 -> BIT_R8         (0,E)
        | 0x44 -> BIT_R8         (0,H)
        | 0x45 -> BIT_R8         (0,L)
        | 0x46 -> BIT_AR16       (0,HL)
        | 0x47 -> BIT_R8         (0,A)
        | 0x48 -> BIT_R8         (1,B)
        | 0x49 -> BIT_R8         (1,C)
        | 0x4A -> BIT_R8         (1,D)
        | 0x4B -> BIT_R8         (1,E)
        | 0x4C -> BIT_R8         (1,H)
        | 0x4D -> BIT_R8         (1,L)
        | 0x4E -> BIT_AR16       (1,HL)
        | 0x4F -> BIT_R8         (1,A)
        | 0x50 -> BIT_R8         (2,B)
        | 0x51 -> BIT_R8         (2,C)
        | 0x52 -> BIT_R8         (2,D)
        | 0x53 -> BIT_R8         (2,E)
        | 0x54 -> BIT_R8         (2,H)
        | 0x55 -> BIT_R8         (2,L)
        | 0x56 -> BIT_AR16       (2,HL)
        | 0x57 -> BIT_R8         (2,A)
        | 0x58 -> BIT_R8         (3,B)
        | 0x59 -> BIT_R8         (3,C)
        | 0x5A -> BIT_R8         (3,D)
        | 0x5B -> BIT_R8         (3,E)
        | 0x5C -> BIT_R8         (3,H)
        | 0x5D -> BIT_R8         (3,L)
        | 0x5E -> BIT_AR16       (3,HL)
        | 0x5F -> BIT_R8         (3,A)
        | 0x60 -> BIT_R8         (4,B)
        | 0x61 -> BIT_R8         (4,C)
        | 0x62 -> BIT_R8         (4,D)
        | 0x63 -> BIT_R8         (4,E)
        | 0x64 -> BIT_R8         (4,H)
        | 0x65 -> BIT_R8         (4,L)
        | 0x66 -> BIT_AR16       (4,HL)
        | 0x67 -> BIT_R8         (4,A)
        | 0x68 -> BIT_R8         (5,B)
        | 0x69 -> BIT_R8         (5,C)
        | 0x6A -> BIT_R8         (5,D)
        | 0x6B -> BIT_R8         (5,E)
        | 0x6C -> BIT_R8         (5,H)
        | 0x6D -> BIT_R8         (5,L)
        | 0x6E -> BIT_AR16       (5,HL)
        | 0x6F -> BIT_R8         (5,A)
        | 0x70 -> BIT_R8         (6,B)
        | 0x71 -> BIT_R8         (6,C)
        | 0x72 -> BIT_R8         (6,D)
        | 0x73 -> BIT_R8         (6,E)
        | 0x74 -> BIT_R8         (6,H)
        | 0x75 -> BIT_R8         (6,L)
        | 0x76 -> BIT_AR16       (6,HL)
        | 0x77 -> BIT_R8         (6,A)
        | 0x78 -> BIT_R8         (7,B)
        | 0x79 -> BIT_R8         (7,C)
        | 0x7A -> BIT_R8         (7,D)
        | 0x7B -> BIT_R8         (7,E)
        | 0x7C -> BIT_R8         (7,H)
        | 0x7D -> BIT_R8         (7,L)
        | 0x7E -> BIT_AR16       (7,HL)
        | 0x7F -> BIT_R8         (7,A)
        | 0x80 -> RES_R8         (0,B)
        | 0x81 -> RES_R8         (0,C)
        | 0x82 -> RES_R8         (0,D)
        | 0x83 -> RES_R8         (0,E)
        | 0x84 -> RES_R8         (0,H)
        | 0x85 -> RES_R8         (0,L)
        | 0x86 -> RES_AR16       (0,HL)
        | 0x87 -> RES_R8         (0,A)
        | 0x88 -> RES_R8         (1,B)
        | 0x89 -> RES_R8         (1,C)
        | 0x8A -> RES_R8         (1,D)
        | 0x8B -> RES_R8         (1,E)
        | 0x8C -> RES_R8         (1,H)
        | 0x8D -> RES_R8         (1,L)
        | 0x8E -> RES_AR16       (1,HL)
        | 0x8F -> RES_R8         (1,A)
        | 0x90 -> RES_R8         (2,B)
        | 0x91 -> RES_R8         (2,C)
        | 0x92 -> RES_R8         (2,D)
        | 0x93 -> RES_R8         (2,E)
        | 0x94 -> RES_R8         (2,H)
        | 0x95 -> RES_R8         (2,L)
        | 0x96 -> RES_AR16       (2,HL)
        | 0x97 -> RES_R8         (2,A)
        | 0x98 -> RES_R8         (3,B)
        | 0x99 -> RES_R8         (3,C)
        | 0x9A -> RES_R8         (3,D)
        | 0x9B -> RES_R8         (3,E)
        | 0x9C -> RES_R8         (3,H)
        | 0x9D -> RES_R8         (3,L)
        | 0x9E -> RES_AR16       (3,HL)
        | 0x9F -> RES_R8         (3,A)
        | 0xA0 -> RES_R8         (4,B)
        | 0xA1 -> RES_R8         (4,C)
        | 0xA2 -> RES_R8         (4,D)
        | 0xA3 -> RES_R8         (4,E)
        | 0xA4 -> RES_R8         (4,H)
        | 0xA5 -> RES_R8         (4,L)
        | 0xA6 -> RES_AR16       (4,HL)
        | 0xA7 -> RES_R8         (4,A)
        | 0xA8 -> RES_R8         (5,B)
        | 0xA9 -> RES_R8         (5,C)
        | 0xAA -> RES_R8         (5,D)
        | 0xAB -> RES_R8         (5,E)
        | 0xAC -> RES_R8         (5,H)
        | 0xAD -> RES_R8         (5,L)
        | 0xAE -> RES_AR16       (5,HL)
        | 0xAF -> RES_R8         (5,A)
        | 0xB0 -> RES_R8         (6,B)
        | 0xB1 -> RES_R8         (6,C)
        | 0xB2 -> RES_R8         (6,D)
        | 0xB3 -> RES_R8         (6,E)
        | 0xB4 -> RES_R8         (6,H)
        | 0xB5 -> RES_R8         (6,L)
        | 0xB6 -> RES_AR16       (6,HL)
        | 0xB7 -> RES_R8         (6,A)
        | 0xB8 -> RES_R8         (7,B)
        | 0xB9 -> RES_R8         (7,C)
        | 0xBA -> RES_R8         (7,D)
        | 0xBB -> RES_R8         (7,E)
        | 0xBC -> RES_R8         (7,H)
        | 0xBD -> RES_R8         (7,L)
        | 0xBE -> RES_AR16       (7,HL)
        | 0xBF -> RES_R8         (7,A)
        | 0xC0 -> SET_R8         (0,B)
        | 0xC1 -> SET_R8         (0,C)
        | 0xC2 -> SET_R8         (0,D)
        | 0xC3 -> SET_R8         (0,E)
        | 0xC4 -> SET_R8         (0,H)
        | 0xC5 -> SET_R8         (0,L)
        | 0xC6 -> SET_AR16       (0,HL)
        | 0xC7 -> SET_R8         (0,A)
        | 0xC8 -> SET_R8         (1,B)
        | 0xC9 -> SET_R8         (1,C)
        | 0xCA -> SET_R8         (1,D)
        | 0xCB -> SET_R8         (1,E)
        | 0xCC -> SET_R8         (1,H)
        | 0xCD -> SET_R8         (1,L)
        | 0xCE -> SET_AR16       (1,HL)
        | 0xCF -> SET_R8         (1,A)
        | 0xD0 -> SET_R8         (2,B)
        | 0xD1 -> SET_R8         (2,C)
        | 0xD2 -> SET_R8         (2,D)
        | 0xD3 -> SET_R8         (2,E)
        | 0xD4 -> SET_R8         (2,H)
        | 0xD5 -> SET_R8         (2,L)
        | 0xD6 -> SET_AR16       (2,HL)
        | 0xD7 -> SET_R8         (2,A)
        | 0xD8 -> SET_R8         (3,B)
        | 0xD9 -> SET_R8         (3,C)
        | 0xDA -> SET_R8         (3,D)
        | 0xDB -> SET_R8         (3,E)
        | 0xDC -> SET_R8         (3,H)
        | 0xDD -> SET_R8         (3,L)
        | 0xDE -> SET_AR16       (3,HL)
        | 0xDF -> SET_R8         (3,A)
        | 0xE0 -> SET_R8         (4,B)
        | 0xE1 -> SET_R8         (4,C)
        | 0xE2 -> SET_R8         (4,D)
        | 0xE3 -> SET_R8         (4,E)
        | 0xE4 -> SET_R8         (4,H)
        | 0xE5 -> SET_R8         (4,L)
        | 0xE6 -> SET_AR16       (4,HL)
        | 0xE7 -> SET_R8         (4,A)
        | 0xE8 -> SET_R8         (5,B)
        | 0xE9 -> SET_R8         (5,C)
        | 0xEA -> SET_R8         (5,D)
        | 0xEB -> SET_R8         (5,E)
        | 0xEC -> SET_R8         (5,H)
        | 0xED -> SET_R8         (5,L)
        | 0xEE -> SET_AR16       (5,HL)
        | 0xEF -> SET_R8         (5,A)
        | 0xF0 -> SET_R8         (6,B)
        | 0xF1 -> SET_R8         (6,C)
        | 0xF2 -> SET_R8         (6,D)
        | 0xF3 -> SET_R8         (6,E)
        | 0xF4 -> SET_R8         (6,H)
        | 0xF5 -> SET_R8         (6,L)
        | 0xF6 -> SET_AR16       (6,HL)
        | 0xF7 -> SET_R8         (6,A)
        | 0xF8 -> SET_R8         (7,B)
        | 0xF9 -> SET_R8         (7,C)
        | 0xFA -> SET_R8         (7,D)
        | 0xFB -> SET_R8         (7,E)
        | 0xFC -> SET_R8         (7,H)
        | 0xFD -> SET_R8         (7,L)
        | 0xFE -> SET_AR16       (7,HL)
        | 0xFF -> SET_R8         (7,A)
        | _ as extended -> raise (System.Exception(sprintf "decoder for extended opcode <%d %d> not implemented" opcode extended)) 
    | 0xCC -> CALL_F_A16    (Z, int16Operand ())
    | 0xCD -> CALL_A16      (int16Operand ())
    | 0xCE -> ADC_R8_D8     (A, int8Operand ())
    | 0xCF -> RST           (0x0008us)
    | 0xD0 -> RET_NF        (FlagName.C)
    | 0xD1 -> POP_R16       (DE)
    | 0xD2 -> JP_NF_A16     (FlagName.C, int16Operand ())
    | 0xD4 -> CALL_NF_A16   (FlagName.C, int16Operand ())
    | 0xD5 -> PUSH_R16      (DE)
    | 0xD6 -> SUB_R8_D8     (A, int8Operand ())
    | 0xD7 -> RST           (0x0010us)
    | 0xD8 -> RET_F         (FlagName.C)
    | 0xD9 -> RETI
    | 0xDA -> JP_F_A16      (FlagName.C, int16Operand ())
    | 0xDC -> CALL_F_A16    (FlagName.C, int16Operand ())
    | 0xDF -> RST           (0x0018us)
    | 0xE1 -> POP_R16       (HL)
    | 0xDE -> SBC_R8_D8     (A, int8Operand ())
    | 0xE0 -> LDH_A8_R8     (int8Operand (), A)
    | 0xE2 -> LDH_AR8_R8    (C, A)
    | 0xE5 -> PUSH_R16      (HL)
    | 0xE6 -> AND_R8_D8     (A, int8Operand ())
    | 0xE7 -> RST           (0x0020us)
    | 0xE8 -> ADD_R16_D8    (SP, int8Operand () |> int8)
    | 0xE9 -> JP_AR16       (HL)
    | 0xEA -> LD_A16_R8     (uint16 <| int16Operand(), A)
    | 0xEE -> XOR_R8_D8     (A, int8Operand ())
    | 0xEF -> RST           (0x0028us)
    | 0xF0 -> LDH_R8_A8     (A,int8Operand ())
    | 0xF1 -> POP_R16       (AF)
    | 0xF3 -> DI
    | 0xF5 -> PUSH_R16      (AF)
    | 0xF6 -> OR_R8_D8      (A, int8Operand ())
    | 0xF7 -> RST           (0x0030us)
    | 0xF8 -> LDHL_R16_D8   (SP,int8 <| int8Operand ())
    | 0xF9 -> LD_R16_R16    (SP,HL)
    | 0xFA -> LD_R8_A16     (A,uint16 <| int16Operand ())
    | 0xFB -> EI
    | 0xFC -> FGBC_PRINTA_R8(A)
    | 0xFD -> FGBC_PRINT_R8 (A)
    | 0xFE -> CP_R8_D8      (A, int8Operand ())
    | 0xFF -> RST           (0x0038us)
    | _ -> raise (System.Exception(sprintf "decoder for opcode 0x%02X> not implemented" opcode))

let readable instruction = sprintf "%A" instruction

// How many bytes does this instruction use in memory?
let sizeOf instruction =
    match instruction with
    | NOP
    | STOP
    | HALT
    | ADC_R8_AR16 _
    | ADC_R8_R8 _ 
    | ADD_R16_R16 _
    | ADD_R8_AR16 _
    | ADD_R8_R8 _
    | AND_R8_R8 _
    | AND_R8_AR16 _
    | CCF
    | CPL 
    | CP_R8_AR16 _
    | CP_R8_R8 _
    | DAA_R8 _
    | DEC_AR16 _
    | DEC_R16 _
    | DEC_R8 _
    | DI
    | EI
    | FGBC_PRINTA_R8 _
    | FGBC_PRINT_R8 _
    | INC_AR16 _
    | INC_R16 _
    | INC_R8 _
    | JP_AR16 _
    | LDD_AR16_R8 _
    | LDD_R8_AR16 _
    | LDH_AR8_R8 _
    | LDI_AR16_R8 _
    | LDI_R8_AR16 _
    | LD_AR16_R8 _
    | LD_R16_R16 _
    | LD_R8_R8 _
    | LD_R8_AR16 _
    | OR_R8_AR16 _
    | OR_R8_R8 _
    | POP_R16 _
    | PUSH_R16 _
    | RLCA _
    | SBC_R8_AR16 _
    | SBC_R8_R8 _
    | SCF 
    | SUB_R8_AR16 _
    | SUB_R8_R8 _
    | XOR_R8_AR16 _
    | XOR_R8_R8 _
    | RET
    | RETI
    | RET_F _
    | RET_NF _
    | RST _
        -> 1
    | BIT_AR16 _
    | BIT_R8 _
    | LD_R8_D8 _ 
    | ADC_R8_D8 _
    | ADD_R8_D8 _
    | ADD_R16_D8 _
    | AND_R8_D8 _
    | CP_R8_D8 _
    | JR_A8 _
    | JR_F_A8 _
    | JR_NF_A8 _
    | LDHL_R16_D8 _
    | LDH_A8_R8 _
    | LDH_R8_A8 _
    | LD_AR16_D8 _
    | OR_R8_D8 _
    | RES_AR16 _
    | RES_R8 _
    | ADC_R8_D8 _
    | SBC_R8_D8 _
    | SET_AR16 _
    | SET_R8 _
    | SUB_R8_D8 _
    | SWAP_AR16 _
    | SWAP_R8 _
    | XOR_R8_D8 _
         -> 2
    | LD_R8_A16 _
    | JP_A16 _
    | JP_F_A16 _
    | JP_NF_A16 _
    | LD_A16_R16 _
    | LD_A16_R8 _
    | LD_R16_D16 _
    | CALL_A16 _
    | CALL_F_A16 _
    | CALL_NF_A16 _
        -> 3
    
// How many cycles does it take to execute this instruction?
let cycleCount instruction long =
    match instruction with
    | ADC_R8_R8 _
    | ADD_R8_R8 _
    | AND_R8_R8 _
    | CCF
    | CPL
    | CP_R8_R8 _
    | DAA_R8 _
    | DEC_R8 _
    | DI
    | EI
    | FGBC_PRINT_R8 _ // Since unused opcodes usually work as NOP use 4 cycles for custom instructions.
    | FGBC_PRINTA_R8 _ // ↑
    | HALT
    | INC_R8 _
    | JP_AR16 _
    | LD_R8_R8 _
    | NOP
    | OR_R8_R8 _
    | RLCA
    | SBC_R8_R8 _
    | SCF
    | STOP
    | SUB_R8_R8 _
    | XOR_R8_R8 _
        -> 4
    | ADC_R8_AR16 _
    | ADC_R8_D8 _
    | ADD_R16_R16 _
    | ADD_R8_AR16 _
    | ADD_R8_D8 _
    | AND_R8_D8 _
    | AND_R8_AR16 _
    | BIT_R8 _
    | CP_R8_AR16 _
    | CP_R8_D8 _
    | DEC_R16 _
    | INC_R16 _
    | LDD_AR16_R8 _
    | LDD_R8_AR16 _
    | LDI_AR16_R8 _
    | LDI_R8_AR16 _
    | LD_R8_D8 _
    | LD_AR16_R8 _
    | LD_R8_AR16 _
    | OR_R8_AR16 _
    | OR_R8_D8 _
    | RES_R8 _
    | SBC_R8_AR16 _
    | SBC_R8_D8 _
    | SET_R8 _
    | SUB_R8_AR16 _
    | SUB_R8_D8 _
    | SWAP_R8 _
    | XOR_R8_AR16 _
    | XOR_R8_D8 _
        -> 8
    | BIT_AR16 _
    | DEC_AR16 _
    | INC_AR16 _
    | JR_A8 _
    | LDHL_R16_D8 _
    | LDH_A8_R8 _
    | LD_R16_D16 _
    | LD_R16_R16 _
    | POP_R16 _
        -> 12
    | ADD_R16_D8 _
    | JP_A16 _
    | LD_A16_R8 _
    | LD_R8_A16 _
    | PUSH_R16 _
    | RES_AR16 _
    | RET
    | RETI
    | SET_AR16 _
    | SWAP_AR16 _
    | RST _
        -> 16
    | LD_A16_R16 _
        -> 20
    | CALL_A16 _
        -> 24
    | CALL_F_A16 _
    | CALL_NF_A16 _
        -> if not long then 12 else 24
    | JP_F_A16 _
    | JP_NF_A16 _
        -> if not long then 12 else 16
    | JR_F_A8 _
    | JR_NF_A8 _
        -> if not long then 8 else 12
    | RET_F _
    | RET_NF _
        -> if not long then 8 else 20
    | LDH_A8_R8 _ // Unsure
    | LDH_AR8_R8 _ // Unsure
    | LDH_R8_A8 _ // Unsure
    | LD_AR16_D8 _ // Unsure
        -> 0
    




