module Instruction

open Register
open Mmu
open Misc
open Units

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
    | RLA                                                       // Rotate A left    
    | RRCA                                                      // Rotate A right with carry
    | RRA                                                       // Rotate A right  
    | RLC_R8         of Register8Name                           // Rotate 8 bit register left with carry
    | RLC_AR16       of Register16Name                          // Rotate value pointed by 16 bit register left with carry
    | RL_R8          of Register8Name                           // Rotate 8 bit register left
    | RL_AR16        of Register16Name                          // Rotate value pointed by 16 bit register left
    | RRC_R8         of Register8Name                           // Rotate 8 bit register right with carry
    | RRC_AR16       of Register16Name                          // Rotate value pointed by 16 bit register right with carry
    | RR_R8          of Register8Name                           // Rotate 8 bit register right
    | RR_AR16        of Register16Name                          // Rotate value pointed by 16 bit register right      
    | SLA_R8         of Register8Name                           // Shift 8 bit register left, preserving sign
    | SLA_AR16       of Register16Name                          // Shift value pointed by 16 bit register left, preserving sign
    | SRA_R8         of Register8Name                           // Shift 8 bit register right, preserving sign
    | SRA_AR16       of Register16Name                          // Shift value pointed by 16 bit register right, preserving sign
    | SRL_R8         of Register8Name                           // Shift 8 bit register right
    | SRL_AR16       of Register16Name                          // Shift value pointed by 16 bit register right
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

    let int8Operand () = mmu.Read8 (address + 1us)
    let int16Operand () = mmu.Read16 (address + 1us)

    let opcode = mmu.Read8 address

    match opcode with
    | 0x00uy -> NOP
    | 0x01uy -> LD_R16_D16    (BC,int16Operand ())
    | 0x02uy -> LD_AR16_R8    (BC,A)
    | 0x03uy -> INC_R16       (BC)
    | 0x04uy -> INC_R8        (B)
    | 0x05uy -> DEC_R8        (B)
    | 0x06uy -> LD_R8_D8      (B,int8Operand ())
    | 0x07uy -> RLCA
    | 0x08uy -> LD_A16_R16    (int16Operand (), SP)
    | 0x09uy -> ADD_R16_R16   (HL,BC)
    | 0x0Auy -> LD_R8_AR16    (A,BC)
    | 0x0Buy -> DEC_R16       (BC)
    | 0x0Cuy -> INC_R8        (C)
    | 0x0Duy -> DEC_R8        (C)
    | 0x0Euy -> LD_R8_D8      (C,int8Operand ())
    | 0x0Fuy -> RRCA
    | 0x10uy -> STOP
    | 0x11uy -> LD_R16_D16    (DE,int16Operand ())
    | 0x12uy -> LD_AR16_R8    (DE,A)
    | 0x13uy -> INC_R16       (DE)
    | 0x14uy -> INC_R8        (D)
    | 0x15uy -> DEC_R8        (D)
    | 0x16uy -> LD_R8_D8      (D,int8Operand ())
    | 0x17uy -> RLA
    | 0x18uy -> JR_A8         (int8Operand () |> int8)
    | 0x19uy -> ADD_R16_R16   (HL,DE)
    | 0x1Auy -> LD_R8_AR16    (A,DE)
    | 0x1Buy -> DEC_R16       (DE)
    | 0x1Cuy -> INC_R8        (E)
    | 0x1Duy -> DEC_R8        (E)
    | 0x1Euy -> LD_R8_D8      (E,int8Operand ())
    | 0x1Fuy -> RRA
    | 0x20uy -> JR_NF_A8      (Z, int8Operand () |> int8)
    | 0x21uy -> LD_R16_D16    (HL,int16Operand ())
    | 0x22uy -> LDI_AR16_R8   (HL,A)
    | 0x23uy -> INC_R16       (HL)
    | 0x24uy -> INC_R8        (H)
    | 0x25uy -> DEC_R8        (H)
    | 0x26uy -> LD_R8_D8      (H,int8Operand ())
    | 0x27uy -> DAA_R8        (A)       
    | 0x28uy -> JR_F_A8       (Z, int8Operand () |> int8)
    | 0x29uy -> ADD_R16_R16   (HL,HL)
    | 0x2Auy -> LDI_R8_AR16   (A,HL)
    | 0x2Buy -> DEC_R16       (HL)
    | 0x2Cuy -> INC_R8        (L)
    | 0x2Duy -> DEC_R8        (L)
    | 0x2Euy -> LD_R8_D8      (L,int8Operand ())
    | 0x2Fuy -> CPL
    | 0x30uy -> JR_NF_A8      (FlagName.C, int8Operand () |> int8)
    | 0x31uy -> LD_R16_D16    (SP,int16Operand ())
    | 0x32uy -> LDD_AR16_R8   (HL,A)
    | 0x33uy -> INC_R16       (SP)
    | 0x34uy -> INC_AR16      (HL)
    | 0x35uy -> DEC_AR16      (HL)
    | 0x36uy -> LD_AR16_D8    (HL,int8Operand ())
    | 0x37uy -> SCF
    | 0x38uy -> JR_F_A8       (FlagName.C, int8Operand () |> int8)
    | 0x39uy -> ADD_R16_R16   (HL,SP)
    | 0x3Auy -> LDD_R8_AR16   (A,HL)
    | 0x3Buy -> DEC_R16       (SP)
    | 0x3Cuy -> INC_R8        (A)
    | 0x3Duy -> DEC_R8        (A)
    | 0x3Euy -> LD_R8_D8      (A,int8Operand ())
    | 0x3Fuy -> CCF
    | 0x40uy -> LD_R8_R8      (B,B)
    | 0x41uy -> LD_R8_R8      (B,C)
    | 0x42uy -> LD_R8_R8      (B,D)
    | 0x43uy -> LD_R8_R8      (B,E)
    | 0x44uy -> LD_R8_R8      (B,H)
    | 0x45uy -> LD_R8_R8      (B,L)
    | 0x46uy -> LD_R8_AR16    (B,HL)
    | 0x47uy -> LD_R8_R8      (B,A)
    | 0x48uy -> LD_R8_R8      (C,B)
    | 0x49uy -> LD_R8_R8      (C,C)
    | 0x4Auy -> LD_R8_R8      (C,D)
    | 0x4Buy -> LD_R8_R8      (C,E)
    | 0x4Cuy -> LD_R8_R8      (C,H)
    | 0x4Duy -> LD_R8_R8      (C,L)
    | 0x4Euy -> LD_R8_AR16    (C,HL)
    | 0x4Fuy -> LD_R8_R8      (C,A)
    | 0x50uy -> LD_R8_R8      (D,B)
    | 0x51uy -> LD_R8_R8      (D,C)
    | 0x52uy -> LD_R8_R8      (D,D)
    | 0x53uy -> LD_R8_R8      (D,E)
    | 0x54uy -> LD_R8_R8      (D,H)
    | 0x55uy -> LD_R8_R8      (D,L)
    | 0x56uy -> LD_R8_AR16    (D,HL)
    | 0x57uy -> LD_R8_R8      (D,A)
    | 0x58uy -> LD_R8_R8      (E,B)
    | 0x59uy -> LD_R8_R8      (E,C)
    | 0x5Auy -> LD_R8_R8      (E,D)
    | 0x5Buy -> LD_R8_R8      (E,E)
    | 0x5Cuy -> LD_R8_R8      (E,H)
    | 0x5Duy -> LD_R8_R8      (E,L)
    | 0x5Euy -> LD_R8_AR16    (E,HL)
    | 0x5Fuy -> LD_R8_R8      (E,A)
    | 0x60uy -> LD_R8_R8      (H,B)
    | 0x61uy -> LD_R8_R8      (H,C)
    | 0x62uy -> LD_R8_R8      (H,D)
    | 0x63uy -> LD_R8_R8      (H,E)
    | 0x64uy -> LD_R8_R8      (H,H)
    | 0x65uy -> LD_R8_R8      (H,L)
    | 0x66uy -> LD_R8_AR16    (H,HL)
    | 0x67uy -> LD_R8_R8      (H,A)
    | 0x68uy -> LD_R8_R8      (L,B)
    | 0x69uy -> LD_R8_R8      (L,C)
    | 0x6Auy -> LD_R8_R8      (L,D)
    | 0x6Buy -> LD_R8_R8      (L,E)
    | 0x6Cuy -> LD_R8_R8      (L,H)
    | 0x6Duy -> LD_R8_R8      (L,L)
    | 0x6Euy -> LD_R8_AR16    (L,HL)
    | 0x6Fuy -> LD_R8_R8      (L,A)
    | 0x70uy -> LD_AR16_R8    (HL,B)
    | 0x71uy -> LD_AR16_R8    (HL,C)
    | 0x72uy -> LD_AR16_R8    (HL,D)
    | 0x73uy -> LD_AR16_R8    (HL,E)
    | 0x74uy -> LD_AR16_R8    (HL,H)
    | 0x75uy -> LD_AR16_R8    (HL,L)
    | 0x76uy -> HALT
    | 0x77uy -> LD_AR16_R8    (HL,A)
    | 0x78uy -> LD_R8_R8      (A,B)
    | 0x79uy -> LD_R8_R8      (A,C)
    | 0x7Auy -> LD_R8_R8      (A,D)
    | 0x7Buy -> LD_R8_R8      (A,E)
    | 0x7Cuy -> LD_R8_R8      (A,H)
    | 0x7Duy -> LD_R8_R8      (A,L)
    | 0x7Euy -> LD_R8_AR16    (A,HL)
    | 0x7Fuy -> LD_R8_R8      (A,A)
    | 0x80uy -> ADD_R8_R8     (A,B)
    | 0x81uy -> ADD_R8_R8     (A,C)
    | 0x82uy -> ADD_R8_R8     (A,D)
    | 0x83uy -> ADD_R8_R8     (A,E)
    | 0x84uy -> ADD_R8_R8     (A,H)
    | 0x85uy -> ADD_R8_R8     (A,L)
    | 0x86uy -> ADD_R8_AR16   (A,HL)
    | 0x87uy -> ADD_R8_R8     (A,A)
    | 0x88uy -> ADC_R8_R8     (A,B)
    | 0x89uy -> ADC_R8_R8     (A,C)
    | 0x8Auy -> ADC_R8_R8     (A,D)
    | 0x8Buy -> ADC_R8_R8     (A,E)
    | 0x8Cuy -> ADC_R8_R8     (A,H)
    | 0x8Duy -> ADC_R8_R8     (A,L)
    | 0x8Euy -> ADC_R8_AR16   (A,HL)
    | 0x8Fuy -> ADC_R8_R8     (A,A)
    | 0x90uy -> SUB_R8_R8     (A,B)
    | 0x91uy -> SUB_R8_R8     (A,C)
    | 0x92uy -> SUB_R8_R8     (A,D)
    | 0x93uy -> SUB_R8_R8     (A,E)
    | 0x94uy -> SUB_R8_R8     (A,H)
    | 0x95uy -> SUB_R8_R8     (A,L)
    | 0x96uy -> SUB_R8_AR16   (A,HL)
    | 0x97uy -> SUB_R8_R8     (A,A) 
    | 0x98uy -> SBC_R8_R8     (A,B)
    | 0x99uy -> SBC_R8_R8     (A,C)
    | 0x9Auy -> SBC_R8_R8     (A,D)
    | 0x9Buy -> SBC_R8_R8     (A,E)
    | 0x9Cuy -> SBC_R8_R8     (A,H)
    | 0x9Duy -> SBC_R8_R8     (A,L)
    | 0x9Euy -> SBC_R8_AR16   (A,HL)
    | 0x9Fuy -> SBC_R8_R8     (A,A)
    | 0xA0uy -> AND_R8_R8     (A,B)
    | 0xA1uy -> AND_R8_R8     (A,C)
    | 0xA2uy -> AND_R8_R8     (A,D)
    | 0xA3uy -> AND_R8_R8     (A,E)
    | 0xA4uy -> AND_R8_R8     (A,H)
    | 0xA5uy -> AND_R8_R8     (A,L)
    | 0xA6uy -> AND_R8_AR16   (A,HL)
    | 0xA7uy -> AND_R8_R8     (A,A)    
    | 0xA8uy -> XOR_R8_R8     (A,B)
    | 0xA9uy -> XOR_R8_R8     (A,C)
    | 0xAAuy -> XOR_R8_R8     (A,D)
    | 0xABuy -> XOR_R8_R8     (A,E)
    | 0xACuy -> XOR_R8_R8     (A,H)
    | 0xADuy -> XOR_R8_R8     (A,L)
    | 0xAEuy -> XOR_R8_AR16   (A,HL)
    | 0xAFuy -> XOR_R8_R8     (A,A)
    | 0xB0uy -> OR_R8_R8      (A,B)
    | 0xB1uy -> OR_R8_R8      (A,C)
    | 0xB2uy -> OR_R8_R8      (A,D)
    | 0xB3uy -> OR_R8_R8      (A,E)
    | 0xB4uy -> OR_R8_R8      (A,H)
    | 0xB5uy -> OR_R8_R8      (A,L)
    | 0xB6uy -> OR_R8_AR16    (A,HL)
    | 0xB7uy -> OR_R8_R8      (A,A)  
    | 0xB8uy -> CP_R8_R8      (A,B)
    | 0xB9uy -> CP_R8_R8      (A,C)
    | 0xBAuy -> CP_R8_R8      (A,D)
    | 0xBBuy -> CP_R8_R8      (A,E)
    | 0xBCuy -> CP_R8_R8      (A,H)
    | 0xBDuy -> CP_R8_R8      (A,L)
    | 0xBEuy -> CP_R8_AR16    (A,HL)
    | 0xBFuy -> CP_R8_R8      (A,A)
    | 0xC0uy -> RET_NF        (Z)
    | 0xC1uy -> POP_R16       (BC)
    | 0xC2uy -> JP_NF_A16     (Z,int16Operand ())
    | 0xC3uy -> JP_A16        (int16Operand())
    | 0xC4uy -> CALL_NF_A16   (Z, int16Operand ())
    | 0xC5uy -> PUSH_R16      (BC)
    | 0xC6uy -> ADD_R8_D8     (A, int8Operand ())
    | 0xC7uy -> RST           (0x0000us)
    | 0xC8uy -> RET_F         (Z)
    | 0xC9uy -> RET
    | 0xCAuy -> JP_F_A16      (Z, int16Operand ())
    | 0xCBuy ->
        match int8Operand () with
        | 0x00uy -> RLC_R8         (B)
        | 0x01uy -> RLC_R8         (C)
        | 0x02uy -> RLC_R8         (D)
        | 0x03uy -> RLC_R8         (E)
        | 0x04uy -> RLC_R8         (H)
        | 0x05uy -> RLC_R8         (L)
        | 0x06uy -> RLC_AR16       (HL)
        | 0x07uy -> RLC_R8         (A)
        | 0x08uy -> RRC_R8         (B)
        | 0x09uy -> RRC_R8         (C)
        | 0x0Auy -> RRC_R8         (D)
        | 0x0Buy -> RRC_R8         (E)
        | 0x0Cuy -> RRC_R8         (H)
        | 0x0Duy -> RRC_R8         (L)
        | 0x0Euy -> RRC_AR16       (HL)
        | 0x0Fuy -> RRC_R8         (A)
        | 0x10uy -> RL_R8          (B)
        | 0x11uy -> RL_R8          (C)
        | 0x12uy -> RL_R8          (D)
        | 0x13uy -> RL_R8          (E)
        | 0x14uy -> RL_R8          (H)
        | 0x15uy -> RL_R8          (L)
        | 0x16uy -> RL_AR16        (HL)
        | 0x17uy -> RL_R8          (A)
        | 0x18uy -> RR_R8          (B)
        | 0x19uy -> RR_R8          (C)
        | 0x1Auy -> RR_R8          (D)
        | 0x1Buy -> RR_R8          (E)
        | 0x1Cuy -> RR_R8          (H)
        | 0x1Duy -> RR_R8          (L)
        | 0x1Euy -> RR_AR16        (HL)
        | 0x1Fuy -> RR_R8          (A)
        | 0x20uy -> SLA_R8         (B)
        | 0x21uy -> SLA_R8         (C)
        | 0x22uy -> SLA_R8         (D)
        | 0x23uy -> SLA_R8         (E)
        | 0x24uy -> SLA_R8         (H)
        | 0x25uy -> SLA_R8         (L)
        | 0x26uy -> SLA_AR16       (HL)
        | 0x27uy -> SLA_R8         (A)
        | 0x28uy -> SRA_R8         (B)
        | 0x29uy -> SRA_R8         (C)
        | 0x2Auy -> SRA_R8         (D)
        | 0x2Buy -> SRA_R8         (E)
        | 0x2Cuy -> SRA_R8         (H)
        | 0x2Duy -> SRA_R8         (L)
        | 0x2Euy -> SRA_AR16       (HL)
        | 0x2Fuy -> SRA_R8         (A)
        | 0x30uy -> SWAP_R8        (B)
        | 0x31uy -> SWAP_R8        (C)
        | 0x32uy -> SWAP_R8        (D)
        | 0x33uy -> SWAP_R8        (E)
        | 0x34uy -> SWAP_R8        (H)
        | 0x35uy -> SWAP_R8        (L)
        | 0x36uy -> SWAP_AR16      (HL)
        | 0x37uy -> SWAP_R8        (A)
        | 0x38uy -> SRL_R8         (B)
        | 0x39uy -> SRL_R8         (C)
        | 0x3Auy -> SRL_R8         (D)
        | 0x3Buy -> SRL_R8         (E)
        | 0x3Cuy -> SRL_R8         (H)
        | 0x3Duy -> SRL_R8         (L)
        | 0x3Euy -> SRL_AR16       (HL)
        | 0x3Fuy -> SRL_R8         (A)
        | 0x40uy -> BIT_R8         (0,B)
        | 0x41uy -> BIT_R8         (0,C)
        | 0x42uy -> BIT_R8         (0,D)
        | 0x43uy -> BIT_R8         (0,E)
        | 0x44uy -> BIT_R8         (0,H)
        | 0x45uy -> BIT_R8         (0,L)
        | 0x46uy -> BIT_AR16       (0,HL)
        | 0x47uy -> BIT_R8         (0,A)
        | 0x48uy -> BIT_R8         (1,B)
        | 0x49uy -> BIT_R8         (1,C)
        | 0x4Auy -> BIT_R8         (1,D)
        | 0x4Buy -> BIT_R8         (1,E)
        | 0x4Cuy -> BIT_R8         (1,H)
        | 0x4Duy -> BIT_R8         (1,L)
        | 0x4Euy -> BIT_AR16       (1,HL)
        | 0x4Fuy -> BIT_R8         (1,A)
        | 0x50uy -> BIT_R8         (2,B)
        | 0x51uy -> BIT_R8         (2,C)
        | 0x52uy -> BIT_R8         (2,D)
        | 0x53uy -> BIT_R8         (2,E)
        | 0x54uy -> BIT_R8         (2,H)
        | 0x55uy -> BIT_R8         (2,L)
        | 0x56uy -> BIT_AR16       (2,HL)
        | 0x57uy -> BIT_R8         (2,A)
        | 0x58uy -> BIT_R8         (3,B)
        | 0x59uy -> BIT_R8         (3,C)
        | 0x5Auy -> BIT_R8         (3,D)
        | 0x5Buy -> BIT_R8         (3,E)
        | 0x5Cuy -> BIT_R8         (3,H)
        | 0x5Duy -> BIT_R8         (3,L)
        | 0x5Euy -> BIT_AR16       (3,HL)
        | 0x5Fuy -> BIT_R8         (3,A)
        | 0x60uy -> BIT_R8         (4,B)
        | 0x61uy -> BIT_R8         (4,C)
        | 0x62uy -> BIT_R8         (4,D)
        | 0x63uy -> BIT_R8         (4,E)
        | 0x64uy -> BIT_R8         (4,H)
        | 0x65uy -> BIT_R8         (4,L)
        | 0x66uy -> BIT_AR16       (4,HL)
        | 0x67uy -> BIT_R8         (4,A)
        | 0x68uy -> BIT_R8         (5,B)
        | 0x69uy -> BIT_R8         (5,C)
        | 0x6Auy -> BIT_R8         (5,D)
        | 0x6Buy -> BIT_R8         (5,E)
        | 0x6Cuy -> BIT_R8         (5,H)
        | 0x6Duy -> BIT_R8         (5,L)
        | 0x6Euy -> BIT_AR16       (5,HL)
        | 0x6Fuy -> BIT_R8         (5,A)
        | 0x70uy -> BIT_R8         (6,B)
        | 0x71uy -> BIT_R8         (6,C)
        | 0x72uy -> BIT_R8         (6,D)
        | 0x73uy -> BIT_R8         (6,E)
        | 0x74uy -> BIT_R8         (6,H)
        | 0x75uy -> BIT_R8         (6,L)
        | 0x76uy -> BIT_AR16       (6,HL)
        | 0x77uy -> BIT_R8         (6,A)
        | 0x78uy -> BIT_R8         (7,B)
        | 0x79uy -> BIT_R8         (7,C)
        | 0x7Auy -> BIT_R8         (7,D)
        | 0x7Buy -> BIT_R8         (7,E)
        | 0x7Cuy -> BIT_R8         (7,H)
        | 0x7Duy -> BIT_R8         (7,L)
        | 0x7Euy -> BIT_AR16       (7,HL)
        | 0x7Fuy -> BIT_R8         (7,A)
        | 0x80uy -> RES_R8         (0,B)
        | 0x81uy -> RES_R8         (0,C)
        | 0x82uy -> RES_R8         (0,D)
        | 0x83uy -> RES_R8         (0,E)
        | 0x84uy -> RES_R8         (0,H)
        | 0x85uy -> RES_R8         (0,L)
        | 0x86uy -> RES_AR16       (0,HL)
        | 0x87uy -> RES_R8         (0,A)
        | 0x88uy -> RES_R8         (1,B)
        | 0x89uy -> RES_R8         (1,C)
        | 0x8Auy -> RES_R8         (1,D)
        | 0x8Buy -> RES_R8         (1,E)
        | 0x8Cuy -> RES_R8         (1,H)
        | 0x8Duy -> RES_R8         (1,L)
        | 0x8Euy -> RES_AR16       (1,HL)
        | 0x8Fuy -> RES_R8         (1,A)
        | 0x90uy -> RES_R8         (2,B)
        | 0x91uy -> RES_R8         (2,C)
        | 0x92uy -> RES_R8         (2,D)
        | 0x93uy -> RES_R8         (2,E)
        | 0x94uy -> RES_R8         (2,H)
        | 0x95uy -> RES_R8         (2,L)
        | 0x96uy -> RES_AR16       (2,HL)
        | 0x97uy -> RES_R8         (2,A)
        | 0x98uy -> RES_R8         (3,B)
        | 0x99uy -> RES_R8         (3,C)
        | 0x9Auy -> RES_R8         (3,D)
        | 0x9Buy -> RES_R8         (3,E)
        | 0x9Cuy -> RES_R8         (3,H)
        | 0x9Duy -> RES_R8         (3,L)
        | 0x9Euy -> RES_AR16       (3,HL)
        | 0x9Fuy -> RES_R8         (3,A)
        | 0xA0uy -> RES_R8         (4,B)
        | 0xA1uy -> RES_R8         (4,C)
        | 0xA2uy -> RES_R8         (4,D)
        | 0xA3uy -> RES_R8         (4,E)
        | 0xA4uy -> RES_R8         (4,H)
        | 0xA5uy -> RES_R8         (4,L)
        | 0xA6uy -> RES_AR16       (4,HL)
        | 0xA7uy -> RES_R8         (4,A)
        | 0xA8uy -> RES_R8         (5,B)
        | 0xA9uy -> RES_R8         (5,C)
        | 0xAAuy -> RES_R8         (5,D)
        | 0xABuy -> RES_R8         (5,E)
        | 0xACuy -> RES_R8         (5,H)
        | 0xADuy -> RES_R8         (5,L)
        | 0xAEuy -> RES_AR16       (5,HL)
        | 0xAFuy -> RES_R8         (5,A)
        | 0xB0uy -> RES_R8         (6,B)
        | 0xB1uy -> RES_R8         (6,C)
        | 0xB2uy -> RES_R8         (6,D)
        | 0xB3uy -> RES_R8         (6,E)
        | 0xB4uy -> RES_R8         (6,H)
        | 0xB5uy -> RES_R8         (6,L)
        | 0xB6uy -> RES_AR16       (6,HL)
        | 0xB7uy -> RES_R8         (6,A)
        | 0xB8uy -> RES_R8         (7,B)
        | 0xB9uy -> RES_R8         (7,C)
        | 0xBAuy -> RES_R8         (7,D)
        | 0xBBuy -> RES_R8         (7,E)
        | 0xBCuy -> RES_R8         (7,H)
        | 0xBDuy -> RES_R8         (7,L)
        | 0xBEuy -> RES_AR16       (7,HL)
        | 0xBFuy -> RES_R8         (7,A)
        | 0xC0uy -> SET_R8         (0,B)
        | 0xC1uy -> SET_R8         (0,C)
        | 0xC2uy -> SET_R8         (0,D)
        | 0xC3uy -> SET_R8         (0,E)
        | 0xC4uy -> SET_R8         (0,H)
        | 0xC5uy -> SET_R8         (0,L)
        | 0xC6uy -> SET_AR16       (0,HL)
        | 0xC7uy -> SET_R8         (0,A)
        | 0xC8uy -> SET_R8         (1,B)
        | 0xC9uy -> SET_R8         (1,C)
        | 0xCAuy -> SET_R8         (1,D)
        | 0xCBuy -> SET_R8         (1,E)
        | 0xCCuy -> SET_R8         (1,H)
        | 0xCDuy -> SET_R8         (1,L)
        | 0xCEuy -> SET_AR16       (1,HL)
        | 0xCFuy -> SET_R8         (1,A)
        | 0xD0uy -> SET_R8         (2,B)
        | 0xD1uy -> SET_R8         (2,C)
        | 0xD2uy -> SET_R8         (2,D)
        | 0xD3uy -> SET_R8         (2,E)
        | 0xD4uy -> SET_R8         (2,H)
        | 0xD5uy -> SET_R8         (2,L)
        | 0xD6uy -> SET_AR16       (2,HL)
        | 0xD7uy -> SET_R8         (2,A)
        | 0xD8uy -> SET_R8         (3,B)
        | 0xD9uy -> SET_R8         (3,C)
        | 0xDAuy -> SET_R8         (3,D)
        | 0xDBuy -> SET_R8         (3,E)
        | 0xDCuy -> SET_R8         (3,H)
        | 0xDDuy -> SET_R8         (3,L)
        | 0xDEuy -> SET_AR16       (3,HL)
        | 0xDFuy -> SET_R8         (3,A)
        | 0xE0uy -> SET_R8         (4,B)
        | 0xE1uy -> SET_R8         (4,C)
        | 0xE2uy -> SET_R8         (4,D)
        | 0xE3uy -> SET_R8         (4,E)
        | 0xE4uy -> SET_R8         (4,H)
        | 0xE5uy -> SET_R8         (4,L)
        | 0xE6uy -> SET_AR16       (4,HL)
        | 0xE7uy -> SET_R8         (4,A)
        | 0xE8uy -> SET_R8         (5,B)
        | 0xE9uy -> SET_R8         (5,C)
        | 0xEAuy -> SET_R8         (5,D)
        | 0xEBuy -> SET_R8         (5,E)
        | 0xECuy -> SET_R8         (5,H)
        | 0xEDuy -> SET_R8         (5,L)
        | 0xEEuy -> SET_AR16       (5,HL)
        | 0xEFuy -> SET_R8         (5,A)
        | 0xF0uy -> SET_R8         (6,B)
        | 0xF1uy -> SET_R8         (6,C)
        | 0xF2uy -> SET_R8         (6,D)
        | 0xF3uy -> SET_R8         (6,E)
        | 0xF4uy -> SET_R8         (6,H)
        | 0xF5uy -> SET_R8         (6,L)
        | 0xF6uy -> SET_AR16       (6,HL)
        | 0xF7uy -> SET_R8         (6,A)
        | 0xF8uy -> SET_R8         (7,B)
        | 0xF9uy -> SET_R8         (7,C)
        | 0xFAuy -> SET_R8         (7,D)
        | 0xFBuy -> SET_R8         (7,E)
        | 0xFCuy -> SET_R8         (7,H)
        | 0xFDuy -> SET_R8         (7,L)
        | 0xFEuy -> SET_AR16       (7,HL)
        | 0xFFuy -> SET_R8         (7,A)
        | _ -> raise <| System.Exception("Invalid opcode.")
    | 0xCCuy -> CALL_F_A16    (Z, int16Operand ())
    | 0xCDuy -> CALL_A16      (int16Operand ())
    | 0xCEuy -> ADC_R8_D8     (A, int8Operand ())
    | 0xCFuy -> RST           (0x0008us)
    | 0xD0uy -> RET_NF        (FlagName.C)
    | 0xD1uy -> POP_R16       (DE)
    | 0xD2uy -> JP_NF_A16     (FlagName.C, int16Operand ())
    | 0xD3uy -> NOP
    | 0xD4uy -> CALL_NF_A16   (FlagName.C, int16Operand ())
    | 0xD5uy -> PUSH_R16      (DE)
    | 0xD6uy -> SUB_R8_D8     (A, int8Operand ())
    | 0xD7uy -> RST           (0x0010us)
    | 0xD8uy -> RET_F         (FlagName.C)
    | 0xD9uy -> RETI
    | 0xDAuy -> JP_F_A16      (FlagName.C, int16Operand ())
    | 0xDBuy -> NOP
    | 0xDCuy -> CALL_F_A16    (FlagName.C, int16Operand ())
    | 0xDDuy -> NOP
    | 0xDEuy -> SBC_R8_D8     (A, int8Operand ())
    | 0xDFuy -> RST           (0x0018us)
    | 0xE0uy -> LDH_A8_R8     (int8Operand (), A)
    | 0xE1uy -> POP_R16       (HL)
    | 0xE2uy -> LDH_AR8_R8    (C, A)
    | 0xE3uy -> NOP
    | 0xE4uy -> NOP
    | 0xE5uy -> PUSH_R16      (HL)
    | 0xE6uy -> AND_R8_D8     (A, int8Operand ())
    | 0xE7uy -> RST           (0x0020us)
    | 0xE8uy -> ADD_R16_D8    (SP, int8Operand () |> int8)
    | 0xE9uy -> JP_AR16       (HL)
    | 0xEAuy -> LD_A16_R8     (uint16 <| int16Operand(), A)
    | 0xEBuy -> NOP
    | 0xECuy -> NOP
    | 0xEDuy -> NOP
    | 0xEEuy -> XOR_R8_D8     (A, int8Operand ())
    | 0xEFuy -> RST           (0x0028us)
    | 0xF0uy -> LDH_R8_A8     (A,int8Operand ())
    | 0xF1uy -> POP_R16       (AF)
    | 0xF2uy -> NOP
    | 0xF3uy -> DI
    | 0xF4uy -> NOP
    | 0xF5uy -> PUSH_R16      (AF)
    | 0xF6uy -> OR_R8_D8      (A, int8Operand ())
    | 0xF7uy -> RST           (0x0030us)
    | 0xF8uy -> LDHL_R16_D8   (SP,int8 <| int8Operand ())
    | 0xF9uy -> LD_R16_R16    (SP,HL)
    | 0xFAuy -> LD_R8_A16     (A,uint16 <| int16Operand ())
    | 0xFBuy -> EI
    | 0xFCuy -> FGBC_PRINTA_R8(A)
    | 0xFDuy -> FGBC_PRINT_R8 (A)
    | 0xFEuy -> CP_R8_D8      (A, int8Operand ())
    | 0xFFuy -> RST           (0x0038us)
    | _ -> raise <| System.Exception("Invalid opcode.")

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
    | RLCA 
    | RLA
    | RRCA
    | RRA
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
        -> 1<b>
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
    | RLC_R8 _
    | RLC_AR16 _
    | RL_R8 _
    | RL_AR16 _
    | RRC_R8 _
    | RRC_AR16 _
    | RR_R8 _
    | RR_AR16 _
    | SLA_R8 _
    | SLA_AR16 _
    | SRA_R8 _
    | SRA_AR16 _
    | SRL_R8 _
    | SRL_AR16 _
         -> 2<b>
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
        -> 3<b>
    
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
    | RLA
    | RRCA
    | RRA
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
    | RRC_R8 _
    | RR_R8 _
    | RLC_R8 _
    | RL_R8 _
    | SLA_R8 _
    | SRA_R8 _
    | SRL_R8 _
    | SBC_R8_AR16 _
    | SBC_R8_D8 _
    | SET_R8 _
    | SUB_R8_AR16 _
    | SUB_R8_D8 _
    | SWAP_R8 _
    | XOR_R8_AR16 _
    | XOR_R8_D8 _
    | LDH_AR8_R8 _
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
    | LDH_R8_A8 _
    | LD_AR16_D8 _ 
        -> 12
    | ADD_R16_D8 _
    | JP_A16 _
    | LD_A16_R8 _
    | LD_R8_A16 _
    | PUSH_R16 _
    | RES_AR16 _
    | RET
    | RETI
    | RLC_AR16 _
    | RL_AR16 _
    | RR_AR16 _
    | RRC_AR16 _
    | SLA_AR16 _
    | SRA_AR16 _
    | SRL_AR16 _
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

    




