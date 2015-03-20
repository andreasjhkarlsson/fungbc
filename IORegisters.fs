module IORegisters

open MemoryCell
open BitLogic

type IORegister(cell: MemoryCell) =
    inherit MemoryCell()
    override this.Value
        with get () = cell.Value
        and set value = cell.Value <- value

type InterruptEnableRegister (init) =
    inherit IORegister(ReadWriteCell(init))
    
    member this.VBlank           =  isBitSet 0 this.Value 
    member this.LCDC             =  isBitSet 1 this.Value
    member this.TimerOverflow    =  isBitSet 2 this.Value 
    member this.SerialIOTransfer =  isBitSet 3 this.Value
    member this.P10P13Flip       =  isBitSet 4 this.Value

type IORegisters () =
    
    let ie = InterruptEnableRegister(0uy)   

    member this.IE = ie