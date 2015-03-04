module Register

type Register8Name =  |A |B |C |D |E |F |H |L
type Register16Name = |AF |BC |DE |HL |SP |PC


[<AbstractClass>]
type Register<'a>() =
    abstract value: 'a with get, set

type DataRegister<'a>(init: 'a) =
    inherit Register<'a>()

    let mutable data = init
    
    override this.value
        with get () = data
        and set (newValue) = data <- newValue

type DataRegister8 = DataRegister<uint8>

type DataRegister16 = DataRegister<uint16>

type CombinedDataRegister16 (R1: DataRegister8, R2: DataRegister8) =
    inherit Register<uint16>()

    override this.value
        with get () = ((uint16 R1.value) <<< 8) ||| (uint16 R2.value)
        and set (newValue) =
            R1.value <-  uint8 ((newValue >>> 8) &&& 255us)  // No idea if this is correct
            R2.value <- uint8 (newValue &&& 255us)           // Nor this.

type RegisterSet () =
    let _A = DataRegister8(0uy) 
    let _B = DataRegister8(0uy) 
    let _C = DataRegister8(0uy) 
    let _D = DataRegister8(0uy) 
    let _E = DataRegister8(0uy) 
    let _F = DataRegister8(0uy) 
    let _H = DataRegister8(0uy) 
    let _L = DataRegister8(0uy) 

    let _AF = CombinedDataRegister16(_A,_F)
    let _BC = CombinedDataRegister16(_B,_C)
    let _DE = CombinedDataRegister16(_D,_E)
    let _HL = CombinedDataRegister16(_H,_L) 

    let _SP = DataRegister<uint16>(0us) 
    let _PC = DataRegister<uint16>(0us) 

    member val A = _A
    member val B = _B
    member val C = _C
    member val D = _D
    member val E = _E
    member val F = _F
    member val H = _H
    member val L = _L

    member val AF = _AF
    member val BC = _BC
    member val DE = _DE
    member val HL = _HL

    member val SP = _SP
    member val PC = _PC

    member this.from8Name (name: Register8Name) =
        match name with
        | A -> _A
        | B -> _B
        | C -> _C
        | D -> _D
        | E -> _E
        | F -> _F
        | H -> _H
        | L -> _L

    member this.from16Name (name: Register16Name) =
        // WTF?! Why do I need to upcast here? Can't F# figure out that they belong to the same base class?
        match name with
        | AF -> _AF :> Register<uint16>
        | BC -> _BC :> Register<uint16>
        | DE -> _DE :> Register<uint16>
        | HL -> _HL :> Register<uint16>
        | PC -> _PC :> Register<uint16>
        | SP -> _SP :> Register<uint16>  