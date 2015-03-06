module Register

open BitLogic

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

type DataRegister8(init: uint8) =
    inherit DataRegister<uint8>(init)

type FlagRegister(z,n,h,c) =
    inherit Register<uint8>()
    
    // Flags
    member val Z = z with get, set
    member val N = n with get, set
    member val H = h with get, set
    member val C = c with get, set

    member private this._ZNHC
        with set (z, n, h, c) =
            this.Z <- match z with |Some state -> state |None -> this.Z
            this.N <- match n with |Some state -> state |None -> this.N
            this.H <- match h with |Some state -> state |None -> this.H
            this.C <- match c with |Some state -> state |None -> this.Z
    
    // Poor man's swizzling (cookie for anyone who finds a nice generic syntax)
    member this.ZN   with set (z,n)     = this._ZNHC <- (Some z, Some n, None,   None  )
    member this.ZH   with set (z,h)     = this._ZNHC <- (Some z, None,   Some h, None  )
    member this.ZC   with set (z,c)     = this._ZNHC <- (Some z, None,   None,   Some c)
    member this.ZNH  with set (z,n,h)   = this._ZNHC <- (Some z, Some n, Some h, None  )
    member this.ZNC  with set (z,n,c)   = this._ZNHC <- (Some z, Some n, None,   Some c)
    member this.ZHC  with set (z,h,c)   = this._ZNHC <- (Some z, None,   Some h, Some c)
    member this.ZNHC with set (z,n,h,c) = this._ZNHC <- (Some z, Some n, Some h, Some c)
    member this.NH   with set (n,h)     = this._ZNHC <- (None,   Some n, Some h, None  )
    member this.NC   with set (n,c)     = this._ZNHC <- (None,   Some n, None,   Some c)
    member this.NHC  with set (n,h,c)   = this._ZNHC <- (None,   Some n, Some h, Some c)
    member this.HC   with set (h,c)     = this._ZNHC <- (None,   None,   Some h, Some c)
   
    // Calculate and decompose flag bits as a value.
    override this.value
        with get () =
            let zv = bitStateToValue this.Z
            let nv = bitStateToValue this.N
            let hv = bitStateToValue this.H
            let cv = bitStateToValue this.C
            (zv <<< 7) ||| (nv <<< 6) ||| (hv <<< 5) ||| (cv <<< 4)
        and set (value) =
            this.Z <- bitStateOf 7 value
            this.N <- bitStateOf 6 value
            this.H <- bitStateOf 5 value
            this.C <- bitStateOf 4 value
    

type DataRegister16 = DataRegister<uint16>

type CombinedDataRegister16 (R1: Register<uint8>, R2: Register<uint8>) =
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
    let _F = FlagRegister(CLEAR,CLEAR,CLEAR,CLEAR)
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
        | A -> _A :> Register<uint8>
        | B -> _B :> Register<uint8>
        | C -> _C :> Register<uint8>
        | D -> _D :> Register<uint8>
        | E -> _E :> Register<uint8>
        | F -> _F :> Register<uint8>
        | H -> _H :> Register<uint8>
        | L -> _L :> Register<uint8>

    member this.from16Name (name: Register16Name) =
        // WTF?! Why do I need to upcast here? Can't F# figure out that they belong to the same base class?
        match name with
        | AF -> _AF :> Register<uint16>
        | BC -> _BC :> Register<uint16>
        | DE -> _DE :> Register<uint16>
        | HL -> _HL :> Register<uint16>
        | PC -> _PC :> Register<uint16>
        | SP -> _SP :> Register<uint16>  