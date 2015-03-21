module Register

open BitLogic
open Units
open Constants

type FlagName = |Z |N |H |C

type Register8Name =  |A |B |C |D |E |F |H |L
type Register16Name = |AF |BC |DE |HL |SP |PC

// Any type of cpu register
[<AbstractClass>]
type Register<'a>() =
    abstract Value: 'a with get, set

    member this.Update fn = this.Value <- fn this.Value

// A regulat read/write register
type DataRegister<'a>(init: 'a) =
    inherit Register<'a>()

    let mutable data = init
    
    override this.Value
        with get () = data
        and set (newValue) = data <- newValue

// Register consisting of a single bit
type BitRegister(init) =
    inherit DataRegister<BitState>(init)

    member this.Clear = this.Value <- CLEAR
    member this.Set = this.Value <- SET
    member this.Flip = this.Update bitStateInvert

// A regular 8 bit register!
type DataRegister8(init: uint8) =
    inherit DataRegister<uint8>(init)

// The "F" register
type FlagRegister(z,n,h,c) =
    inherit Register<uint8>()
    
    // Flags
    member val Z = z with get, set // Zero flag
    member val N = n with get, set // Substract flag
    member val H = h with get, set // Half carry flag
    member val C = c with get, set // Carry flag

    member private this._ZNHC
        with set (z, n, h, c) =
            match z with |Some state -> this.Z <- state |None -> ()
            match n with |Some state -> this.N <- state |None -> ()
            match h with |Some state -> this.H <- state |None -> ()
            match c with |Some state -> this.C <- state |None -> ()
    
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
    override this.Value
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

    member this.FlagFromName = function |FlagName.Z -> this.Z |FlagName.N -> this.N |FlagName.H -> this.H |FlagName.C -> this.C 
    
type DataRegister16 = DataRegister<uint16>

type ProgramCounter (init) =
    inherit DataRegister16(init)
    member this.Advance (offset: int) = this.Value <- this.Value + (uint16 offset)

type StackPointer (init) =
    inherit DataRegister16(init)

// A 16 bit register composed of 2 8 bit ones.
type CombinedDataRegister16 (R1: Register<uint8>, R2: Register<uint8>) =
    inherit Register<uint16>()

    override this.Value
        with get () = ((uint16 R1.Value) <<< 8) ||| (uint16 R2.Value)
        and set (newValue) =
            R1.Value <-  uint8 ((newValue >>> 8) &&& 0xFFus)  // No idea if this is correct
            R2.Value <- uint8 (newValue &&& 0xFFus)           // Nor this.

// Collect a complete setup of cpu registers.
type RegisterSet () =
    let a = DataRegister8(0uy) 
    let b = DataRegister8(0uy) 
    let c = DataRegister8(0uy) 
    let d = DataRegister8(0uy) 
    let e = DataRegister8(0uy) 
    let f = FlagRegister(CLEAR,CLEAR,CLEAR,CLEAR)
    let h = DataRegister8(0uy) 
    let l = DataRegister8(0uy) 

    let af = CombinedDataRegister16(a,f)
    let bc = CombinedDataRegister16(b,c)
    let de = CombinedDataRegister16(d,e)
    let hl = CombinedDataRegister16(h,l) 

    let sp = StackPointer(0us) 
    let pc = ProgramCounter(0us) 

    let masterIE = BitRegister(CLEAR)


    member val A = a
    member val B = b
    member val C = c
    member val D = d
    member val E = e
    member val F = f
    member val H = h
    member val L = l

    member val AF = af
    member val BC = bc
    member val DE = de
    member val HL = hl

    member val SP = sp
    member val PC = pc

    member val MasterIE = masterIE


    member this.From8Name (name: Register8Name) =
        match name with
        | A -> a :> Register<uint8>
        | B -> b :> Register<uint8>
        | C -> c :> Register<uint8>
        | D -> d :> Register<uint8>
        | E -> e :> Register<uint8>
        | F -> f :> Register<uint8>
        | H -> h :> Register<uint8>
        | L -> l :> Register<uint8>

    member this.From16Name (name: Register16Name) =
        // WTF?! Why do I need to upcast here? Can't F# figure out that they belong to the same base class?
        match name with
        | AF -> af :> Register<uint16>
        | BC -> bc :> Register<uint16>
        | DE -> de :> Register<uint16>
        | HL -> hl :> Register<uint16>
        | PC -> pc :> Register<uint16>
        | SP -> sp :> Register<uint16>  

    // Print all registers
    member this.Print () =

        printfn 
          @"Registers:
            A  = 0x%02X
            B  = 0x%02X
            C  = 0x%02X
            D  = 0x%02X
            E  = 0x%02X
            F  = 0x%02X (Z = %d, N = %d, H = %d, C = %d)
            H  = 0x%02X
            L  = 0x%02X
            AF = 0x%04X
            BC = 0x%04X
            DE = 0x%04X
            HL = 0x%04X
            PC = 0x%04X
            SP = 0x%04X
            IE = %d" a.Value b.Value c.Value d.Value e.Value 
            f.Value (bitStateToValue f.Z) (bitStateToValue f.N) (bitStateToValue f.H) (bitStateToValue f.C)
            h.Value l.Value af.Value bc.Value
            de.Value hl.Value pc.Value sp.Value
            (bitStateToValue masterIE.Value)