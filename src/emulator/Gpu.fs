module Gpu

open System.Runtime.InteropServices
open System.Diagnostics
open System.Threading
open IORegisters
open Tile
open MemoryCell
open Clock
open Units
open Constants
open BitLogic
open Interrupts
open Palette
open Host
open Sound

type OBJBGPriority = |Above |Behind
type SpritePalette = |Palette0 |Palette1

type SpriteAttribute (c0: MemoryCell,c1: MemoryCell,c2: MemoryCell,c3: MemoryCell) =
    member this.Y = c0.Value
    member this.X = c1.Value

    // Normalize coordinates (not sure why 1 & 9 instead of 0 & 8)
    member this.TX = (int c1.Value) - 1
    member this.TY = (int c0.Value) - 9

    member this.Tile = c2.Value

    // Flags
    member this.Priority = match c3.Value |> bitStateOf 7 with |CLEAR -> Above |SET -> Behind
    member this.YFlip = c3.Value |> isBitSet 6 |> not
    member this.XFlip = c3.Value |> isBitSet 5 |> not
    member this.Palette = match c3.Value |> bitStateOf 4 with |CLEAR -> Palette0 |SET -> Palette1

type TileMapSelect = |Map1 |Map0

type TileMapMode = Signed | Unsigned

type TileDataSelect = |Tiles0 |Tiles1

type LCDControl (init) =
    inherit ValueBackedIORegister(init)

    member this.BGDisplay = this.Value |> isBitSet 0
    member this.SpriteEnable = this.Value |> isBitSet 1
    member this.SpriteSize = if this.Value |> isBitSet 2 then (8,16) else (8,8)
    member this.BGTilemapSelect = if this.Value |> isBitSet 3 then Map1 else Map0
    member this.BGAndWindowTileDataSelect = if this.Value |> isBitSet 4 then Tiles1 else Tiles0
    member this.WindowEnabled = this.Value |> isBitSet 5
    member this.WindowTileMapSelect = if this.Value |> isBitSet 6 then Map1 else Map0
    member this.DisplayEnable = this.Value |> isBitSet 7

type Coincidence = |LYC_NE_LY |LYC_E_LY

type Mode = |HBlank |VBlank |SearchingOAMRAM |LCDDriverDataTransfer

type LCDStatus (init) =
    inherit ValueBackedIORegister(init)
    member this.LYCLYCoincidenceInterrupt
        with get () = this.GetBit 6 |> isSet
        and set value = setIfTrue value |> this.SetBit 6 
    member this.OAMInterrupt 
        with get () = this.GetBit 5 |> isSet
        and set value = setIfTrue value |> this.SetBit 5
    member this.VBlankInterrupt
        with get () = this.GetBit 4 |> isSet
        and set value = setIfTrue value |> this.SetBit 4
    member this.HBlankInterrupt
        with get () = this.GetBit 3 |> isSet
        and set value = setIfTrue value |> this.SetBit 3
    member this.Coincidence
        with get () = match this.GetBit 2 with |SET -> LYC_E_LY |CLEAR -> LYC_NE_LY
        and set value = this.SetBit 2 <| match value with |LYC_E_LY -> SET |LYC_NE_LY -> CLEAR
    member this.Mode
        with get () = match this.Value &&& 0x3uy with 
                        |0uy -> HBlank
                        |1uy -> VBlank
                        |2uy -> SearchingOAMRAM
                        |3uy -> LCDDriverDataTransfer
                        |_ -> raise <| System.Exception("Cannot happen (uint8&0x3 <= 3)")
        and set value = this.Value <- (this.Value &&& (~~~0x3uy)) |||
                        (match value with
                        |HBlank -> 0uy
                        |VBlank -> 1uy
                        |SearchingOAMRAM -> 2uy
                        |LCDDriverDataTransfer -> 3uy)

type LY(init) =
    inherit ValueBackedIORegister(init)

    // Writing from memory resets the register
    override this.MemoryValue with set value = base.MemoryValue <- 0x0uy


type PaletteMapRegister(init,initPalette) =
    inherit ValueBackedIORegister(init)

    // Store palette internally as array for instant lookup from value
    let mutable palette = Palette.toArray initPalette

    member this.Palette
        with get () = Palette.ofArray palette
        and set newPalette = palette <- Palette.toArray newPalette

    member this.Transparent = palette.[0]

    member this.Color index = palette.[(int this.Value >>> (index * 2)) &&& 0x3]

type TileDataBlock(memory: MemoryBlock,offset,count,mode) =
    let data =
        let createTile index = Array.sub memory (offset + index * 16) 16 |> TileData
        Array.init 256 createTile 
       
    member this.Item index = data.[if mode = Signed then int (int8 index) + 128 else index]


type TileMap(memory: MemoryBlock,offset,count) =
    let data = Array.sub memory offset count

    member this.Item (x,y) = (Array.get data (x + y * 32)).Value

type VRAM () =
    let memory = 8*kB |> readWriteMemoryBlock

    let oam = 160<byte> |> readWriteMemoryBlock

    let objectAttributeTable = [|0..39|] |> Array.map (fun index ->
            SpriteAttribute(oam.[4*index],
                            oam.[4*index+1],
                            oam.[4*index+2],
                            oam.[4*index+3])
        )

    let tiles1 = TileDataBlock(memory,0,256,Unsigned)

    let tiles0 = TileDataBlock(memory,2048,256,Signed)

    let tileMap0 = TileMap(memory,0x1800,1024)
    let tileMap1 = TileMap(memory,0x1C00,1024)

    member this.Tiles1 = tiles1

    member this.Tiles0 = tiles0

    member this.TileMap0 = tileMap0

    member this.TileMap1 = tileMap1

    member this.ObjectAttribute index = objectAttributeTable.[index]

    member this.ObjectAttributes = objectAttributeTable

    member this.MemoryBlock = memory

    member this.OAM = oam
        
type GPURegisters () =
    let lcdc = LCDControl(0uy)
    let lcds = LCDStatus(0uy)
    let scx = ValueBackedIORegister(0uy)
    let scy = ValueBackedIORegister(0uy)
    let wx = ValueBackedIORegister(0uy)
    let wy = ValueBackedIORegister(0uy)
    let bgp = PaletteMapRegister(0uy,Palette.Predefined.grayscale)
    let obp0 = PaletteMapRegister(0uy,Palette.Predefined.grayscale)
    let obp1 = PaletteMapRegister(0uy,Palette.Predefined.grayscale)
    let ly = LY(0uy)
    let lyc = ValueBackedIORegister(0uy)

    member this.LCDC = lcdc
    member this.LCDS = lcds
    member this.SCX = scx
    member this.SCY = scy
    member this.WX = wx
    member this.WY = wy
    member this.BGP = bgp
    member this.OBP0 = obp0
    member this.OBP1 = obp1
    member this.LY = ly
    member this.LYC = lyc


type RenderStage = |ScanOAM of int |ScanVRAM of int |HBlank of int |VBlank of int

type Speed = |Unlimited |Limit of int<Hz>

type GPU (sound: Sound.GBS,systemClock, interrupts: InterruptManager,host: Host) =

    let clock = Clock.derive systemClock systemClock.Frequency :?> DerivedClock

    let vram = VRAM()

    let registers = GPURegisters()

    let stopWatch = Stopwatch.StartNew()

    let drawLine y =
        
        let lineWidth = RESOLUTION.Width
        let screenHeight = RESOLUTION.Height
        let lcdc = registers.LCDC

        let drawTileLayer (tileMap: TileMap) (tileData: TileDataBlock) xOffset yOffset scroll = 
            let adjustedY = y + yOffset

            let rec drawPixel x =
                if x >= 0 then
                    let adjustedX = x + xOffset
                    if scroll || (adjustedX >= 0 && adjustedX < lineWidth) then
                        let tile = tileData.[tileMap.[((adjustedX % 256) / 8),((adjustedY % 256) / 8)] |> int]
                        do
                            Tile.decode8x8 tile (adjustedX % 8) (adjustedY % 8)
                            |> registers.BGP.Color
                            |> host.Renderer.SetPixel x y
                    do drawPixel (x - 1)
            if scroll || (adjustedY >= 0 && adjustedY < screenHeight) then do drawPixel (lineWidth - 1)

        let tileData = match registers.LCDC.BGAndWindowTileDataSelect with |Tiles0 -> vram.Tiles0 |Tiles1 -> vram.Tiles1

        // Draw background
        if lcdc.BGDisplay then do
            let map = match registers.LCDC.BGTilemapSelect with |Map0 -> vram.TileMap0 |Map1 -> vram.TileMap1
            drawTileLayer map tileData (int registers.SCX.Value) (int registers.SCY.Value) true
        
        // Draw window
        if lcdc.WindowEnabled then do
            let map = match registers.LCDC.WindowTileMapSelect with |Map0 -> vram.TileMap0 |Map1 -> vram.TileMap1
            drawTileLayer map tileData (-(int registers.WX.Value - 7)) (-(int registers.WY.Value)) false

        // Draw sprites
        if lcdc.SpriteEnable then do
            
            let (_, spriteHeight) = lcdc.SpriteSize

            let drawSprite (sprite: SpriteAttribute) = 
                // Find palette for this sprite
                let palette = match sprite.Palette with |Palette0 -> registers.OBP0 |Palette1 -> registers.OBP1

                // Calculate y coordinate in tile for this sprite (NOTE: May be larger than 7!)
                let tileY = if sprite.YFlip then (spriteHeight-1) - (sprite.TY - y) else (sprite.TY - y)

                // Find tile to be decoded
                let tile = if tileY > 7 then vram.Tiles1.[sprite.Tile+1uy |> int] else vram.Tiles1.[sprite.Tile |> int]

                // Wrap y
                let tileY = tileY % 8 

                let rec drawTileLine x =
                    if x >= 0 then
                        // Calculate destination x value
                        let screenX = sprite.TX - x

                        // Flip x if flag is set
                        let tileX = if sprite.XFlip then 7 - x else x

                        if screenX >= 0 && screenX < lineWidth then
                            let colorIndex = Tile.decode8x8 tile tileX tileY
                            // Apparently index 0 is always transparent (regardless of palette??????)
                            if colorIndex <> 0 then 
                                // Draw pixel if sprite is above background or if background is transparent
                                if sprite.Priority = Above || ((host.Renderer.GetPixel screenX y) = (registers.BGP.Transparent)) then
                                   do (palette.Color colorIndex) |> host.Renderer.SetPixel screenX y    

                        // Next column
                        do drawTileLine (x - 1)
                                
                do drawTileLine 7
            
            do
                vram.ObjectAttributes
                |> Array.filter (fun s -> s.TY >= y && s.TY <= (y + (spriteHeight-1)))
                |> Array.sortBy (fun s -> -(int s.X)) // Lower value X should overlap (draw after) larger x (maybe sacrifice this for performance?).
                |> Array.iter drawSprite


    let drawScreen = host.Renderer.Flush

    let isVBlank = function |VBlank _ -> true |_ -> false

    let stageAt time =
        // Timing constants
        let oamLineCycles = 80UL
        let vramLineCycles = 172UL
        let hBlankCycles = 204UL
        let lineCycles = oamLineCycles + vramLineCycles + hBlankCycles
        let allLinesCycles = lineCycles * uint64 RESOLUTION.Height 
        let vBlankCycles = 4560UL
        let frameCycles = allLinesCycles + vBlankCycles

        match time % frameCycles with
        | cycles when cycles < allLinesCycles ->
            let line = cycles / lineCycles |> int
            match cycles % lineCycles with
            | cycles when cycles > (oamLineCycles + vramLineCycles) ->
                HBlank(line)
            | cycles when cycles > oamLineCycles ->
                ScanVRAM(line)
            | cycles ->
                ScanOAM(line)
        | cycles ->
            VBlank ((cycles - allLinesCycles) / (vBlankCycles / 10UL) |> int)  
            
    let mutable lastStage = VBlank 0 

    let mutable fps = 0.0

    member this.VRAM = vram

    member this.Registers = registers

    member this.ForceRedraw () =
        do
            {0..143} |> Seq.iter drawLine   
            drawScreen () 

    member this.FPS = fps

    member val Speed = Limit 60<Hz> with get, set

    member this.Reset () =
        lastStage <- VBlank 0
        clock.Reset ()

    member this.Palette
        with get () =
            registers.BGP.Palette
        and set palette =
            registers.BGP.Palette <- palette
            registers.OBP0.Palette <- palette
            registers.OBP1.Palette <- palette

    member this.Update () =
        // Extract some registers
        let lcds = registers.LCDS
        let lcdc = registers.LCDC
        let ly = registers.LY
        let lyc = registers.LYC

        if lcdc.DisplayEnable then

            let stage = stageAt clock.Ticks

            if stage <> lastStage then
            
                match stage with
                | HBlank line ->
                    lcds.Mode <- Mode.HBlank

                    // Generate LCDC interrupt from HBlank?
                    if lcds.HBlankInterrupt then
                        interrupts.Current.Set <- LCDC

                    do drawLine line
                | VBlank t  ->
                    if not (lastStage |> isVBlank) then do

                        lcds.Mode <- Mode.VBlank

                        // Should VBlank generate LCDC interrupt?
                        if lcds.VBlankInterrupt then
                            interrupts.Current.Set <- LCDC

                        // Generate VBlank interrupt regardless of the above
                        if interrupts.Enable then
                            interrupts.Current.Set <- Interrupts.VBlank

                        do sound.Mixer |> Mixer.flush

                        do drawScreen ()

                        match this.Speed with
                        | Unlimited ->
                            ()
                        | Limit frequency ->
                            
                            let rec limit () =

                                let ticksLeft = ((float Stopwatch.Frequency) / (float frequency) - (float stopWatch.ElapsedTicks))

                                if ticksLeft >= 0.0 then
                                    
                                    do
                                        ((ticksLeft / (float) Stopwatch.Frequency))
                                        |> (*) (10.0**6.0)
                                        |> int
                                        |> host.Idle

                                    limit ()
                            
                            do limit ()
                            do sound.Mixer |> Mixer.sync

                        // Divide stopwatch ticks with this value to avoid precision errors
                        let precision = 10.0 ** 6.0

                        fps <- precision / float ((float stopWatch.ElapsedTicks) / ((float Stopwatch.Frequency) / precision))

                        stopWatch.Restart()
                                
                    ly.Value <- uint8 <| RESOLUTION.Height + t

                | ScanOAM line ->
                    lcds.Mode <- Mode.SearchingOAMRAM // Correct?
                    ly.Value <- uint8 <| line 

                    // Generate LCDC interrupt from OAM?
                    if lcds.OAMInterrupt then
                        interrupts.Current.Set <- LCDC

                | ScanVRAM _ ->
                    lcds.Mode <- Mode.LCDDriverDataTransfer // Correct?

            if lcds.LYCLYCoincidenceInterrupt then
                if ly.Value = lyc.Value then
                    interrupts.Current.Set <- LCDC
                    lcds.Coincidence <- LYC_E_LY
                else
                    lcds.Coincidence <- LYC_NE_LY

            lastStage <- stage
        else
            clock.Reset ()
            ly.Value <- 0uy
            lcds.Mode <- Mode.HBlank
