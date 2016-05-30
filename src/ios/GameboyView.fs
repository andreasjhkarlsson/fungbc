namespace FunGBC_iOS

open System
open Foundation
open UIKit
open CoreGraphics
open System.ComponentModel
open Constants

[<Register("GameboyView")>]
[<DesignTimeVisible (true)>]
type GameboyView (handle: IntPtr) =

    inherit UIView (handle)

    let frameWidth = RESOLUTION.Width
    let frameHeight = RESOLUTION.Height

    let drawBuffer = Array.create (frameWidth*frameHeight*4) 0uy

    let mutable frameBuffer = None

    override this.Draw (rect) =

        lock this (fun _ ->
            match frameBuffer with
            | Some (bitmap: CGBitmapContext) ->
                
                let ctx = UIGraphics.GetCurrentContext ()

                let destWidth = float this.Frame.Width
                let destHeight = (float this.Frame.Width) / (float frameWidth) * (float frameHeight)

                let destRect = new CGRect(0.0,((float this.Frame.Height) - destHeight)/2.0,destWidth,destHeight)

                do ctx.DrawImage(destRect,bitmap.ToImage())

            | None ->
                ()
        )

    interface Gpu.Renderer with

        member this.SetPixel x y color =

            let offset = ((frameHeight-1) - y) * frameWidth * 4 + x * 4

            drawBuffer.[offset] <- byte ((color >>> 24) &&& 0xff)
            drawBuffer.[offset+1] <- byte ((color >>> 16) &&& 0xff)
            drawBuffer.[offset+2] <- byte ((color >>> 8) &&& 0xff)
            drawBuffer.[offset+3] <- byte ((color >>> 0) &&& 0xff)

        member this.GetPixel x y =
            
            let offset = ((frameHeight-1) - y) * frameWidth * 4 + x * 4

            (int drawBuffer.[offset] <<< 24) |||
            (int drawBuffer.[offset+1] <<< 16) |||
            (int drawBuffer.[offset+2] <<< 8) |||
            (int drawBuffer.[offset+3])

        member this.Flush () =
            
            lock this (fun _ ->
                frameBuffer <-
                    Some <| new CGBitmapContext(drawBuffer,
                                                nint frameWidth,
                                                nint frameHeight,
                                                nint 8,
                                                nint (frameWidth*4),
                                                CGColorSpace.CreateDeviceRGB(),
                                                CGImageAlphaInfo.PremultipliedFirst)
            )

            do this.InvokeOnMainThread (fun () -> this.SetNeedsDisplay ())

            



        