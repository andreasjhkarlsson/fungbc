namespace FunGBC_iOS

open System
open System.Drawing
open System.IO
open Rom
open Foundation
open UIKit

[<Register ("GameboyViewController")>]
type GameboyViewController (handle:IntPtr) =
    inherit UIViewController (handle)

    override x.DidReceiveMemoryWarning () =
        // Releases the view if it doesn't have a superview.
        base.DidReceiveMemoryWarning ()
        // Release any cached data, images, etc that aren't in use.

    member this.GameboyView = this.View :?> GameboyView

    override this.ViewDidLoad () =
        base.ViewDidLoad ()

        let rom = File.ReadAllBytes("<rom>")

        let rom =
            Rom.load rom { new SaveFile with
                            member x.Load () = None
                            member x.Save _ = () }

        let gb = Gameboy.create rom (this.GameboyView :> Gpu.Renderer) { new Host.Host with
                                                                            member x.Idle _ = ()
                                                                            member x.Error _ = () }

        do gb |> Gameboy.start
        
    override x.ShouldAutorotateToInterfaceOrientation (toInterfaceOrientation) =
        // Return true for supported orientations
        if UIDevice.CurrentDevice.UserInterfaceIdiom = UIUserInterfaceIdiom.Phone then
           toInterfaceOrientation <> UIInterfaceOrientation.PortraitUpsideDown
        else
           true