module GameboyWindow

open System.Windows.Forms
open System.Drawing
open Constants

type GameboyWindow (title) as this =

    inherit Form()

    let scale = 3

    let width = RESOLUTION.Width

    let height = RESOLUTION.Height

    let framebuffer = new Bitmap(width, height)

    do
        this.Width <- width*scale
        this.Height <- height*scale

        this.Text <- title

        this.DoubleBuffered <- true

        this.FormBorderStyle <- FormBorderStyle.FixedSingle

    member this.PresentFrame bitmap =
        lock framebuffer (fun () -> Graphics.FromImage(framebuffer).DrawImage(bitmap,0,0,width,height))
        this.BeginInvoke(new System.Action(fun _ -> this.Invalidate ())) |> ignore

    override this.OnPaint args =
        base.OnPaint args
        lock framebuffer (fun () -> args.Graphics.DrawImage(framebuffer,0,0,width*scale,height*scale))
        


    