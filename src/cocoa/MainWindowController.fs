namespace FunGBC_Cocoa
open System
open Foundation
open AppKit
open Gameboy

[<Register ("MainWindowController")>]
type MainWindowController =

    inherit NSWindowController

    val mutable private gameboy: Gameboy option

    new () = { inherit NSWindowController ("MainWindow"); gameboy = None }
    new (handle : IntPtr) = { inherit NSWindowController (handle); gameboy = None }

    [<Export ("initWithCoder:")>]
    new (coder : NSCoder) = { inherit NSWindowController (coder); gameboy = None }

    override x.AwakeFromNib () =
        base.AwakeFromNib ()

    member x.Window with get () = base.Window :?> MainWindow
