module Program

open System
open Update
open State
open System.Diagnostics
open System.Threading
open ImGuiOpenTK
open OpenTK
open OpenTK.Input
open ImGuiNET
open OpenTK.Graphics.OpenGL4


type BatchRenderWindow() =
    inherit ImGuiOpenTKWindow ("BatchRender", 640, 480)

    let (opts, _) = state.Options
    let mutable bgcolor = !opts.BG

    override x.ClearGL () =
        let color = bgcolor
        GL.ClearColor(color.X, color.Y, color.Z, color.W);
        GL.Clear(ClearBufferMask.ColorBufferBit);

    override x.ImGuiLayout () =
        RenderGui state |> List.iter (function | Minimize -> x.WindowState <- WindowState.Minimized | Close -> x.Close () | ChangeBG x -> bgcolor <- x)

[<EntryPoint>]
let main argv =

    let win = new BatchRenderWindow ()

    win.WindowState <- WindowState.Maximized
    win.WindowBorder <- WindowBorder.Hidden

    win.Show ()
    win.Start ()

    saveState state

    0