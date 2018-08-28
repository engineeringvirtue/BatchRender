module RenderEditor
open RenderState
open Utility
open System.IO
open ImGuiNET

type State = {
    HipPath:FileDialog.State
    ROP:string
    Persistent:bool
}

type Message =
    | FileDialogMsg of FileDialog.Message
    | ROPChange of string
    | PersistentChange of bool

let Update state msg =
    let {HipPath=fd} = state
    match msg with
        | FileDialogMsg msg -> {state with HipPath=msg |> FileDialog.Update fd}
        | ROPChange x -> {state with ROP=x}
        | PersistentChange x -> {state with Persistent=x}

let Make x =
    let filedialog = FileDialog.Make [".hip"; ".hiplc"; ".hipnc"]
    match x with
        | Some {Render.ROP=rop; Render.Path=path; Render.Persistent=persist} ->
            {HipPath=filedialog |> FileDialog.SelectFile path; ROP=rop; Persistent=persist}
        | None -> {HipPath = filedialog; ROP=""; Persistent=false}

let Render {HipPath=dialog; ROP=rop; Persistent=persist} =
    let choosepath = ImGui.Button "Choose .hip file..." |> BoolToEv (FileDialog.Open |> FileDialogMsg)
    ImGui.SameLine ()
    dialog.Selected |> Option.defaultValue "No path specified!" |> ImGui.Text

    let filemsgs = FileDialog.Render dialog |> List.map FileDialogMsg
    let ropchange = FInputText rop ("ROP Path", InputTextFlags.Default) |> Option.map ROPChange
    let persistchange = FCheckbox persist ("Persistent") |> Option.map PersistentChange

    [choosepath; ropchange; persistchange] |> List.choose id |> (@)filemsgs

let ToRender {HipPath=hippath; ROP=rop; Persistent=persist;} =
    match hippath.Selected, rop with
        | None, _ -> Error "Path is empty!"
        | _, "" -> Error "ROP path is empty!"
        | Some x, y when File.Exists (x) -> {Path=x; Render.ROP=y; State=NotStarted; Render.Persistent=persist} |> Ok
        | _ -> Error "Houdini file doesn't exist!"