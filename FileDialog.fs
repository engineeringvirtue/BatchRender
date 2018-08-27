module FileDialog
open System
open System.IO
open ImGuiNET
open System.Drawing
open Utility
open System.Numerics

type File = string
type Folder = string
type DirContents = File of File | Folder of Folder
type State = {AllowedFileTypes:string list; Directory:string; Contents:DirContents list; Selected:File option;  Visible:bool}

type Message =
    | Dir of string
    | Select of string
    | Close
    | Open

let Update state msg =
    match msg with
        | Dir str when Directory.Exists str ->
            let folders = Directory.GetDirectories str |> Array.toList |> List.map Folder
            let {AllowedFileTypes=exts} = state
            let files = Directory.GetFiles str |> Array.toList |> List.map File
                        |> List.filter (function | File x -> Path.GetExtension (x) |> List.contains <| exts | _ -> true)
            {state with Contents = folders@files; Directory=str}
        | Select str -> {state with Selected=Some str}
        | Open -> {state with Visible=true}
        | Close -> {state with Visible=false}
        | _ -> state

let Make allowedfiles =
    {AllowedFileTypes=allowedfiles; Directory=""; Contents=[]; Selected=None; Visible=false} |> Update <| Dir Environment.CurrentDirectory

let GetFileName:string -> string = Path.GetFileName
let GetDirName:string -> string = Path.GetFileName >> sprintf "/%s/"

let Open = Open

let Render {AllowedFileTypes=x; Directory=dir; Contents=contents; Selected=selected; Visible=vis} updater =
    if vis then
        ImGui.OpenPopup ("Choose file...")
        ImGui.BeginPopupModal ("Choose file...") |> ignore

        ImGui.GetContentRegionAvailableWidth () |> ImGui.PushItemWidth

        let mutable buff = MakeTextInputBuffer dir
        let changedir = ImGui.InputText ("", buff.contents, TextBufferSize, InputTextFlags.Default, null) |> BoolToEv (ParseTextInputBuffer !buff |> Dir)

        ImGui.BeginChildFrame (uint32 1, ImGui.GetContentRegionAvailable()-Vector2(0.0f, 25.0f), WindowFlags.Default) |> ignore

        let renderdir name path =
            ImGui.PushStyleColor (ColorTarget.Text, ColToVec Color.Yellow)
            let sel = ImGui.Selectable name
            ImGui.PopStyleColor ()
            sel |> BoolToEv (Dir path)

        let parentdir = Directory.GetParent dir
        let up = if isNull parentdir then None else
                    renderdir "../" parentdir.FullName

        let evs = contents |> List.map (fun x ->
            match x with
                | File x ->
                    let name = GetFileName x
                    let sel = ImGui.Selectable (name, selected |> Option.map ((=)x) |> Option.defaultValue false)
                    sel |> BoolToEv (Select x)
                | Folder x ->
                    let name = GetDirName x
                    renderdir name x
        )

        ImGui.EndChildFrame ()

        let close = ImGui.Button ("Close", ImGui.GetContentRegionAvailable()) |> BoolToEv Close

        ImGui.PopItemWidth ()
        ImGui.EndPopup ()

        evs@[
            up
            changedir
            close
        ] |> List.choose id |> List.iter updater