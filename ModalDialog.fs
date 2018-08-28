module ModalDialog
open ImGuiNET
open System.Numerics
open Utility

type State = Opened | Closed

type Message = Open | Close

let Update state = function
    | Open -> Opened | Close -> Closed

let Render name ``default`` x state = function
    | Closed -> [], ``default``
    | Opened ->
        ImGui.OpenPopup (name)
        ImGui.BeginPopupModal (name) |> ignore

        let msg = x state
        let close = ImGui.Button ("Close", Vector2(ImGui.GetContentRegionAvailable().X, 20.0f))

        ImGui.EndPopup ()

        [close |> BoolToEv Close] |> List.choose id, msg