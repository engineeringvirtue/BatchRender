module State
open System.Threading
open System.IO
open Newtonsoft.Json
open Utility

//let (f) = float32<float>

type Frame = int
type RenderMsg = RenderFrame of Frame | RenderStarted of Frame | RenderRes of Result<unit, string> | Output of string //renderstarted contains max frame

type TextInput = byte array

type RenderProgress = | FrameProgress of Frame*Frame | Starting
type RenderState = Res of Result<unit, string> | RenderProgress of RenderProgress*ManualResetEvent | NotStarted
type Render = {Path:string; ROP:string; State:RenderState; Persistent:bool;}

type Options = {
    BG:System.Numerics.Vector4 ref;
}
type State = {
    Renders:Render list;

    HipPath:FileDialog.State;
    Options:Options*bool;

    RopNode:byte array ref;
    Persistent:bool ref;

    Status: string list;
    STD: string*bool ref;
}


type WindowUpdate = Close | Minimize | ChangeBG of System.Numerics.Vector4
type Message =
    | AddToQueue
    | SwapQueue of int*int
    | Render
    | StopRender of int

    | ToggleOptions
    | FileDialogMsg of FileDialog.Message

    | SetOptions of Options

    | ClearStatus
    | ClearSTD
    | RenderMsg of int*RenderMsg


let defaultopt = {BG=System.Numerics.Vector4 (0.0f) |> ref}
let defaultstate = {Renders=[]; HipPath=FileDialog.Make [".hiplc"]; Options=defaultopt, false; Persistent=ref false; RopNode=MakeTextInputBuffer ""; Status=[]; STD="Start of standard output.\n", ref true}
let mutable state =
    if File.Exists ("./config.json") then
        File.ReadAllText ("./config.json") |> JsonConvert.DeserializeObject<State>
            |> (fun x -> {x with Renders=x.Renders |> List.map (fun x -> {x with State=NotStarted})}) //reset render state
    else defaultstate

let saveState state =
    File.WriteAllText ("./config.json", JsonConvert.SerializeObject state)