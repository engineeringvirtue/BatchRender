module State
open System.Threading
open System.IO
open Newtonsoft.Json
open Utility
open RenderState

//let (f) = float32<float>

type State = {
    Renders:Render list;
    Parallel:int;

    RenderAdd:RenderEditor.State;
    Options:Options*bool;

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
    | RenderAddMsg of RenderEditor.Message

    | SetOptions of Options

    | ClearStatus
    | ClearSTD
    | RenderMsg of int*RenderMsg


let defaultopt = {BG=System.Numerics.Vector4 (0.0f) |> ref}
let defaultstate = {Renders=[]; Parallel=1; RenderAdd=RenderEditor.Make None; Options=defaultopt, false; Status=[]; STD="Start of standard output.\n", ref true}
let mutable state =
    if File.Exists ("./config.json") then
        File.ReadAllText ("./config.json") |> JsonConvert.DeserializeObject<State>
            |> (fun x -> {x with Renders=x.Renders |> List.map (fun x -> {x with State=NotStarted})}) //reset render state
    else defaultstate

let saveState state =
    File.WriteAllText ("./config.json", JsonConvert.SerializeObject state)