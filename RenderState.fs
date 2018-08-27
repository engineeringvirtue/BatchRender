module RenderState
open System.Threading

type Frame = int
type RenderMsg = RenderFrame of Frame | RenderStarted of Frame | RenderRes of Result<unit, string> | Output of string //renderstarted contains max frame

type TextInput = byte array

type RenderProgress = | FrameProgress of Frame*Frame | Starting
type RenderState = Res of Result<unit, string> | RenderProgress of RenderProgress*ManualResetEvent | NotStarted
type Render = {Path:string; ROP:string; State:RenderState; Persistent:bool;}

type Options = {
    BG:System.Numerics.Vector4 ref;
}