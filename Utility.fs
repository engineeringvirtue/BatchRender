module Utility
open System.Numerics
open System.Drawing
open System
open ImGuiNET
open System.Text.RegularExpressions
open ImGuiNET

let BoolToEv ev = function | true -> Some ev | false -> None

let ChannelToFloat (x:byte) =
    (x |> int |> float32)/255.0f

let ColToVec (c:System.Drawing.Color) =
    new Vector4 (c.R |> ChannelToFloat, c.G |> ChannelToFloat, c.B |> ChannelToFloat, c.A |> ChannelToFloat)

let TextBufferSize = 500 |> uint32
let MakeTextInputBuffer (txt:string) =
    let mutable arr = Array.create 500 (Unchecked.defaultof<byte>)
    let txtbytes = System.Text.Encoding.Default.GetBytes txt
    Array.blit txtbytes 0 arr 0 txtbytes.Length
    arr

let ParseTextInputBuffer = Array.choose (function | 0uy -> None | x -> Some x) >> System.Text.Encoding.Default.GetString

let FInputText x (label:string, flags:InputTextFlags) =
    let mutable buff = MakeTextInputBuffer x
    if ImGui.InputText (label, buff, TextBufferSize, flags, null) then
        buff |> ParseTextInputBuffer |> Some else None

let FIntSlider x (label:string, min:int, max:int, text:string) =
    let bref = ref x
    if ImGui.SliderInt (label, bref, min, max, text) then Some !bref else None

let FCheckbox x (label:string) =
    if ImGui.Checkbox (label, ref x) then not x |> Some else None

let (|Regex|_|) pattern str =
    let rmatch = Regex.Match (str, pattern)
    if rmatch.Success then
        rmatch.Groups |> Seq.toList |> List.map (fun x -> x.Value) |> Some
    else None

let (|Integer|_|) (str: string) =
    let mutable intvalue = 0
    if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
    else None