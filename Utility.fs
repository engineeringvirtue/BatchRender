module Utility
open System.Numerics
open System.Drawing
open System

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
    arr |> ref

let ParseTextInputBuffer = Array.choose (function | 0uy -> None | x -> Some x) >> System.Text.Encoding.Default.GetString