module Render
open State
open System.Diagnostics
open System.Text.RegularExpressions
open System.Threading
open System.IO
open FSharp.Control
open RenderState

let (|Regex|_|) pattern str =
    let rmatch = Regex.Match (str, pattern)
    if rmatch.Success then
        rmatch.Groups |> Seq.toList |> List.map (fun x -> x.Value) |> Some
    else None

let (|Integer|_|) (str: string) =
    let mutable intvalue = 0
    if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
    else None

let ParseStrToMsg = function
    | Regex @"render started for (\d+)" [_; Integer renderlength] -> RenderStarted renderlength |> Some
    | Regex @"Rendering frame (\d+)" [_; Integer frame] -> RenderFrame frame |> Some
    | Regex @"ROP node endRender" _ -> Ok () |> RenderRes |> Some
    | Regex @"Couldn't find renderer .+" _ -> "ROP node not found!" |> Error |> RenderRes |> Some
    | Regex @"RAISING EXCEPTION.+" _ -> "An exception has occured inside hbatch!" |> Error |> RenderRes |> Some
    | Regex @"Unable to open file|Unknown file type" _ -> "File was not found!" |> Error |> RenderRes |> Some
    | _ -> None

let DoRender (mailbox:RenderMsg -> unit) hippath roppath (stop:ManualResetEvent) = async {
    try
        let processstartinfo = new ProcessStartInfo ()
        processstartinfo.FileName <- "hbatch"
        processstartinfo.Arguments <- hippath
        processstartinfo.RedirectStandardInput <- true
        processstartinfo.RedirectStandardOutput <- true
        processstartinfo.RedirectStandardError <- true

        processstartinfo.UseShellExecute <- false

        use proc = Process.Start (processstartinfo)
        proc.EnableRaisingEvents <- true

        let mutable res = Unchecked.defaultof<Result<unit,string>>

        let datarecieved (x:string) =
            let msg = x |> Option.ofObj
            msg |> Option.iter (fun str -> Output (sprintf "%s\n" str) |> mailbox)

            msg |> Option.bind ParseStrToMsg |> Option.iter (
                function
                    | RenderRes x ->
                        res <- x
                        stop.Set() |> ignore
                    | msg -> mailbox msg
            )

        proc.BeginErrorReadLine ()
        proc.BeginOutputReadLine ()

        proc.OutputDataReceived.Add (fun x -> datarecieved x.Data)
        proc.ErrorDataReceived.Add (fun x -> datarecieved x.Data)

        do! proc.StandardInput.WriteLineAsync (sprintf "render -V %s" roppath) |> Async.AwaitTask

        do! Async.AwaitWaitHandle stop |> Async.Ignore
        do! proc.StandardInput.WriteLineAsync "bye" |> Async.AwaitTask

        proc.WaitForExit ()

        do! Async.Sleep 5000 //5 second margin, just to be sure...
        res |> RenderRes |> mailbox

    with | x ->
        x.Message |> Error |> RenderRes |> mailbox //handle exceptions from stupid c#
}