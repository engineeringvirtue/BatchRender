module Update
open State
open System.Numerics
open Render
open System.Threading
open System.Runtime.InteropServices
open System.IO
open ImGuiNET
open Utility
open RenderState

let UpdateRenders state (box:Message -> Unit) =
    let {Renders=renders; Status=status; Parallel=par} = state
    
    let updaterenderlist renders =
        match List.indexed renders |> List.tryFind (function | _,{State=NotStarted} -> true | _ -> false) with
            | Some (i, render) ->
                let {Path=hip; ROP=rop; Persistent=persist} = render

                let cancel = new ManualResetEvent (false)
                DoRender (fun x -> RenderMsg (i, x) |> box) hip rop cancel |> Async.Start
                ClearSTD |> box

                renders |> List.mapi (fun i2 x -> if i=i2 then {render with State=(Starting, cancel) |> RenderProgress} else x)
            | None -> renders

    let renders =
        List.fold (fun state acc -> state |> updaterenderlist) renders [0..par]

    {state with Renders=renders;}

module List =
    let replacei i x = List.mapi (fun i2 y -> if i=i2 then x else y)

let rec UpdateState (box:MailboxProcessor<Message>) = async {
    let {Renders=renders; Options=options,optdisplay; RenderEdit=redit; RenderAdd=radd; Status=status; STD=std, stdlog} = state
    let log x = {state with Status=x::status}

    let! msg = box.Receive ()


    let newstate =
        match msg with
            | AddToQueue  ->
                match RenderEditor.ToRender radd with
                    | Ok x -> {state with Renders=x::renders} | Error x -> log x
            | SwapQueue (x,y) ->
                let xitem = List.item x renders
                let yitem = List.item y renders
                let newrenders = renders |> List.mapi (fun i d -> if i=x then yitem elif i=y then xitem else d)
                {state with Renders=newrenders}
            | ClearStatus -> {state with Status=[]}
            | ClearSTD -> {state with STD="Cleared standard output.\n", stdlog}

            | OptionsDialog x -> {state with Options= options,x |> ModalDialog.Update optdisplay}
            | SetOptions opt -> {state with Options = (opt, optdisplay)}

            | RenderEdit i ->
                let render = List.item i renders
                {state with RenderEdit=(i, Some render |> RenderEditor.Make) |> Some}
            | RenderEditMsg x ->
                let newstate = redit |> Option.map (fun (i, state) -> i, x |> RenderEditor.Update state)
                {state with RenderEdit=newstate}
            | RenderEditClose ->
                match redit with
                    | Some (i,x) ->
                        match RenderEditor.ToRender x with
                            | Ok x -> {state with Renders=renders |> List.replacei i x; RenderEdit=None} | Error x -> log x
                    | None -> state

            | StopRender i ->
                let {State=x} = List.item i renders
                let renders = renders |> List.indexed
                let newrenders = match x with
                                    | RenderProgress (_, stop) ->
                                        stop.Set () |> ignore
                                        renders |> List.mapi (fun i2 (_, x) -> if i2=i then {x with State=NotStarted} else x)
                                    | _ ->
                                        renders |> List.filter (fun (i2,_) -> i<>i2) |> List.map (fun (_, x) -> x)

                {state with Renders=newrenders}

            | Render ->
                if List.exists (function | {State=RenderProgress _} -> true | _ -> false) renders |> not then
                    UpdateRenders state box.Post else log "Something is already rendering!"
            | RenderMsg (i, x) ->
                List.tryItem i renders |> Option.map (fun render ->
                    let newrender, newstate =
                        match x, render with
                            | RenderStarted f, {State=RenderProgress (_,t)} ->
                                {render with State=RenderProgress (FrameProgress (0,f), t)}, state
                            | RenderFrame f, {State=RenderProgress (FrameProgress (_, max), t)} ->
                                {render with State=RenderProgress (FrameProgress (f, max), t)}, state
                            | RenderRes (Error err), {Persistent=true} ->
                                let msg = sprintf "Error on persistent render %i (%s), restarting." i err
                                {render with State=NotStarted}, msg |> log
                            | RenderRes res, _ -> {render with State=res |> Res}, state
                            | Output x, _ when !stdlog -> render, {state with STD=std+x, stdlog}
                            | _ -> render, state

                    let newstate = {newstate with Renders=List.mapi (fun i2 x -> if i2=i then newrender else x) renders}
                    match newrender with
                        | {State=RenderProgress _} -> newstate
                        | _ -> UpdateRenders newstate box.Post //if state has gone outta progress then reupdate 

                ) |> Option.defaultValue state

            | RenderAddMsg x -> {state with RenderAdd=x |> RenderEditor.Update radd}
            | SetParallel x -> {state with Parallel=x}

    state <- newstate

    return! UpdateState box
}

let mailbox = (MailboxProcessor.Start UpdateState).Post


let RenderGui state =
    let {Renders=renders; Status=status; STD=std, stdlog; Parallel=par; RenderAdd=radd; RenderEdit=redit; Options=options, optdisplay} = state
    ImGui.BeginMainMenuBar () |> ignore

    let optopen = ImGui.MenuItem ("Options", true) |> BoolToEv ModalDialog.Open |> Option.map OptionsDialog

    let renderopts () = ImGui.ColorPicker4 ("Background Color", options.BG, ColorEditFlags.AlphaBar)
    let opt, col = ModalDialog.Render "Options" false renderopts () optdisplay
    let opt = opt |> List.map OptionsDialog

    let min = ImGui.MenuItem ("Minimize", true)
    let close = ImGui.MenuItem ("Close", true)

    //ImGui.BeginChild ("Status", Vector2 (ImGuiNative.igGetWindowContentRegionWidth (), 100.0f), true, WindowFlags.Default) |> ignore
    status |> List.iter (fun x ->
        ImGui.MenuItem (x, false) |> ignore
    )

    //ImGui.EndChild ()

    ImGui.EndMainMenuBar ()

    let modalmsg, reditmsg = match redit with
                                | Some (_, reditor) ->
                                    ModalDialog.Render "Edit render" [] RenderEditor.Render reditor ModalDialog.Opened
                                | None -> [], []

    ImGui.BeginWindow("Control Panel") |> ignore

    let editormsgs = RenderEditor.Render radd |> List.map RenderAddMsg

    let add = ImGui.Button "Add to render queue"
    ImGui.SameLine ()
    let render = ImGui.Button "Render"

    let changepar = par |> FIntSlider <| ("Paralellize renders", 0, 30, par.ToString ())
                        |> Option.map SetParallel

    let clearstatus = ImGui.Button "Clear status"

    ImGui.EndWindow ()

    ImGui.BeginWindow ("Render Panel") |> ignore
    ImGui.Text ("Renders:")

    let cancels =
        renders |> List.mapi (fun i {Path=path; ROP=rop; Persistent=persist; State=rstate} ->
            let name = sprintf "Render %i: %s-%s" i path rop

            ImGui.BeginChild (name, Vector2(ImGuiNative.igGetWindowContentRegionWidth (), 80.0f), true, WindowFlags.ResizeFromAnySide) |> ignore

            let dragflags = DragDropFlags.SourceNoDisableHover ||| DragDropFlags.SourceNoHoldToOpenOthers ||| DragDropFlags.AcceptBeforeDelivery ||| DragDropFlags.AcceptNoDrawDefaultRect

            ImGui.TextWrapped name |> ignore

            // if ImGui.BeginDragDropSource (dragflags, 0) then
            //     ImGui.Text name |> ignore
            //     ImGui.SetDragDropPayload ("RENDER", nativeint i, (sizeof<int>) |> uint32, Condition.Appearing) |> ignore
            //     ImGui.EndDragDropSource ()

            // if ImGui.BeginDragDropTarget () then
            //     let dragged = ImGui.AcceptDragDropPayload ("RENDER", dragflags)
            //     let index = dragged.Data.ToInt32 ()
            //     SwapQueue (index, i) |> mailbox

            //     ImGui.EndDragDropTarget ()

            let edited = ImGui.SmallButton "edit"
            ImGui.SameLine ()
            let cancelled = ImGui.SmallButton "cancel"
            if persist then
                ImGui.SameLine ()
                ImGui.Text "Persistent"

            let status, progress =
                match rstate with
                    | NotStarted -> "Not started", 0.0f
                    | RenderProgress (Starting, _) -> "Starting...", 0.0f
                    | RenderProgress (FrameProgress (x,y),_) -> sprintf "Frame %i/%i" x y, float32 x/float32 y
                    | Res (Ok _) -> "Finished!", 1.0f
                    | Res (Error x) -> x, 1.0f
            ImGui.ProgressBar (progress, Vector2 (ImGui.GetContentRegionAvailableWidth(), 20.0f), status)

            ImGui.EndChild ()

            [cancelled |> BoolToEv (StopRender i); edited |> BoolToEv (RenderEdit i)]
        ) |> List.collect id

    ImGui.EndWindow ()

    ImGui.BeginWindow ("Standard output stream from hbatch") |> ignore

    ImGui.BeginChild ("StreamString", Vector2 (ImGuiNative.igGetWindowContentRegionWidth (), ImGuiNative.igGetFrameHeightWithSpacing () - 50.0f), true, WindowFlags.AlwaysAutoResize) |> ignore
    std.Split [|'\n'|] |> Array.iter ImGui.TextWrapped
    ImGui.EndChild ()

    let clearstd = ImGui.Button "Clear"
    ImGui.SameLine ()
    let togglestd = ImGui.Checkbox ("Toggle output", stdlog)
    ImGui.EndWindow ()

    let msgs = cancels @ [
        add |> BoolToEv AddToQueue
        render |> BoolToEv Render
        clearstatus |> BoolToEv ClearStatus
        clearstd |> BoolToEv ClearSTD
        modalmsg |> List.isEmpty |> not |> BoolToEv RenderEditClose
        optopen
        changepar
    ]

    msgs |> List.choose id
        |> (@) editormsgs
        |> (@) opt
        |> (@) (reditmsg |> List.map RenderEditMsg)
        
        |> List.iter mailbox

    [
        col |> BoolToEv (ChangeBG !options.BG)
        min |> BoolToEv Minimize
        close |> BoolToEv Close
    ] |> List.choose id