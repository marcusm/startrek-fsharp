module StarTrek.App.TerminalUI

open Terminal.Gui
open NStack
open StarTrek.GameTypes
open StarTrek.App

let scanPanelWidth = 26  // 24 content + 2 border
let topPanelHeight = 12  // 10 content + 2 border
let commandPanelHeight = 3

let mutable private messageLog : string list = []
// Queue of message lines waiting to be revealed page by page
let mutable private pendingMessages : string list = []
let mutable private isPaging = false
// How many lines from messageLog have been revealed so far
let mutable private revealedCount = 0

let private scanFrame = new FrameView(ustring.Make "Short Range Scan")
let private statusFrame = new FrameView(ustring.Make "Status")
let private messagesFrame = new FrameView(ustring.Make "Messages")
let private commandFrame = new FrameView(ustring.Make "Command")

let private scanLabel = new Label(ustring.Make "")
let private statusLabel = new Label(ustring.Make "")
let private messagesView = new TextView(ReadOnly = true)
let private hintLabel = new Label(ustring.Make "")
let private commandLabel = new Label(ustring.Make "COMMAND? > ")
let private commandField = new TextField(ustring.Make "")

// Callbacks registered by GameLoop
let mutable private onCommandCallback : (string -> unit) option = None
let mutable private onPagingCallback : (unit -> unit) option = None

let private conditionColorScheme (condition: string) =
    let fg =
        match condition with
        | "RED" -> Color.BrightRed
        | "YELLOW" -> Color.BrightYellow
        | "GREEN" -> Color.BrightGreen
        | _ -> Color.White
    let scheme = ColorScheme()
    let attr = Application.Driver.MakeAttribute(fg, Color.Black)
    scheme.Normal <- attr
    scheme.Focus <- attr
    scheme.HotNormal <- attr
    scheme.HotFocus <- attr
    scheme

let private refreshScan (state: GameState) =
    let lines = Display.renderScanLines state
    scanLabel.Text <- ustring.Make (System.String.Join("\n", lines))
    let condition = Commands.getCondition state
    let scheme = conditionColorScheme condition
    scanFrame.ColorScheme <- scheme
    scanLabel.ColorScheme <- scheme

let private refreshStatus (state: GameState) =
    let lines = Display.statusLines state
    // Blank first line to align with scan border, then status lines
    let padded = Array.append [| "" |] lines
    statusLabel.Text <- ustring.Make (System.String.Join("\n", padded))

let private messagesContentHeight () =
    // Usable height inside the frame for messages (excluding hint line)
    let bounds = messagesFrame.Bounds
    // Bounds.Height is content area (inside border). Reserve 1 row for hint.
    max 1 (bounds.Height - 1)

let private showRevealedMessages () =
    let visibleLines = messageLog |> List.take revealedCount
    let text = System.String.Join("\n", visibleLines)
    messagesView.Text <- ustring.Make text
    // Scroll to bottom
    messagesView.MoveEnd()

let private enterPagingMode () =
    isPaging <- true
    hintLabel.Text <- ustring.Make "-- press any key for more messages --"
    hintLabel.Visible <- true
    commandField.Visible <- false
    commandLabel.Visible <- false

let private exitPagingMode () =
    isPaging <- false
    hintLabel.Text <- ustring.Make ""
    hintLabel.Visible <- false
    commandField.Visible <- true
    commandLabel.Visible <- true
    commandField.SetFocus()

let advancePage () =
    let pageSize = messagesContentHeight ()
    let linesToReveal = min pageSize (List.length pendingMessages)
    revealedCount <- revealedCount + linesToReveal
    pendingMessages <- List.skip linesToReveal pendingMessages
    showRevealedMessages ()
    if pendingMessages.Length = 0 then
        exitPagingMode ()

let refreshAll (state: GameState) =
    refreshScan state
    refreshStatus state

let appendMessages (msgs: string list) =
    messageLog <- messageLog @ msgs
    let pageSize = messagesContentHeight ()
    if msgs.Length <= pageSize then
        // All fit on one page — reveal immediately
        revealedCount <- messageLog.Length
        pendingMessages <- []
        showRevealedMessages ()
    else
        // Show first page, queue the rest
        let firstPage = min pageSize msgs.Length
        revealedCount <- (messageLog.Length - msgs.Length) + firstPage
        pendingMessages <- List.skip firstPage msgs
        showRevealedMessages ()
        enterPagingMode ()

let setPromptText (text: string) =
    commandLabel.Text <- ustring.Make text

let requestStop () = Application.RequestStop()

let setCommandHandler (handler: string -> unit) =
    onCommandCallback <- Some handler

let setPagingHandler (handler: unit -> unit) =
    onPagingCallback <- Some handler

let init (initialState: GameState) =
    Application.Init()

    let top = Application.Top

    // Apply a non-blue color scheme to the messages view (after Init so Driver is available)
    let normalScheme = ColorScheme()
    normalScheme.Normal <- Application.Driver.MakeAttribute(Color.White, Color.Black)
    normalScheme.Focus <- Application.Driver.MakeAttribute(Color.White, Color.Black)
    normalScheme.HotNormal <- Application.Driver.MakeAttribute(Color.White, Color.Black)
    normalScheme.HotFocus <- Application.Driver.MakeAttribute(Color.White, Color.Black)

    // Scan panel (top-left, fixed width)
    scanFrame.X <- Pos.At 0
    scanFrame.Y <- Pos.At 0
    scanFrame.Width <- Dim.Sized scanPanelWidth
    scanFrame.Height <- Dim.Sized topPanelHeight

    scanLabel.X <- Pos.At 0
    scanLabel.Y <- Pos.At 0
    scanLabel.Width <- Dim.Fill()
    scanLabel.Height <- Dim.Fill()
    scanFrame.Add(scanLabel)

    // Status panel (top-right, fills remaining width)
    statusFrame.X <- Pos.Right scanFrame
    statusFrame.Y <- Pos.At 0
    statusFrame.Width <- Dim.Fill()
    statusFrame.Height <- Dim.Sized topPanelHeight

    statusLabel.X <- Pos.At 0
    statusLabel.Y <- Pos.At 0
    statusLabel.Width <- Dim.Fill()
    statusLabel.Height <- Dim.Fill()
    statusFrame.Add(statusLabel)

    // Command panel (bottom, fixed height)
    commandFrame.X <- Pos.At 0
    commandFrame.Y <- Pos.AnchorEnd(commandPanelHeight)
    commandFrame.Width <- Dim.Fill()
    commandFrame.Height <- Dim.Sized commandPanelHeight

    commandLabel.X <- Pos.At 0
    commandLabel.Y <- Pos.At 0
    commandField.X <- Pos.Right commandLabel
    commandField.Y <- Pos.At 0
    commandField.Width <- Dim.Fill()
    commandFrame.Add(commandLabel)
    commandFrame.Add(commandField)

    // Messages panel (middle, fills between top panels and command panel)
    messagesFrame.X <- Pos.At 0
    messagesFrame.Y <- Pos.Bottom scanFrame
    messagesFrame.Width <- Dim.Fill()
    messagesFrame.Height <- Dim.Fill() - Dim.Sized commandPanelHeight

    messagesView.X <- Pos.At 0
    messagesView.Y <- Pos.At 0
    messagesView.Width <- Dim.Fill()
    messagesView.Height <- Dim.Fill() - Dim.Sized 1
    messagesView.ColorScheme <- normalScheme

    hintLabel.X <- Pos.At 0
    hintLabel.Y <- Pos.AnchorEnd(1)
    hintLabel.Width <- Dim.Fill()
    hintLabel.Height <- Dim.Sized 1
    hintLabel.Visible <- false

    messagesFrame.Add(messagesView)
    messagesFrame.Add(hintLabel)

    top.Add(scanFrame)
    top.Add(statusFrame)
    top.Add(messagesFrame)
    top.Add(commandFrame)

    // Handle Enter key in command field
    commandField.add_KeyPress(fun args ->
        if args.KeyEvent.Key = Key.Enter then
            match onCommandCallback with
            | Some cb ->
                let input = commandField.Text.ToString()
                commandField.Text <- ustring.Make ""
                cb input
            | None -> ()
            args.Handled <- true)

    // Intercept any key at the root level for paging (fires before focused views)
    Application.RootKeyEvent <-
        System.Func<KeyEvent, bool>(fun _keyEvent ->
            if isPaging then
                match onPagingCallback with
                | Some cb -> cb ()
                | None -> advancePage ()
                true  // consume the key
            else
                false)  // let normal processing continue

    // Initialize state and render
    revealedCount <- 0
    refreshAll initialState

    // Focus the command field after layout is realized
    top.add_Ready(fun _ -> commandField.SetFocus())

let runApplication () =
    Application.Run()
    Application.Shutdown()
