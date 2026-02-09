module StarTrek.App.GameLoop

open StarTrek.GameTypes
open StarTrek.Enterprise
open StarTrek.Galaxy
open StarTrek.App
open StarTrek.App.Display

let mutable private inputMode : InputMode = CommandMode
let mutable private gameState : GameState option = None

let processCommand (input: string) (state: GameState) : string list * GameState =
    match input.Trim().ToUpper() with
    | "1" -> Commands.shortRangeCommand state
    | "2" -> Commands.longRangeScan state
    | "3" -> [], state // handled in handleCommandMode
    | "4" -> [], state // handled in handleCommandMode
    | "5" -> Commands.shieldControl state
    | "6" -> Commands.damageControlReport state
    | "7" -> [], state // handled in handleCommandMode
    | "HELP" -> Commands.help (), state
    | "Q" ->
        let endReport = statusReportLines state
        TerminalUI.requestStop()
        endReport, state
    | _ -> ["INVALID COMMAND. ENTER 0-7 OR HELP."], state

let private updatePrompt () =
    TerminalUI.setPromptText (promptText inputMode)

let private checkAndHandleGameEnd (state: GameState) : string list =
    match checkGameEnd state with
    | Some Victory ->
        let msgs = victoryMessages state
        TerminalUI.requestStop ()
        msgs
    | Some TimeExpired ->
        let msgs = timeExpiredMessages state
        TerminalUI.requestStop ()
        msgs
    | Some PlayerDestroyed ->
        let msgs = destroyedMessages ()
        TerminalUI.requestStop ()
        msgs
    | Some PlayerDeadInSpace ->
        let msgs = deadInSpaceMessages ()
        TerminalUI.requestStop ()
        msgs
    | None -> []

let private executeWithKlingonAttack (state: GameState) (action: GameState -> string list * GameState) : string list * GameState =
    let attackMsgs, stateAfterAttack = klingonAttack state
    let endMsgs = checkAndHandleGameEnd stateAfterAttack
    if endMsgs.Length > 0 then
        attackMsgs @ endMsgs, stateAfterAttack
    else
        let actionMsgs, finalState = action stateAfterAttack
        let endMsgsAfterAction = checkAndHandleGameEnd finalState
        attackMsgs @ actionMsgs @ endMsgsAfterAction, finalState

let private handleCommandMode (input: string) (state: GameState) =
    match input with
    | null | "" -> ()
    | _ ->
        match input.Trim().ToUpper() with
        | "0" ->
            let msgs = Commands.warpStart state
            inputMode <- WarpCourseInput
            updatePrompt ()
            if msgs.Length > 0 then TerminalUI.appendMessages msgs
        | "3" ->
            let msgs, canFire = Commands.phaserStart state
            if canFire then
                inputMode <- PhaserEnergyInput
                updatePrompt ()
            if msgs.Length > 0 then TerminalUI.appendMessages msgs
        | "4" ->
            let msgs, canFire = Commands.torpedoStart state
            if canFire then
                inputMode <- TorpedoCourseInput
                updatePrompt ()
            if msgs.Length > 0 then TerminalUI.appendMessages msgs
        | "5" ->
            let msgs, _ = Commands.shieldControl state
            if msgs.Length > 0 && msgs.[0] = "SHIELD CONTROL INOPERABLE" then
                TerminalUI.appendMessages msgs
            else
                inputMode <- ShieldEnergyInput
                updatePrompt ()
                if msgs.Length > 0 then TerminalUI.appendMessages msgs
        | "7" ->
            let msgs, newState = Commands.libraryComputer state
            gameState <- Some newState
            if msgs.Length > 0 && msgs.[0] = "COMPUTER DISABLED" then
                TerminalUI.appendMessages msgs
            else
                inputMode <- ComputerOptionInput
                updatePrompt ()
                if msgs.Length > 0 then TerminalUI.appendMessages msgs
        | _ ->
            let msgs, newState = processCommand input state
            gameState <- Some newState
            TerminalUI.refreshAll newState
            if msgs.Length > 0 then TerminalUI.appendMessages msgs

let private handleWarpCourseInput (input: string) =
    match input with
    | null | "" ->
        inputMode <- CommandMode
        updatePrompt ()
    | _ ->
        match Commands.warpValidateCourse (input.Trim()) with
        | Ok course ->
            inputMode <- WarpFactorInput course
            updatePrompt ()
        | Error msg ->
            TerminalUI.appendMessages [msg]
            inputMode <- CommandMode
            updatePrompt ()

let private handleWarpFactorInput (course: float) (input: string) (state: GameState) =
    match input with
    | null | "" ->
        inputMode <- CommandMode
        updatePrompt ()
    | _ ->
        let msgs, newState =
            executeWithKlingonAttack state (fun s ->
                Commands.warpValidateAndExecute course (input.Trim()) s)
        gameState <- Some newState
        inputMode <- CommandMode
        updatePrompt ()
        TerminalUI.refreshAll newState
        if msgs.Length > 0 then TerminalUI.appendMessages msgs

let private handleShieldEnergyInput (input: string) (state: GameState) =
    match input with
    | null | "" ->
        inputMode <- CommandMode
        updatePrompt ()
    | _ ->
        let msgs, newState = Commands.shieldValidateAndExecute (input.Trim()) state
        gameState <- Some newState
        inputMode <- CommandMode
        updatePrompt ()
        TerminalUI.refreshAll newState
        if msgs.Length > 0 then TerminalUI.appendMessages msgs

let private handlePhaserEnergyInput (input: string) (state: GameState) =
    match input with
    | null | "" ->
        inputMode <- CommandMode
        updatePrompt ()
    | _ ->
        let msgs, newState =
            executeWithKlingonAttack state (fun s ->
                Commands.phaserValidateAndExecute (input.Trim()) s)
        gameState <- Some newState
        inputMode <- CommandMode
        updatePrompt ()
        TerminalUI.refreshAll newState
        if msgs.Length > 0 then TerminalUI.appendMessages msgs

let private handleTorpedoCourseInput (input: string) (state: GameState) =
    match input with
    | null | "" ->
        inputMode <- CommandMode
        updatePrompt ()
    | _ ->
        let msgs, newState =
            executeWithKlingonAttack state (fun s ->
                Commands.torpedoValidateAndExecute (input.Trim()) s)
        gameState <- Some newState
        inputMode <- CommandMode
        updatePrompt ()
        TerminalUI.refreshAll newState
        if msgs.Length > 0 then TerminalUI.appendMessages msgs

let private handleComputerOptionInput (input: string) (state: GameState) =
    match input with
    | null | "" ->
        inputMode <- CommandMode
        updatePrompt ()
    | _ ->
        let msgs, newState = Commands.libraryComputerOption (input.Trim()) state
        gameState <- Some newState
        inputMode <- CommandMode
        updatePrompt ()
        TerminalUI.refreshAll newState
        if msgs.Length > 0 then TerminalUI.appendMessages msgs

let private onCommandEntered (input: string) =
    match gameState with
    | None -> ()
    | Some state ->
        match inputMode with
        | CommandMode -> handleCommandMode input state
        | WarpCourseInput -> handleWarpCourseInput input
        | WarpFactorInput course -> handleWarpFactorInput course input state
        | ShieldEnergyInput -> handleShieldEnergyInput input state
        | PhaserEnergyInput -> handlePhaserEnergyInput input state
        | TorpedoCourseInput -> handleTorpedoCourseInput input state
        | ComputerOptionInput -> handleComputerOptionInput input state

let run (initialState: GameState) =
    gameState <- Some initialState
    TerminalUI.init initialState
    TerminalUI.setCommandHandler onCommandEntered
    TerminalUI.setPagingHandler TerminalUI.advancePage
    let startReport = statusReportLines initialState
    TerminalUI.appendMessages startReport
    TerminalUI.runApplication ()
