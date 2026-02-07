open StarTrek.Galaxy
open StarTrek.App

[<EntryPoint>]
let main argv =
    let seed = System.Random().Next()
    let state = initializeGame seed
    TerminalUI.run state
    0
