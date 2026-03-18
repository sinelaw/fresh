// F# syntax highlighting test
module Hello

let greet name =
    sprintf "Hello, %s!" name

type Config = {
    Version: string
    Enabled: bool
    Count: int
}

let config = {
    Version = "1.0"
    Enabled = true
    Count = 42
}

[<EntryPoint>]
let main argv =
    let message = greet "World"
    printfn "%s" message
    0
