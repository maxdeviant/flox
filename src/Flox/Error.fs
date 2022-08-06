module Flox.Error

let report line where message =
    printfn "[line %i] Error%s: %s" line where message

let error line message =
    report line "" message
