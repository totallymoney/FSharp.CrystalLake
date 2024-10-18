module Tests.Expecto

open Expecto

module Main =

    [<EntryPoint>]
    let main args =
        runTestsInAssemblyWithCLIArgs [] args

