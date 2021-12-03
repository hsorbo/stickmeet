open Argu
open StickMeet.Core
open System

type StatsArguments =
    | [<MainCommand>] File of filename:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "Filename"

type Arguments = 
| [<CliPrefix(CliPrefix.None)>] Stats of ParseResults<StatsArguments>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Stats _ -> "stats"


let swimStats filename =
    filename 
        |> Tmlu.openTmlu  
        |> Tmlu.toGraph
        |> CaveGraph.calcSwimLengthExplorers "," 
        |> Map.toList 
        |> List.sortBy snd 
        |> Seq.iter (printfn "%A") 

[<EntryPoint>]
let main args =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "tmlu", errorHandler = errorHandler)
    let arguments = parser.ParseCommandLine args 
    match arguments.GetSubCommand () with
    | Stats statsArgs -> 
        statsArgs.GetResult(File) |> swimStats
    0

