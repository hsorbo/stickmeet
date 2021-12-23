open Argu
open StickMeet.Core
open System
open SharpKml.Dom
open SharpKml.Base

type StatsArguments =
    | [<MainCommand>] File of filename:string
    | Bin of Stats.Binning
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "Filename"
            | Bin _ -> "Bin by" 

type KmlArguments =
    | [<MainCommand>] File of filename:string
    | Output of filename:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "Filename"
            | Output _ -> "Output"
    

type Arguments = 
| [<CliPrefix(CliPrefix.None)>] Stats of ParseResults<StatsArguments>
| [<CliPrefix(CliPrefix.None)>] Kml of ParseResults<KmlArguments>
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Stats _ -> "stats"
            | Kml _ -> "kml"


let swimStats filename binner =
    filename 
        |> Tmlu.openTmlu 
        |> SurveyData.fromCaveFile 
        |> Stats.toGraph
        |> Stats.calcBinnedSwimLength binner
        |> Map.toList 
        |> List.sortBy snd 
        |> Seq.iter (printfn "%A") 

let kml tmlFile kmlFile =
    let geoVec geo = Vector(geo.Latitude, geo.Longitude)
    let addFeatures (c:Container) features = features |> Seq.iter c.AddFeature ; c

    let file = tmlFile |> Tmlu.openTmlu
    let trees = file.Data |> GeoMap.toSurveyTrees

    let treeToPlacemarks tree =
        let geoMap = GeoMap.getCoordinates tree
        tree
            |> Tree.map (fun x -> geoMap |> Map.find x.Id |> geoVec) 
            |> Tree.pairwiseBf
            |> List.map (fun (src,dst) -> CoordinateCollection([src;dst]))
            |> List.map (fun x -> Placemark(Geometry = LineString(Coordinates = x)))

    let doc = 
        trees 
        |> List.map (fun tree -> tree |> treeToPlacemarks |> addFeatures (Folder())) 
        |> addFeatures (Document())
       
    let kml = SharpKml.Engine.KmlFile.Create(doc, false)
    use stream = System.IO.File.Create(kmlFile)
    kml.Save(stream)
   

[<EntryPoint>]
let main args =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(programName = "tmlu", errorHandler = errorHandler)
    let arguments = parser.ParseCommandLine args 
    match arguments.GetSubCommand () with
    | Stats statsArgs -> 
        swimStats (statsArgs.GetResult(StatsArguments.File)) (statsArgs.GetResult(Bin))
    | Kml kmlArgs -> kml (kmlArgs.GetResult(File)) (kmlArgs.GetResult(Output))
    0

