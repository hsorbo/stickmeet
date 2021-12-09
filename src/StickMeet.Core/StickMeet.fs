namespace StickMeet.Core
open Ariane.SaveFile.Contract
open FSharp.FGL
open System

type CaveWorker = { Suerveyors:string list; Explorers:string list}
module CaveWorker = 
    open System.Text.RegularExpressions
    let rxMatch (rx:Regex) s =
        let m = rx.Match(s)
        if m.Success then Some(m) else None
    
    let private split(delim:string)  (s:string) = 
        s.Split(delim,StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim()) |> Array.toList
    
    let private parseSection (rx:Regex) delim (s:string) = 
        match rxMatch rx s with
        | Some m -> m.Groups.[1].Value |> (split delim)
        | None -> []
        
    let parseExplorerSection (delim:string) (s:string) =
        //todo: can bee 3 things, just string, just explorer, explorer and surveyor
        let rxE = new Regex(".*<Explorer>(.*)</Explorer>")
        let rxS = new Regex(".*<Surveyor>(.*?)</Surveyor>")
        {Suerveyors = parseSection rxS delim s; Explorers = parseSection rxE delim s}

    

type Shape = {
    //TODO
    //[<XmlElement(ElementName = "SH")>]
    // "RadiusCollection", "RC"
    // "RadiusVector", "RV"
    // "angle", "ag"
    // "length", "lg"
    // "TensionProfile", "tp"
    // "TensionCorridor", "tc"
    HasProfileAzimut:bool
    HasProfileTilt:bool
    ProfileAzimut:float32
    ProfileTilt:float32
    //Description:string
}
type SurveyData = {
    Shape:Shape
    Azimut:float32
    ClosureToId:int
    Color:string
    Comment:string
    Date:DateTime
    Depth:float32
    DepthIn:float32
    Down:float32
    Explorer:string
    Excluded:bool
    FromId:int
    Id:int
    Inclination:float32
    Latitude:decimal
    Left:float32
    Length:float32
    Locked:bool
    Longitude:decimal
    Name:string
    Profiletype:CaveFileDataSRVDPRTY 
    Right:float32
    Section:string
    Type:CaveFileDataSRVDTY
    Up:float32
    
}

module SurveyData =
    let private srvdToSurveyData (x:CaveFileDataSRVD) = 
        {
            Azimut = x.AZ
            ClosureToId = x.CID
            Color = x.CL
            Comment = x.CM
            Date = x.DT
            Depth = x.DP
            DepthIn = x.DPI
            Down = x.D
            Explorer = x.EX
            Excluded = x.EXC
            FromId = x.FRID
            Id = x.ID
            Inclination = x.INC
            Latitude = x.LT
            Left = x.L
            Length = x.LG
            Locked = x.LK
            Longitude = x.LGT
            Name = x.NM
            Profiletype = x.PRTY
            Right = x.R
            Section = x.SC
            Type = x.TY
            Up = x.U
            Shape = { 
                HasProfileAzimut = x.SH.HPRA 
                HasProfileTilt = x.SH.HPRT
                ProfileAzimut = x.SH.PRAZ
                ProfileTilt = x.SH.PRT}
        }

    let fromCaveFile (caveFile:CaveFile) =  caveFile.Data |> Seq.map srvdToSurveyData

type Line = {
    Length: float32
}


type CaveGraph = Graph<SurveyData,string,Line>
module CaveGraph =
    let private cartographicLength (lineLengh:float32) (depthDifference:float32) = 
        if(lineLengh = 0.f) then 0.f
        else sqrt((lineLengh ** 2.f)-(depthDifference ** 2.f))

    let private cartographicLength2 station1 station2 line =
        cartographicLength line.Length (abs(station1.Depth - station2.Depth))

    let calculateLineLength caveGraph =
        caveGraph |> Undirected.Edges.fold (fun state _ _ line -> state + line.Length) 0f

    let calculateMapDistance caveGraph =
        caveGraph |> Undirected.Edges.fold (fun state ffrom tto line -> state + (cartographicLength2 ffrom tto line)) 0.f

module Stats =
    type StatBinner = SurveyData -> SurveyData -> Line -> string list
    type StatGather = SurveyData -> SurveyData -> Line -> float32

    let private addOrCreate k v map = map |> Map.change k (function | None -> v |> Some | Some y -> Some(y + v))
    let calcSwimLength (binner:StatBinner) (caveGraph:CaveGraph) = 
        caveGraph
        |> Undirected.Edges.fold (fun sstate stationFrom stationTo line -> 
            binner stationFrom stationTo line 
            |> Seq.fold (fun state bin -> state |> addOrCreate bin line.Length) sstate) Map.empty
    type Binning =
    | Explorer
    | Surveyor
    | Color
    | Year
    | Month

    let private defaultIfEmpty def lst = 
        match lst with 
        | [] -> [def]
        | x -> x

    let calcBinnedSwimLength binner (caveGraph:CaveGraph) = 
        let f (from:SurveyData) (too:SurveyData) (line:Line) = 
                let exp = CaveWorker.parseExplorerSection "," too.Explorer
                match binner with
                | Explorer -> exp.Explorers |> defaultIfEmpty "UNKNOWN" |> List.append ["TOTAL"] 
                | Surveyor -> exp.Suerveyors |> defaultIfEmpty "UNKNOWN" |> List.append ["TOTAL"]
                | Color -> [too.Color.Replace("#","").Replace("0x","")]
                | Year -> [string too.Date.Year]
                | Month -> [sprintf "%i-%i" too.Date.Year too.Date.Month]
        calcSwimLength f caveGraph


module Tmlu = 
    open System.Xml.Serialization
    open System.IO

    let openTmlu filename =
        let xmlserializer = XmlSerializer(typedefof<CaveFile>)
        use f = File.OpenRead filename
        xmlserializer.Deserialize f :?> CaveFile

    let toGraph (cavefile:CaveFile) : CaveGraph =
        let surveyData = cavefile |> SurveyData.fromCaveFile
        let surveymap = surveyData |> Seq.map (fun x -> (x.Id, x)) |> Map.ofSeq
        let stations = surveyData |> Seq.fold (fun state x -> state |> Vertices.add (x, x.Comment)) Graph.empty
        surveyData |> Seq.fold (fun state x -> 
            if x.FromId = -1 then state 
            else state |> Undirected.Edges.add (surveymap |> Map.find x.FromId, x, {Length = x.Length})) stations

    let openTmluGraph filename = openTmlu filename |> toGraph
    