namespace StickMeet.Core
open Ariane.SaveFile.Contract
open FSharp.FGL
open System

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

    let private rx = new Text.RegularExpressions.Regex("<Explorer>(?<explorers>.*)</Explorer>.*?<Surveyor>(?<surveyors>.*?)</Surveyor>")
    let parseExplorerSection (delim:string) (s:string) =
        let m = rx.Match(s)
        let split (s:string) = s.Split(delim,StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.Trim())
        if m.Success then
            m.Groups.["explorers"].Value |> split, m.Groups.["surveyors"].Value |> split
        else 
            ([|"UNK"|],[|"UNK"|])

    let private addOrCreate k v map = map |> Map.change k (function | None -> v |> Some | Some y -> Some(y + v))
    
    let calcSwimLengthStatsSingle f (caveGraph:CaveGraph) = 
        caveGraph
        |> Undirected.Edges.fold (fun state stationFrom stationTo line -> addOrCreate (f stationFrom stationTo line) line.Length state) Map.empty


    let calcSwimLengthStatsMany f (caveGraph:CaveGraph) = 
        caveGraph
        |> Undirected.Edges.fold (fun sstate stationFrom stationTo line -> 
            f stationFrom stationTo line 
            |> Seq.fold (fun state exp -> state |> addOrCreate exp line.Length) sstate) Map.empty

    let calcSwimLengthExplorers delim graph = calcSwimLengthStatsMany (fun _ stationTo _ -> 
        parseExplorerSection delim stationTo.Explorer |> fst |> Array.append [|"TOTAL"|]) graph
    let calcSwimLengthSurveyers delim graph = calcSwimLengthStatsMany (fun _ stationTo _ -> parseExplorerSection delim stationTo.Explorer |> snd) graph
    
    let test graph =
        calcSwimLengthStatsMany (fun fromS toS line -> [toS.Color.Replace("#","").Replace("0x","")] ) graph 

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
    