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
    Azimut:float
    ClosureToId:int
    Color:string
    Comment:string
    Date:DateTime
    Depth:float
    DepthIn:float32
    Down:float
    Explorer:string
    Excluded:bool
    FromId:int
    Id:int
    Inclination:float32
    Latitude:float
    Left:float
    Length:float
    Locked:bool
    Longitude:float
    Name:string
    Profiletype:CaveFileDataSRVDPRTY 
    Right:float
    Section:string
    Type:CaveFileDataSRVDTY
    Up:float
    
}

module SurveyData =
    let srvdToSurveyData (x:CaveFileDataSRVD) = 
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
            Length = float x.LG
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

    let cartographicLength lineLengh depthDifference = 
        if(lineLengh = 0.) then 0.
        else sqrt((lineLengh ** 2.)-(depthDifference ** 2.))

    let cartographicLength2 station1 station2 line =
        cartographicLength line.Length (abs(station1.Depth - station2.Depth))

type Tree<'t> = | Node of 't * Tree<'t> list
module Tree =
    let fromTuple (n,s) = Node(n,s)
    let toTuple = function | Node (node, subtrees) -> (node, subtrees)
    let rec map mapper = function
    | Node (node,lst) -> Node(mapper node, lst |> List.map (map mapper))

    let rec fromAdjacencyList (adjacencyList:Map<_,list<_>>) nodeId =
        //TODO: guard against loops?
        let children = 
            match adjacencyList |> Map.tryFind nodeId with
            | Some x -> x
            | None -> [] //printfn "%A EOL?" nodeId
        Node(nodeId, [for y in children do fromAdjacencyList adjacencyList y])

    let pairwiseBf node = 
        let rec recurser nodes = 
            match nodes with
            | [] -> []
            | node::tail -> 
                let (data, children) = toTuple node
                let mine = [for c in children do yield (data, c |> toTuple |> fst)]
                mine @ recurser (tail @ children)
        recurser [node]

module GeoMap =
    let getCoordinates workTree =
        let startState =
            let startn = workTree |> Tree.toTuple |> fst
            let startGeo = {Latitude = startn.Latitude; Longitude = startn.Longitude}
            [(startn.Id, startGeo)] |> Map.ofList

        let folder state (parent,child) =
            let parentPos = state |> Map.find parent.Id
            let length = SurveyData.cartographicLength2 parent child child 
            //let length = child.Length
            let ny = Geolib.computeDestinationPoint parentPos length child.Azimut
            state |> Map.add child.Id ny
        workTree |> Tree.pairwiseBf |> List.fold folder startState

    let toAdjacencyList (data:CaveFileDataSRVD seq) =
        data 
            |> Seq.map (fun x -> (x.FRID, x.ID)) 
            |> Seq.groupBy fst
            |> Seq.map (fun (fromid,tups) -> (fromid,tups |> Seq.map snd |> Seq.toList))
            |> Map.ofSeq
    
    let toSurveyTrees (data:CaveFileDataSRVD seq) =
        let adjacencyList = data |> toAdjacencyList
        let all = data |> Seq.map (fun x -> (x.ID, x |> SurveyData.srvdToSurveyData)) |> Map.ofSeq
        adjacencyList 
            |> Map.find -1
            |> List.map (Tree.fromAdjacencyList adjacencyList)
            |> List.map (fun tree -> tree |> Tree.map (fun x ->  all |> Map.find x))

module Stats =
    type Line = {Length: float}
    type CaveGraph = Graph<SurveyData,string,Line>
    type StatBinner = SurveyData -> SurveyData -> Line -> string list
    type StatGather = SurveyData -> SurveyData -> Line -> float

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
        let f _ (too:SurveyData) _ = 
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

    let toGraph (cavefile:CaveFile) : Stats.CaveGraph =
        let surveyData = cavefile |> SurveyData.fromCaveFile
        let surveymap = surveyData |> Seq.map (fun x -> (x.Id, x)) |> Map.ofSeq
        let stations = surveyData |> Seq.fold (fun state x -> state |> Vertices.add (x, x.Comment)) Graph.empty
        surveyData |> Seq.fold (fun state x -> 
            if x.FromId = -1 then state 
            else state |> Undirected.Edges.add (surveymap |> Map.find x.FromId, x, {Length = x.Length})) stations

    let openTmluGraph filename = openTmlu filename |> toGraph
    