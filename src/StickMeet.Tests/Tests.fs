module Tests
open Xunit
open StickMeet.Core
open Swensen.Unquote.Assertions
open System

let between (a:double) b v = v <= Math.Max(a,b) && v >= Math.Min(a,b)

[<Fact>]
let ``parseExplorerSection parses`` () =
    let example = "<Explorer>Super Ted, Daffy Duck</Explorer><Surveyor>No Way, Jose</Surveyor>"
    //let exampletodo = "<Explorer>Super Ted, Daffy Duck</Explorer>"
    //let exampletodo2 = "Super Ted"
    test<@ (CaveWorker.parseExplorerSection "," example).Explorers |> Seq.contains "Super Ted" @>

module GeolibTests =
    let london = {Latitude = 51.50; Longitude = 0.0}
    let rio = {Latitude = -22.97; Longitude = -43.18}
    let oslo = {Latitude = 59.913869; Longitude=10.752245}
    let bergen = {Latitude = 60.3912628; Longitude= 5.3220544}
    let eqToPrecision digits p1 p2 =
        //https://stackoverflow.com/questions/7167604/how-accurately-should-i-store-latitude-and-longitude
        let m = Geo.map (roundDigits digits)
        (m p1) = (m p2) 

    [<Fact>]
    let ``getDistance`` () =
        test <@ Geolib.getDistance oslo bergen = 305414. @>

    [<Fact>]
    let ``getGreatCircleBearing`` () =
        test <@ Geolib.getGreatCircleBearing london rio |> round = round(219.35) @>

    [<Fact>]
    let ``computeDestinationPoint`` () =
        //https://github.com/manuelbieh/geolib/blob/master/src/computeDestinationPoint.test.js
        let from = {Latitude = 52.518611; Longitude = 13.408056}
        let expected = {Latitude= 52.38386371; Longitude = 13.408056} //= {Latitude= 52.383712759112186; Longitude = 13.408056}
        test <@ Geolib.computeDestinationPoint from 15000. 180. |> eqToPrecision 5 expected @>

module TreeTests = 
    [<Fact>]
    let pairwiseBf () =
        let testTree = 
            Node(1,[
                Node(2,[
                    Node(21,[])
                    Node(22,[])])
                Node(3,[])])
        test <@ Tree.pairwiseBf testTree = [(1,2);(1,3);(2,21);(2,22)] @>
