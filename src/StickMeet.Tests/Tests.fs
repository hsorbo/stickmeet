module Tests
open Xunit
open StickMeet.Core
open Swensen.Unquote.Assertions
open System

let between (a:double) b v = v <= Math.Max(a,b) && v >= Math.Min(a,b)

[<Fact>]
let ``parseExplorerSection parses`` () =
    let example = "<Explorer>Super Ted, Daffy Duck</Explorer><Surveyor>No Way, Jose</Surveyor>"
    test<@ 
        let (explorers, _) = CaveGraph.parseExplorerSection "," example 
        explorers |> Seq.contains "Super Ted"
        @>

[<Fact>]
let ``Location.calcDistance`` () =
    test <@ Location.calcDistance Location.oslo Location.bergen |> between 302000. 306000. @>

[<Fact>]
let ``Location.calcAzimuth`` () =
    //https://www.omnicalculator.com/other/azimuth
    let london = {Latitude = 51.50; Longitude = 0.0}
    let rio = {Latitude = -22.97; Longitude = -43.18}
    test <@ Location.calcAzimuth london rio |> round = round(219.35) @>
