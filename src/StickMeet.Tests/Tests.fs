module Tests
open Xunit
open StickMeet.Core
open Swensen.Unquote.Assertions

[<Fact>]
let ``parseExplorerSection parses`` () =
    let example = "<Explorer>Super Ted, Daffy Duck</Explorer><Surveyor>No Way, Jose</Surveyor>"
    test<@ 
        let (explorers, _) = CaveGraph.parseExplorerSection "," example 
        explorers |> Seq.contains "Super Ted"
        @>

