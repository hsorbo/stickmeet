namespace StickMeet.Core
open System

[<AutoOpen>]
module MathUtils =
    let rad x = (Math.PI / 180.0) * x
    let deg x = (180.0 / Math.PI) * x
    let positiveAngle x = (3600000. + x) % 360.
    let roundDigits (digits:int) (y:float) = Math.Round(y,digits)
    let acos x = Math.Acos(x)
    let atan2 x = Math.Atan2(x)
    let min (x:float) y = Math.Min(x,y)
    let max (x:float) y = Math.Max(x,y)

type Geo = {Latitude:float;Longitude:float}
module Geo =
    let private split (delim:string) (str:string) = str.Split(delim, StringSplitOptions.RemoveEmptyEntries)
    let private join separator (strs:string seq) = String.Join(separator, strs)

    let map m x = { Latitude = m x.Latitude; Longitude = m x.Longitude}
    let combine m x y = {Latitude = m x.Latitude y.Latitude; Longitude = m x.Longitude y.Longitude}
    let toRad x = x |> map rad 
    let toGeoString geo = sprintf "%.15f,%.15f,%i" geo.Longitude geo.Latitude 0
    let toGeoStrings geo = geo |> Seq.map toGeoString |> join " "
    
    // let parseGeoString s = 
    //     s 
    //     |> split " "
    //     |> Array.map (fun x -> x |> 
    //         let components = split "," )

module Geolib =
    //https://github.com/manuelbieh/geolib
    let private MINLAT = -90.
    let private MAXLAT = 90.
    let private MINLON = -180.
    let private MAXLON = 180.
    let private earthRadius = 6378137.


    //let robustAcos2 value = value |> max -1. |> min 1.
    let private robustAcos value =
        if value > 1. then 1.
        elif value < -1. then -1.
        else value
    
    // Computes the destination point given an initial point, a distance and a bearing
    // See http://www.movable-type.co.uk/scripts/latlong.html for the original code
    let computeDestinationPoint (point:Geo) distance bearing =
        let normalise lambda2 = 
            let l = deg lambda2
            if l < MINLON || l > MAXLON then
                // normalise to >=-180 and <=180° if value is >MAXLON or <MINLON
                deg ((lambda2 + 3. * Math.PI) % (2. * Math.PI)) - Math.PI
            else l
        let delta = distance / earthRadius;
        let theta = rad bearing
        let phi1, lambda1 = rad point.Latitude, rad point.Longitude
        let phi2 = Math.Asin(sin(phi1)*cos(delta)+cos(phi1)*sin(delta)*cos(theta))
        let lambda2 = lambda1 + atan2(sin(theta)*sin(delta)*cos(phi1), cos(delta)-sin(phi1)*sin(phi2))
        {Latitude = deg phi2; Longitude = normalise lambda2}

    let private getDistanceRadians rfrom rto = 
        let accuracy = 1.
        //accuracy = typeof accuracy !== 'undefined' && !isNaN(accuracy) ? accuracy : 1;
        let distance =
            acos(
                robustAcos(
                    sin(rto.Latitude) 
                    * sin(rfrom.Latitude) 
                    + cos(rto.Latitude) 
                    * cos(rfrom.Latitude) 
                    * cos(rfrom.Longitude - rto.Longitude))) 
                    * earthRadius;
        round(distance / accuracy) * accuracy;

    // Calculates the distance between two points.
    // This method is simple but also more inaccurate
    let getDistance ffrom fto = 
        getDistanceRadians (ffrom |> Geo.toRad) (fto |> Geo.toRad)

    let private getGreatCircleBearingRadians rOrig rDest =
        atan2(
            sin(rDest.Longitude - rOrig.Longitude) * cos(rDest.Latitude),
            cos(rOrig.Latitude) 
            * sin(rDest.Latitude)    
            - sin(rOrig.Latitude) 
            * cos(rDest.Latitude) 
            * cos(rDest.Longitude - rOrig.Longitude))
    
    //Gets great circle bearing of two points. See description of getRhumbLineBearing for more information
    let getGreatCircleBearing orig dest =
        let norm x = (x + 360.) % 360.
        getGreatCircleBearingRadians (orig |> Geo.toRad) (dest |> Geo.toRad) |> deg |> norm

    