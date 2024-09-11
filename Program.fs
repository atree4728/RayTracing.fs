open Vector
open Hittable
open Camera


let world =
    Hittables
        [ Sphere
              { center = { x = 0; y = -100.5; z = -1 }
                radius = 100 }
          Sphere
              { center = { x = 0; y = 0; z = -1 }
                radius = 0.5 } ]

let camera =
    let aspectRatio = 16. / 9.
    let imageWidth = 400
    let samplesPerPixel = 100
    let maxDepth = 50
    Camera.create aspectRatio imageWidth samplesPerPixel maxDepth

let logger = eprintfn "%s"
let image = render logger camera world

printf $"{image}"
