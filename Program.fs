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
    Camera.create aspectRatio imageWidth

let logger = eprintfn "%s"
let image = render logger camera world

printf $"{image}"
