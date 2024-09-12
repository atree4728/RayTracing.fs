open Vector
open Hittable
open Camera


let world =
    let ground = Lambertian { albedo = { r = 0.8; g = 0.8; b = 0 } }
    let center = Lambertian { albedo = { r = 0.1; g = 0.2; b = 0.5 } }
    let left = Metal { albedo = { r = 0.8; g = 0.8; b = 0.8 } }
    let right = Metal { albedo = { r = 0.8; g = 0.6; b = 0.2 } }

    Hittables
        [ Sphere
              { center = { x = 0; y = -100.5; z = -1 }
                radius = 100
                material = ground }
          Sphere
              { center = { x = 0; y = 0; z = -1.2 }
                radius = 0.5
                material = center }
          Sphere
              { center = { x = -1; y = 0; z = -1 }
                radius = 0.5
                material = left }
          Sphere
              { center = { x = 1; y = 0; z = -1 }
                radius = 0.5
                material = right } ]

let camera =
    let aspectRatio = 16. / 9.
    let imageWidth = 400
    let samplesPerPixel = 100
    let maxDepth = 50
    Camera.create aspectRatio imageWidth samplesPerPixel maxDepth

let logger = eprintfn "%s"
let image = render logger camera world

printf $"{image}"
