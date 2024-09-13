open Vector
open Hittable
open Camera


let world =
    let r = System.Math.PI / 4. |> cos
    let left = Lambertian { albedo = { r = 0; g = 0; b = 1 } }
    let right = Lambertian { albedo = { r = 1; g = 0; b = 0 } }

    Hittables
        [ Sphere
              { center = { x = -r; y = 0; z = -1 }
                radius = r
                material = left }
          Sphere
              { center = { x = r; y = 0; z = -1 }
                radius = r
                material = right } ]

let camera =
    let aspectRatio = 16. / 9.
    let imageWidth = 400
    let samplesPerPixel = 100
    let maxDepth = 50
    let vFov = 90.<Utils.deg>
    Camera.create aspectRatio imageWidth samplesPerPixel maxDepth vFov

let logger = eprintfn "%s"
let image = render logger camera world

printf $"{image}"
