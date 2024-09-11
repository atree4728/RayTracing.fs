module Camera

open Color
open Vector
open Ray
open Hittable


type Camera =
    { aspectRatio: float
      imageWidth: int
      imageHeight: int
      samplesPerPixel: int
      center: Vector
      pixel00Loc: Vector
      pixelDeltaU: Vector
      pixelDeltaV: Vector }

    static member create aspectRatio imageWidth samplesPerPixel =
        let imageHeight = float imageWidth / aspectRatio |> int |> max 1
        let center = { x = 0; y = 0; z = 0 }
        let aspectRatio = float imageWidth / float imageHeight

        let focalLength = 1.
        let viewportHeight = 2.
        let viewportWidth = viewportHeight * aspectRatio

        let viewportU = { x = viewportWidth; y = 0; z = 0 }
        let viewportV = { x = 0; y = -viewportHeight; z = 0 }

        let pixelDeltaU = viewportU / float imageWidth
        let pixelDeltaV = viewportV / float imageHeight

        let viewportUpperLeft =
            center - { x = 0; y = 0; z = focalLength } - viewportU / 2. - viewportV / 2.

        let pixel00Loc = viewportUpperLeft + (pixelDeltaU + pixelDeltaV) * 0.5

        { aspectRatio = aspectRatio
          imageWidth = imageWidth
          imageHeight = imageHeight
          samplesPerPixel = samplesPerPixel
          center = center
          pixel00Loc = pixel00Loc
          pixelDeltaU = pixelDeltaU
          pixelDeltaV = pixelDeltaV }

let rayColor camera world ray =
    let white = { r = 1; g = 1; b = 1 }
    let blue = { r = 0.5; g = 0.7; b = 1 }

    let interval = { min = 0; max = infinity }

    match tryGetHit interval ray world with
    | Some { normal = normal } ->
        0.5
        * { r = normal.x + 1.
            g = normal.y + 1.
            b = normal.z + 1. }
    | None ->
        let unitDirection = normalize ray.direction
        let scaler = (unitDirection.y + 1.) / 2.
        (1. - scaler) * white + scaler * blue

let render logger camera world =
    let header =
        $"""P3
{camera.imageWidth} {camera.imageHeight}
255
"""

    let logger (remaining: int) =
        logger $"Scanlines remaining: {remaining}"

    let body =
        let rng = System.Random()
        let rand () = rng.NextDouble() - 0.5

        seq {
            for j in 0 .. camera.imageHeight - 1 do
                logger (camera.imageHeight - j)

                for i in 0 .. camera.imageWidth - 1 do
                    let colors =
                        Seq.init camera.samplesPerPixel (fun _ ->
                            let pixel =
                                camera.pixel00Loc
                                + (float i + rand ()) * camera.pixelDeltaU
                                + (float j + rand ()) * camera.pixelDeltaV

                            let origin = camera.center
                            let direction = pixel - origin

                            { origin = origin
                              direction = direction })
                        |> Seq.map (rayColor camera world)
                        |> Seq.reduce (+)

                    let color = colors / float camera.samplesPerPixel
                    $"{color}\n"
        }
        |> Seq.reduce (+)

    header + body
