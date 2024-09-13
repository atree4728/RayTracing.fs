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
      maxDepth: int
      center: Vector
      vFov: float<Utils.deg>
      lookFrom: Vector
      lookAt: Vector
      vUp: Vector
      pixel00Loc: Vector
      pixelDeltaU: Vector
      pixelDeltaV: Vector
      basis: {| u: Vector; v: Vector; w: Vector |} }

    static member create aspectRatio imageWidth samplesPerPixel maxDepth vFov lookFrom lookAt vUp =
        let imageHeight = float imageWidth / aspectRatio |> int |> max 1
        let center = lookFrom
        let aspectRatio = float imageWidth / float imageHeight

        let focalLength = lookFrom - lookAt |> norm
        let theta = vFov |> Utils.toRad
        let h = theta / 2.<Utils.rad> |> tan
        let viewportHeight = 2. * h * focalLength
        let viewportWidth = viewportHeight * aspectRatio

        let (UnitVector w) = lookFrom - lookAt |> normalize
        let (UnitVector u) = cross vUp w |> normalize
        let v = cross w u

        let viewportU = viewportWidth * u
        let viewportV = viewportHeight * -v

        let pixelDeltaU = viewportU / float imageWidth
        let pixelDeltaV = viewportV / float imageHeight

        let viewportUpperLeft = center - focalLength * w - viewportU / 2. - viewportV / 2.

        let pixel00Loc = viewportUpperLeft + (pixelDeltaU + pixelDeltaV) * 0.5

        { aspectRatio = aspectRatio
          imageWidth = imageWidth
          imageHeight = imageHeight
          samplesPerPixel = samplesPerPixel
          maxDepth = maxDepth
          center = center
          vFov = vFov
          lookFrom = lookFrom
          lookAt = lookAt
          vUp = vUp
          pixel00Loc = pixel00Loc
          pixelDeltaU = pixelDeltaU
          pixelDeltaV = pixelDeltaV
          basis = {| u = u; v = v; w = w |} }

let rec rayColor depth world ray =
    let interval = { min = 0.001; max = infinity }

    match depth <= 0, tryGetHit interval ray world with
    | true, _ -> Color.black
    | false, Some hit ->
        match tryGetScattered ray hit with
        | Some { attenuation = attenuation; ray = ray } ->
            let color = rayColor (depth - 1) world ray
            attenuation * color
        | None -> Color.black
    | false, None ->
        let (UnitVector unitDirection) = normalize ray.direction
        let scaler = (unitDirection.y + 1.) / 2.
        (1. - scaler) * Color.white + scaler * Color.blue

let render logger camera world =
    let header =
        $"""P3
{camera.imageWidth} {camera.imageHeight}
255
"""

    let logger (remaining: int) =
        logger $"Scanlines remaining: {remaining}"

    let body =
        let rand () = Utils.rand () - 0.5

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
                        |> Seq.map (rayColor camera.maxDepth world)
                        |> Seq.reduce (+)

                    let color = colors / float camera.samplesPerPixel |> gammanize
                    $"{color}\n"
        }
        |> Seq.reduce (+)

    header + body
