open Color
open Vector
open Ray

let rayColor (ray: Ray) =
    let unitDirection = normalize ray.direction
    let scaler = (unitDirection.y + 1.) / 2.
    let white = { r = 1; g = 1; b = 1 }
    let blue = { r = 0.5; g = 0.7; b = 1 }
    (1. - scaler) * white + scaler * blue


let imageWidth = 400

let imageHeight =
    let aspectRatio = 16. / 9.
    float imageWidth / aspectRatio |> int |> max 1

let aspectRatio = float imageWidth / float imageHeight

let focalLength = 1.
let viewportHeight = 2.
let viewportWidth = viewportHeight * aspectRatio
let cameraCenter = { x = 0; y = 0; z = 0 }

let viewportU = { x = viewportWidth; y = 0; z = 0 }
let viewportV = { x = 0; y = -viewportHeight; z = 0 }

let pixelDeltaU = viewportU / float imageWidth
let pixelDeltaV = viewportV / float imageHeight

let viewportUpperLeft =
    cameraCenter
    - { x = 0; y = 0; z = focalLength }
    - viewportU / 2.
    - viewportV / 2.

let pixel00Loc = viewportUpperLeft + (pixelDeltaU + pixelDeltaV) * 0.5


let header =
    $"""P3
{imageWidth} {imageHeight}
255
"""

let logger (remaining: int) =
    eprintfn $"Scanlines remaining: {remaining}"

let body =
    seq {
        for j in 0 .. imageHeight - 1 do
            logger (imageHeight - j)

            for i in 0 .. imageWidth - 1 do
                let pixelCenter = pixel00Loc + pixelDeltaU * float i + pixelDeltaV * float j
                let rayDirection = pixelCenter - cameraCenter

                let ray =
                    { origin = cameraCenter
                      direction = rayDirection }

                let color = rayColor ray
                $"{color}\n"
    }
    |> Seq.reduce (+)

printf $"{header}{body}"
