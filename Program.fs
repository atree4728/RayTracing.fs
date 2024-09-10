open Color
open Vector

let imageWidth = 256
let imageHeight = 256

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
                let r = float i / float (imageWidth - 1)
                let g = float j / float (imageHeight - 1)
                let b = 0.
                let color = { r = r; g = g; b = b }
                $"{color}\n"
    }
    |> Seq.reduce (+)

printf $"{header}{body}"
