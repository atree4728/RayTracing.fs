let imageWidth = 256
let imageHeight = 256

let header =
    $"""P3
{imageWidth} {imageHeight}
255
"""

let scale d = d * 255.999 |> int

let body =
    seq {
        for j in 0 .. imageHeight - 1 do
            for i in 0 .. imageWidth - 1 do
                let r = float i / float (imageWidth - 1) |> scale
                let g = float j / float (imageHeight - 1) |> scale
                let b = 0. |> scale
                $"{r} {g} {b}\n"
    }
    |> Seq.reduce (+)

printf $"{header}{body}"
