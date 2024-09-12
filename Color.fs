module Color

type Color =
    { r: float
      g: float
      b: float }

    override this.ToString() =
        let r = this.r * 255.999 |> byte
        let g = this.g * 255.999 |> byte
        let b = this.b * 255.999 |> byte
        $"{r} {g} {b}"

    static member inline (+)(c, d) =
        { r = c.r + d.r
          g = c.g + d.g
          b = c.b + d.b }

    static member inline (*)(t, color) =
        { r = t * color.r
          g = t * color.g
          b = t * color.b }

    static member inline (*)(c, d) =
        { r = c.r * d.r
          g = c.g * d.g
          b = c.b * d.b }

    static member inline (/)(color: Color, t) = (1. / t) * color

    static member black = { r = 0; g = 0; b = 0 }
    static member blue = { r = 0.5; g = 0.7; b = 1 }
    static member white = { r = 1; g = 1; b = 1 }

let gammanize { r = r; g = g; b = b } = { r = sqrt r; g = sqrt g; b = sqrt b }
