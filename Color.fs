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
