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
