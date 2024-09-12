module Vector

type Vector =
    { x: float
      y: float
      z: float }

    static member inline (+)(u, v) =
        { x = u.x + v.x
          y = u.y + v.y
          z = u.z + v.z }

    static member inline (-)(u, v) =
        { x = u.x - v.x
          y = u.y - v.y
          z = u.z - v.z }

    static member inline (~-)(u) = { x = -u.x; y = -u.y; z = -u.z }

    static member inline (*)(u, v) =
        { x = u.x * v.x
          y = u.y * v.y
          z = u.z * v.z }

    static member inline (*)(t, v) =
        { x = t * v.x
          y = t * v.y
          z = t * v.z }

    static member inline (*)(v: Vector, t: float) = t * v
    static member inline (/)(v: Vector, t) = (1. / t) * v

type UnitVector = UnitVector of Vector

let dot u v = u.x * v.x + u.y * v.y + u.z * v.z

let cross u v =
    { x = u.y * v.z - u.z * v.y
      y = u.z * v.x - u.x * v.z
      z = u.x * v.y - u.y * v.x }

let normSquared v = v.x ** 2 + v.y ** 2 + v.z ** 2
let norm = normSquared >> sqrt

let normalize v = v / (norm v) |> UnitVector

let randomUnitVector =
    let rng = System.Random()
    let rand () = rng.NextDouble() * 2. - 1.

    fun () ->
        Seq.initInfinite (fun _ ->
            { x = rand ()
              y = rand ()
              z = rand () })
        |> Seq.find (fun p ->
            let lsq = normSquared p
            1e-160 < lsq && lsq <= 1)
        |> normalize
