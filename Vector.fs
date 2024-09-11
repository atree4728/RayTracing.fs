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

let dot u v = u.x * v.x + u.y * v.y + u.z * v.z

let cross u v =
    { x = u.y * v.z - u.z * v.y
      y = u.z * v.x - u.x * v.z
      z = u.x * v.y - u.y * v.x }

let norm_squared v = v.x ** 2 + v.y ** 2 + v.z ** 2
let norm = norm_squared >> sqrt

let normalize v = v / (norm v)
