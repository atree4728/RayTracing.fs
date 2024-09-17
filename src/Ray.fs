module Ray

open Vector

type Ray = { origin: Vector; direction: Vector }

let point ray (t: float) = ray.origin + t * ray.direction
