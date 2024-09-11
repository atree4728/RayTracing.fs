module Hittable

open FsToolkit.ErrorHandling

open Vector
open Ray

type Sphere = { center: Vector; radius: float }

type Hittable = Sphere of Sphere

type Hit =
    { point: Vector
      normal: Vector
      t: float }

type Interval = { min: float; max: float }

let tryGetHit interval object ray =
    match object with
    | Sphere { center = center; radius = radius } ->
        let oc = center - ray.origin
        let a = ray.direction |> norm_squared
        let h = dot ray.direction oc
        let c = norm_squared oc - radius * radius
        let discriminant = h * h - a * c

        let trySqrt (f: float) = if f >= 0 then Some(sqrt f) else None

        let validate interval t =
            let { min = min; max = max } = interval
            if min < t && t < max then Some t else None

        option {
            let! dSqrt = trySqrt discriminant
            let! t = [ (h - dSqrt) / a; (h + dSqrt) / a ] |> List.tryPick (validate interval)
            let point = point ray t
            let normal = (point - center) / radius

            return
                { point = point
                  normal = normal
                  t = t }
        }
