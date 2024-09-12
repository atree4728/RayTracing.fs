module Hittable

open FsToolkit.ErrorHandling

open Vector
open Ray
open Color

type Lambertian = { albedo: Color }
type Metal = { albedo: Color }

type Material =
    | Lambertian of Lambertian
    | Metal of Metal

type Sphere =
    { center: Vector
      radius: float
      material: Material }

type Hittable =
    | Sphere of Sphere
    | Hittables of Hittable list

type Hit =
    { point: Vector
      normal: UnitVector
      material: Material
      t: float }

type Interval = { min: float; max: float }

let rec tryGetHit interval ray object =
    match object with
    | Sphere { center = center
               radius = radius
               material = material } ->
        let oc = center - ray.origin
        let a = ray.direction |> normSquared
        let h = dot ray.direction oc
        let c = normSquared oc - radius * radius
        let discriminant = h * h - a * c

        let trySqrt (f: float) = if f >= 0 then Some(sqrt f) else None

        let validate interval t =
            let { min = min; max = max } = interval
            if min < t && t < max then Some t else None

        option {
            let! dSqrt = trySqrt discriminant
            let! t = [ (h - dSqrt) / a; (h + dSqrt) / a ] |> List.tryPick (validate interval)
            let point = point ray t
            let normal = (point - center) / radius |> UnitVector

            return
                { point = point
                  normal = normal
                  material = material
                  t = t }
        }
    | Hittables hittables ->
        hittables
        |> List.choose (tryGetHit interval ray)
        |> function
            | [] -> None
            | hits -> hits |> List.minBy _.t |> Some

type Scattered = { attenuation: Color; ray: Ray }

let tryGetScattered
    rayIn
    { point = point
      normal = UnitVector normal
      material = material }
    =
    match material with
    | Lambertian { albedo = albedo } ->
        let direction =
            let (UnitVector randamized) = randomUnitVector ()
            let vector = normal + randamized
            if isNearZero vector then normal else vector

        let ray =
            { origin = point
              direction = direction }

        Some { attenuation = albedo; ray = ray }
    | Metal { albedo = albedo } ->
        let reflected =
            let proj = dot rayIn.direction normal * normal
            rayIn.direction - proj * 2.

        let ray =
            { origin = point
              direction = reflected }

        Some { attenuation = albedo; ray = ray }
