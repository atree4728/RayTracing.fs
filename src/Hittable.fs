module Hittable

open FsToolkit.ErrorHandling

open Vector
open Ray
open Color

type Lambertian = { albedo: Color }
type Metal = { albedo: Color; fuzz: float }
type Dielectric = { refractionIndex: float }

type Material =
    | Lambertian of Lambertian
    | Metal of Metal
    | Dielectric of Dielectric

type Sphere =
    { center: Vector
      radius: float
      material: Material }

type Hittable =
    | Sphere of Sphere
    | Hittables of Hittable array

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
        |> Array.choose (tryGetHit interval ray)
        |> function
            | [||] -> None
            | hits -> hits |> Array.minBy _.t |> Some

type Scattered = { attenuation: Color; ray: Ray }

let tryGetScattered
    { direction = direction }
    { point = point
      normal = UnitVector normal
      material = material }
    =
    match material with
    | Lambertian { albedo = albedo } ->
        let reflected =
            let (UnitVector randamized) = randomUnitVector ()
            let vector = normal + randamized
            if isNearZero vector then normal else vector

        let ray =
            { origin = point
              direction = reflected }

        Some { attenuation = albedo; ray = ray }
    | Metal { albedo = albedo; fuzz = fuzz } ->
        let reflected =
            let proj = dot direction normal * normal
            let (UnitVector direction) = direction - proj * 2. |> normalize
            let (UnitVector randamized) = randomUnitVector ()
            direction + fuzz * randamized

        let ray =
            { origin = point
              direction = reflected }

        Some { attenuation = albedo; ray = ray }
    | Dielectric { refractionIndex = refractionIndex } ->
        let attenuation = Color.white

        let out =
            let ri =
                if dot direction normal > 0 then
                    refractionIndex
                else
                    1.0 / refractionIndex

            let (UnitVector r) = normalize direction
            let cos = dot -r normal
            let sin = 1. - cos * cos |> sqrt

            let cannotRefract = ri * sin > 1

            let reflectance =
                let t = (1. - ri) / (1. + ri)
                let r0 = t * t
                r0 + (1. - r0) * (1. - abs cos) ** 5

            if cannotRefract || reflectance > Utils.rand () then
                let proj =
                    let cand = dot r normal * normal
                    if dot cand r >= 0 then cand else -cand

                r - proj * 2.
            else
                let perp = ri * (r + cos * normal)

                let prll =
                    let cand = sqrt (1. - normSquared perp) * normal
                    if dot cand r >= 0 then cand else -cand

                perp + prll

        let ray = { origin = point; direction = out }

        Some { attenuation = attenuation; ray = ray }
