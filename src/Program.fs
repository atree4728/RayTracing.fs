open Vector
open Hittable
open Camera
open Color

let world =
    let ground =
        let material = Lambertian { albedo = { r = 0.5; g = 0.5; b = 0.5 } }

        Sphere
            { center = { x = 0; y = -1000; z = -1 }
              radius = 1000
              material = material }

    let smalls =
        seq {
            for a in -11 .. 10 do
                for b in -11 .. 10 do
                    let center =
                        { x = float a + 0.9 * Utils.rand ()
                          y = 0.2
                          z = float b + 0.9 * Utils.rand () }

                    if norm (center - { x = 4; y = 0.2; z = 0 }) > 0.9 then
                        let material =
                            match Utils.rand () with
                            | p when p < 0.8 ->
                                let albedo = Color.random () * Color.random ()
                                Lambertian { albedo = albedo }
                            | p when p < 0.95 ->
                                let albedo = Color.random () / 2. + { r = 0.5; g = 0.5; b = 0.5 }
                                let fuzz = Utils.rand () / 2.
                                Metal { albedo = albedo; fuzz = fuzz }
                            | _ -> Dielectric { refractionIndex = 1.5 }

                        Sphere
                            { center = center
                              radius = 0.2
                              material = material }
        }
        |> Seq.toList

    let larges =
        [ let material = Dielectric { refractionIndex = 1.5 }

          Sphere
              { center = { x = 0; y = 1; z = 0 }
                radius = 1
                material = material }

          let material = Lambertian { albedo = { r = 0.4; g = 0.2; b = 0.1 } }

          Sphere
              { center = { x = -4; y = 1; z = 0 }
                radius = 1
                material = material }

          let material =
              Metal
                  { albedo = { r = 0.7; g = 0.6; b = 0.5 }
                    fuzz = 0 }

          Sphere
              { center = { x = 4; y = 1; z = 0 }
                radius = 1
                material = material } ]

    ground :: smalls @ larges |> List.toArray |> Hittables

let camera =
    let aspectRatio = 16. / 9.
    let imageWidth = 1200
    let samplesPerPixel = 500
    let maxDepth = 50
    let vFov = 20.<Utils.deg>
    let lookFrom = { x = 13; y = 2; z = 3 }
    let lookAt = { x = 0; y = 0; z = 0 }
    let vUp = { x = 0; y = 1; z = 0 }
    let defocusAngle = 0.6<Utils.deg>
    let focusDistance = 10

    Camera.create aspectRatio imageWidth samplesPerPixel maxDepth vFov lookFrom lookAt vUp defocusAngle focusDistance

let logger = eprintfn "%s"
let image = render logger camera world

printf $"{image}"
