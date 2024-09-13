module Utils

let rand =
    let rng = System.Random()
    fun () -> rng.NextDouble()

[<Measure>]
type rad

[<Measure>]
type deg

let degPerRad = 180. / System.Math.PI * 1.<deg / rad>
let toDeg (rad: float<rad>) = rad * degPerRad
let toRad (deg: float<deg>) = deg / degPerRad
