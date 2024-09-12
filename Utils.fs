module Utils

let rand =
    let rng = System.Random()
    fun () -> rng.NextDouble()
