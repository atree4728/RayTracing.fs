module Material

open Color

type Lambertian = { albedo: Color }

type Material = Lambertian of Lambertian
