import basetypes, math, random

type
  HitRecord* = object
    p*: Point
    normal*: Vec3
    t*: float
    frontFace*: bool
    mat*: Material

  Sphere* = object
    center*: Point
    radius*: float
    mat*: Material

  MaterialKind* = enum
    mkLambertian, mkMetal, mkDielectric
  Material* = object
    case kind*: MaterialKind
    of mkLambertian: mLambertian: Lambertian
    of mkMetal: mMetal: Metal
    of mkDielectric: mDielectric: Dielectric

  Lambertian* = object
    albedo*: Color

  Metal* = object
    albedo*: Color
    fuzz*: float

  Dielectric* = object
    ir*: float # refractive index (could use `eta`)

proc initLambertian*(a: Color): Lambertian =
  result = Lambertian(albedo: a)

proc initMetal*(a: Color, f: float): Metal =
  result = Metal(albedo: a, fuzz: f)

proc initDielectric*(ir: float): Dielectric =
  result = Dielectric(ir: ir)

proc initMaterial*[T](m: T): Material =
  when T is Lambertian:
    result = Material(kind: mkLambertian, mLambertian: m)
  elif T is Metal:
    result = Material(kind: mkMetal, mMetal: m)
  else:
    result = Material(kind: mkDielectric, mDielectric: m)

proc setFaceNormal*(rec: var HitRecord, r: Ray, outward_normal: Vec3) =
  rec.frontFace = r.dir.dot(outward_normal) < 0
  rec.normal = if rec.frontFace: outward_normal else: -outward_normal

proc hit*(s: Sphere, r: Ray, t_min, t_max: float, rec: var HitRecord): bool =
  let oc = r.orig - s.center
  let a = r.dir.length_squared()
  let half_b = oc.Vec3.dot(r.dir)
  let c = oc.length_squared() - s.radius * s.radius

  let discriminant = half_b * half_b - a * c
  if discriminant < 0:
    return false
  let sqrtd = sqrt discriminant

  # find nearest root that lies in acceptable range
  var root = (-half_b - sqrtd) / a
  if root < t_min or t_max < root:
    root = (-half_b + sqrtd) / a
    if root < t_min or t_max < root:
      return false

  rec.t = root
  rec.p = r.at(rec.t)
  let outward_normal = (rec.p - s.center) / s.radius
  rec.setFaceNormal(r, outward_normal.Vec3)
  rec.mat = s.mat

  result = true


proc scatter*(l: Lambertian, r_in: Ray, rec: HitRecord,
              attenuation: var Color, scattered: var Ray): bool =
  var scatter_direction = rec.normal + randomUnitVector()

  # catch degenerate scatter direction
  if scatter_direction.nearZero():
    scatter_direction = rec.normal

  scattered = initRay(rec.p, scatter_direction)
  attenuation = l.albedo
  result = true

proc scatter*(m: Metal, r_in: Ray, rec: HitRecord,
              attenuation: var Color, scattered: var Ray): bool =
  var reflected = unitVector(r_in.dir).reflect(rec.normal)
  scattered = initRay(rec.p, reflected + m.fuzz * randomInUnitSphere())
  attenuation = m.albedo
  result = scattered.dir.dot(rec.normal) > 0

proc reflectance(cosine, refIdx: float): float =
  ## use Schlick's approximation for reflectance
  var r0 = (1 - refIdx) / (1 + refIdx)
  r0 = r0 * r0
  result = r0 + (1 - r0) * pow(1 - cosine, 5)

proc scatter*(m: Dielectric, r_in: Ray, rec: HitRecord,
              attenuation: var Color, scattered: var Ray): bool =
  attenuation = color(1.0, 1.0, 1.0)
  let refractionRatio = if rec.frontFace: (1.0 / m.ir) else: m.ir

  let unitDirection = unitVector(r_in.dir)
  let cosTheta = min(dot(-unitDirection, rec.normal), 1.0)
  let sinTheta = sqrt(1.0 - cosTheta * cosTheta)

  let cannotRefract = refraction_ratio * sinTheta > 1.0
  var direction: Vec3

  if cannotRefract or reflectance(cosTheta, refractionRatio) > rand(1.0):
    direction = reflect(unitDirection, rec.normal)
  else:
    direction = refract(unitDirection, rec.normal, refractionRatio)

  scattered = initRay(rec.p, direction)
  result = true

proc scatter*(m: Material, r_in: Ray, rec: HitRecord,
              attenuation: var Color, scattered: var Ray): bool =
  case m.kind
  of mkLambertian: result = m.mLambertian.scatter(r_in, rec, attenuation, scattered)
  of mkMetal: result = m.mMetal.scatter(r_in, rec, attenuation, scattered)
  of mkDielectric: result = m.mDielectric.scatter(r_in, rec, attenuation, scattered)
