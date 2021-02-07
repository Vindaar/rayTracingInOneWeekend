import basetypes, math, hitrecord

type
  HittableKind* = enum
    htSphere
  Hittable* = object
    case kind*: HittableKind
    of htSphere: hSphere*: Sphere

  HittablesList* = object
    len: int
    size*: int # internal data size
    data*: ptr UncheckedArray[Hittable]

proc `=destroy`*(h: var HittablesList) =
  deallocShared(h.data)
  h.data = nil
  h.len = 0
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

proc initHittables*(size: int = 8): HittablesList =
  ## allocates memory for `size`, but remains empty
  let size = if size < 8: 8 else: size
  result.len = 0
  result.size = size
  if size > 0:
    result.data = cast[ptr UncheckedArray[Hittable]](allocShared0(sizeof(Hittable) * size))

proc resize*(h: var HittablesList, newSize: int = 0) =
  let newLen = if newSize > 0: newSize
               elif h.size < 8: 8
               else: h.size * 3 div 2
  var newBuf = cast[ptr UncheckedArray[Hittable]](allocShared0(sizeof(Hittable) * newLen))
  if h.size > 0:
    copyMem(newBuf[0].addr, h.data[0].addr, h.size * sizeof(Hittable))
    deallocShared(h.data)
  h.data = newBuf
  h.size = newLen

proc setLen*(h: var HittablesList, len: int) =
  if len > h.size:
    # resize
    h.resize(len)
  else:
    # just set new length
    h.len = len

proc `[]`*(h: HittablesList, idx: int): Hittable =
  assert idx < h.len
  result = h.data[idx]

proc `[]`*(h: var HittablesList, idx: int): var Hittable =
  assert idx < h.len
  result = h.data[idx]

proc `[]=`*(h: var HittablesList, idx: int, el: Hittable) =
  assert idx < h.len
  h.data[idx] = el

proc add*(h: var HittablesList, el: Hittable) =
  ## adds a new element to h. If space is there
  if h.len == h.size - 1:
    h.resize()
  inc h.len
  h[h.len - 1] = el

proc add*(h: var HittablesList, s: Sphere) =
  var ht = Hittable(kind: htSphere)
  ht.hSphere = s
  h.add ht

iterator items*(h: HittablesList): Hittable =
  for idx in 0 ..< h.len:
    yield h[idx]

proc hit*(h: Hittable, r: Ray, t_min, t_max: float, rec: var HitRecord): bool =
  case h.kind
  of htSphere: result = h.hSphere.hit(r, t_min, t_max, rec)

proc hit*(h: var HittablesList, r: Ray, t_min, t_max: float, rec: var HitRecord): bool =
  var tmpRec: HitRecord
  result = false
  var closestSoFar = t_max

  for obj in h:
    if obj.hit(r, t_min, closestSoFar, tmpRec):
      result = true
      closestSoFar = tmpRec.t
      rec = tmpRec

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

proc hit*(h: Hittable, r: Ray, t_min, t_max: float, rec: var HitRecord): bool {.gcsafe.} =
  case h.kind
  of htSphere: result = h.hSphere.hit(r, t_min, t_max, rec)
  of htDisk: result = h.hDisk.hit(r, t_min, t_max, rec)
  of htBvhNode: result = h.hBvhNode.hit(r, t_min, t_max, rec)
  of htXyRect: result = h.hXyRect.hit(r, t_min, t_max, rec)
  of htXzRect: result = h.hXzRect.hit(r, t_min, t_max, rec)
  of htYzRect: result = h.hYzRect.hit(r, t_min, t_max, rec)
  of htBox: result = h.hBox.hit(r, t_min, t_max, rec)

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

proc initSphere*(center: Point, radius: float, mat: Material): Sphere =
  result = Sphere(center: center, radius: radius, mat: mat)
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
