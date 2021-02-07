import macros, math, random

type
  Image* = object
    width*, height*: int

  Vec3* = object
    arr*: array[3, float]

  Color* = distinct Vec3
  Point* = distinct Vec3

  ColorU8* = tuple[r, g, b: uint8]

  Ray* = object
    orig*: Point
    dir*: Vec3

proc vec3*(x, y, z: float): Vec3 = Vec3(arr: [x, y, z])
proc color*(r, g, b: float): Color = Color(Vec3(arr: [r, g, b]))
proc point*(x, y, z: float): Point = Point(Vec3(arr: [x, y, z]))

proc `[]=`*(v: var Vec3; ix: int; c: float): void {.inline.} = v.arr[ix] = c
proc `[]`*(v: Vec3; ix: int): float {.inline.} = v.arr[ix]
proc `[]`*(v: var Vec3; ix: int): var float {.inline.} = v.arr[ix]

proc dot*(v, u: Vec3): float =
  for i in 0 ..< 3:
    result += v[i] * u[i]

proc cross*(v, u: Vec3): Vec3 =
  result[0] = v[1] * u[2] - v[2] * u[1]
  result[1] = v[2] * u[0] - v[0] * u[2]
  result[2] = v[0] * u[1] - v[1] * u[0]

template `.`*(c: Color, field: untyped): untyped =
  when astToStr(field) == "r":
    c[0]
  elif astToStr(field) == "g":
    c[1]
  elif astToStr(field) == "b":
    c[2]
  else:
    error("Invalid field " & astToStr(field) & " for Color!")

template `.`*(p: Point, field: untyped): untyped =
  when astToStr(field) == "x":
    p[0]
  elif astToStr(field) == "y":
    p[1]
  elif astToStr(field) == "z":
    p[2]
  else:
    error("Invalid field " & astToStr(field) & " for Point!")

template `.`*(v: Vec3, field: untyped): untyped =
  when astToStr(field) == "x":
    v[0]
  elif astToStr(field) == "y":
    v[1]
  elif astToStr(field) == "z":
    v[2]
  else:
    error("Invalid field " & astToStr(field) & " for Vec3!")

proc `+`*(v: Vec3): Vec3 {.inline.} = v
proc `-`*(v: Vec3): Vec3 {.inline.} =
  for ii in 0 ..< 3:
    result[ii] = -v[ii]
template makeMath(op: untyped): untyped =
  proc `op`*(v, u: Vec3): Vec3 {.inline.} =
    for ii in 0 ..< 3:
      result[ii] = op(v[ii], u[ii])
  proc `op`*(v: Vec3; val: float): Vec3 {.inline.} =
    for ii in 0 ..< 3:
      result[ii] = op(v[ii], val)
  proc `op`*(val: float; v: Vec3): Vec3 {.inline.} =
    for ii in 0 ..< 3:
      result[ii] = op(val, v[ii])
makeMath(`+`)
makeMath(`-`)
makeMath(`/`)
makeMath(`*`)
proc `+=`*(v: var Vec3, u: Vec3) {.inline.} =
  v = v + u
proc `-=`*(v: var Vec3, u: Vec3) {.inline.} =
  v = v - u

proc length_squared*(v: Vec3): float {.inline.} =
  result = v.x * v.x + v.y * v.y + v.z * v.z
proc length*(v: Vec3): float {.inline.} =
  result = sqrt(v.length_squared)

proc normalize*(v: Vec3): Vec3 = v / length(v)

proc unitVector*(v: Vec3): Vec3 = normalize(v)

proc nearZero*(v: Vec3): bool =
  ## return true if the vector is close to 0 in all dim.
  const epsilon = 1e-8
  result = abs(v[0]) < epsilon and abs(v[1]) < epsilon and abs(v[2]) < epsilon

proc rotate*(v: Vec3, phi, theta, gamma: float): Vec3 =
  ## rotates the vector `v` around the angles phi and theta
  result[0] = cos(phi) * cos(theta) * v[0] +
              (cos(phi) * sin(theta) * sin(gamma) - sin(phi) * cos(gamma)) * v[1] +
              (cos(phi) * sin(theta) * cos(gamma) + sin(phi) * sin(gamma)) * v[2]
  result[1] = sin(phi) * cos(theta) * v[0] +
              (sin(phi) * sin(theta) * sin(gamma) + cos(phi) * cos(gamma)) * v[1] +
              (sin(phi) * sin(theta) * cos(gamma) - cos(phi) * sin(gamma)) * v[2]
  result[2] = -sin(theta) * v[0] +
              cos(theta) * sin(gamma) * v[1] +
              cos(theta) * cos(gamma) * v[2]

proc reflect*(v, n: Vec3): Vec3 =
  result = v - 2 * v.dot(n) * n

proc refract*(uv, n: Vec3, etapOverEta: float): Vec3 =
  let cosTheta = min(dot(-uv, n), 1.0)
  let r_out_perp = etapOverEta * (uv + cosTheta * n)
  let r_out_parallel = -sqrt(abs(1.0 - r_out_perp.length_squared())) * n
  result = r_out_perp + r_out_parallel

proc randomVec*(min = 0.0, max = 1.0): Vec3 =
  ## generate a random 3 vector
  result = Vec3(arr: [rand(min .. max), rand(min .. max), rand(min .. max)])

proc randomInUnitSphere*(): Vec3 =
  while true:
    let p = randomVec(-1, 1)
    if p.length_squared >= 1: continue
    return p

proc randomUnitVector*(): Vec3 =
  result = unitVector(randomInUnitSphere())

proc randomInHemisphere*(normal: Vec3): Vec3 =
  let inUnitSphere = randomInUnitSphere()
  if inUnitSphere.dot(normal) > 0.0: # same hemisphere as the normal
    result = inUnitSphere
  else:
    result = -inUnitSphere

proc randomInUnitDisk*(): Vec3 =
  while true:
    let p = vec3(rand(-1.0 .. 1.0), rand(-1.0 .. 1.0), 0)
    if p.length_squared() >= 1.0: continue
    return p

template borrowOps(typ: typed): untyped =
  proc `[]=`*(v: var typ; ix: int; c: float): void {.inline, borrow.}
  proc `[]`*(v: typ; ix: int): float {.inline, borrow.}
  # following cannot be borrowd
  # proc `[]`*(v: var Color; ix: int): var float {.inline, borrow.}
  proc dot*(v, u: typ): float {.borrow.}
  proc cross*(v, u: typ): typ {.borrow.}
  proc length_squared*(v: typ): float {.inline, borrow.}
  proc length*(v: typ): float {.inline, borrow.}
  proc normalize*(v: typ): typ {.inline, borrow.}
  proc `+`*(v: typ): typ {.inline, borrow.}
  proc `-`*(v: typ): typ {.inline, borrow.}
  proc `+=`*(v: var typ, u: typ) {.inline, borrow.}
  proc `-=`*(v: var typ, u: typ) {.inline, borrow.}

borrowOps(Color)
borrowOps(Point)

template makeMathBorrow(typ, op: typed): untyped {.dirty.} =
  proc `op`*(v, u: typ): typ {.inline, borrow.}
  proc `op`*(v: typ; val: float): typ {.inline, borrow.}
  proc `op`*(val: float; v: typ): typ {.inline, borrow.}
makeMathBorrow(Color, `+`)
makeMathBorrow(Color, `-`)
makeMathBorrow(Color, `/`)
makeMathBorrow(Color, `*`)
makeMathBorrow(Point, `+`)
makeMathBorrow(Point, `-`)
makeMathBorrow(Point, `/`)
makeMathBorrow(Point, `*`)

proc initRay*(origin: Point, direction: Vec3): Ray =
  result = Ray(orig: origin, dir: direction)

proc at*(r: Ray, t: float): Point = result = (r.orig.Vec3 + t * r.dir).Point
