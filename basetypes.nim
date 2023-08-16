import macros, math, random, strformat

import glm
export glm

type
  Image* = object
    width*, height*: int

  #Vec3* = object
  #  arr*: array[3, float]

  Color* = distinct Vec3d
  Point* = distinct Vec3d

  ColorU8* = tuple[r, g, b: uint8]

  Ray* = object
    orig*: Point
    dir*: Vec3d

#proc vec3*(x, y, z: float): Vec3 = Vec3(arr: [x, y, z])
proc color*(r, g, b: float): Color = Color(vec3(r, g, b))
proc point*(x, y, z: float): Point = Point(vec3(x, y, z))

#proc `[]=`*(v: var Vec3; ix: int; c: float): void {.inline.} = v.arr[ix] = c
#proc `[]`*(v: Vec3; ix: int): float {.inline.} = v.arr[ix]
#proc `[]`*(v: var Vec3; ix: int): var float {.inline.} = v.arr[ix]
#
#proc `$`*(v: Vec3): string =
#  result = &"(Vec3: [{v[0]}, {v[1]}, {v[2]}])"
#
#proc dot*(v, u: Vec3): float =
#  for i in 0 ..< 3:
#    result += v[i] * u[i]
#
#proc cross*(v, u: Vec3): Vec3 =
#  result[0] = v[1] * u[2] - v[2] * u[1]
#  result[1] = v[2] * u[0] - v[0] * u[2]
#  result[2] = v[0] * u[1] - v[1] * u[0]

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

#template `.`*(v: Vec3, field: untyped): untyped =
#  when astToStr(field) == "x":
#    v[0]
#  elif astToStr(field) == "y":
#    v[1]
#  elif astToStr(field) == "z":
#    v[2]
#  else:
#    error("Invalid field " & astToStr(field) & " for Vec3!")
#
#proc `+`*(v: Vec3): Vec3 {.inline.} = v
#proc `-`*(v: Vec3): Vec3 {.inline.} =
#  for ii in 0 ..< 3:
#    result[ii] = -v[ii]
#template makeMath(op: untyped): untyped =
#  proc `op`*(v, u: Vec3): Vec3 {.inline.} =
#    for ii in 0 ..< 3:
#      result[ii] = op(v[ii], u[ii])
#  proc `op`*(v: Vec3; val: float): Vec3 {.inline.} =
#    for ii in 0 ..< 3:
#      result[ii] = op(v[ii], val)
#  proc `op`*(val: float; v: Vec3): Vec3 {.inline.} =
#    for ii in 0 ..< 3:
#      result[ii] = op(val, v[ii])
#makeMath(`+`)
#makeMath(`-`)
#makeMath(`/`)
#makeMath(`*`)
#proc `+=`*(v: var Vec3, u: Vec3) {.inline.} =
#  v = v + u
#proc `-=`*(v: var Vec3, u: Vec3) {.inline.} =
#  v = v - u
#
#proc length_squared*(v: Vec3): float {.inline.} =
#  result = v.x * v.x + v.y * v.y + v.z * v.z
#proc length*(v: Vec3): float {.inline.} =
#  result = sqrt(v.length_squared)
#
#proc normalize*(v: Vec3): Vec3 = v / length(v)

proc unitVector*(v: Vec3d): Vec3d = normalize(v)

proc nearZero*(v: Vec3d): bool =
  ## return true if the vector is close to 0 in all dim.
  const epsilon = 1e-8
  result = abs(v[0]) < epsilon and abs(v[1]) < epsilon and abs(v[2]) < epsilon


### we cannot use the rotation matrix to rotate the camera, because the
### z and y axes would have to be exchanged and -sin(θ) should be sin(θ)
#type
#  ## simple hacky matrix type
#  Matrix = object
#    v1, v2, v3: Vec3
#
#proc rotMatrix*(phi, theta, gamma: float): Matrix =
#  result.v1 = vec3(cos(phi) * cos(theta),
#                   sin(phi) * cos(theta),
#                   -sin(theta),
#                   )
#  result.v2 = vec3((cos(phi) * sin(theta) * sin(gamma) - sin(phi) * cos(gamma)),
#                   (sin(phi) * sin(theta) * sin(gamma) + cos(phi) * cos(gamma)),
#                   cos(theta) * sin(gamma))
#  result.v3 = vec3((cos(phi) * sin(theta) * cos(gamma) + sin(phi) * sin(gamma)),
#                   (sin(phi) * sin(theta) * cos(gamma) - cos(phi) * sin(gamma)),
#                   cos(theta) * cos(gamma))
#
#proc transpose*(m: Matrix): Matrix =
#  result.v1 = vec3(m.v1[0], m.v2[0], m.v3[0])
#  result.v2 = vec3(m.v1[1], m.v2[1], m.v3[1])
#  result.v3 = vec3(m.v1[2], m.v2[2], m.v3[2])
#
#proc dot*(m: Matrix, v: Vec3): Vec3 =
#  result = vec3(m.v1[0] * v[0] + m.v2[0] * v[1] + m.v3[0] * v[2],
#                m.v1[1] * v[0] + m.v2[1] * v[1] + m.v3[1] * v[2],
#                m.v1[2] * v[0] + m.v2[2] * v[1] + m.v3[2] * v[2])
#
#proc rotate*(v: Vec3, phi, theta, gamma: float): Vec3 =
#  ## rotates the vector `v` around the angles phi and theta
#  result[0] = cos(phi) * cos(theta) * v[0] +
#              (cos(phi) * sin(theta) * sin(gamma) - sin(phi) * cos(gamma)) * v[1] +
#              (cos(phi) * sin(theta) * cos(gamma) + sin(phi) * sin(gamma)) * v[2]
#  result[1] = sin(phi) * cos(theta) * v[0] +
#              (sin(phi) * sin(theta) * sin(gamma) + cos(phi) * cos(gamma)) * v[1] +
#              (sin(phi) * sin(theta) * cos(gamma) - cos(phi) * sin(gamma)) * v[2]
#  result[2] = -sin(theta) * v[0] +
#              cos(theta) * sin(gamma) * v[1] +
#              cos(theta) * cos(gamma) * v[2]
#
#proc rotateX*(v: Vec3, phi: float): Vec3 =
#  ## rotates the vector `v` around the x axis
#  result = v
#  result[1] = cos(phi) * v[1] - sin(phi) * v[2]
#  result[2] = sin(phi) * v[1] + cos(phi) * v[2]
#
#proc rotateY*(v: Vec3, theta: float): Vec3 =
#  ## rotates the vector `v` around the y axis
#  result = v
#  result[0] = cos(theta) * v[0] + sin(theta) * v[2]
#  result[2] = -sin(theta) * v[0] + cos(theta) * v[2]
#
#proc rotateZ*(v: Vec3, gamma: float): Vec3 =
#  ## rotates the vector `v` around the z axis
#  result = v
#  result[0] = cos(gamma) * v[0] - sin(gamma) * v[1]
#  result[1] = sin(gamma) * v[0] + cos(gamma) * v[1]


#proc rotate*(p: Point, phi, theta, gamma: float): Point =
#  p.Vec3d.rotate(phi, theta, gamma).Point
#
#proc rotate*(r: Ray, phi, theta, gamma: float): Ray =
#  result = Ray(orig: r.orig.rotate(phi, theta, gamma),
#               dir: r.dir.rotate(phi, theta, gamma))

proc rotateAround*(v: Vec3d, around: Point, phi, theta, gamma: float): Vec3d =
  var v0 = v - around.Vec3d
  var mrot = rotateX(mat4d(), phi)
  mrot = rotateY(mrot, theta)
  mrot = rotateZ(mrot, gamma)
  let vrot = mrot * vec4(v0, 0)
  result = vec3(vrot.x, vrot.y, vrot.z) + around.Vec3d

  #let rot = v0.rotate(phi, theta, gamma)
  #result = rot + around.Vec3d

#proc reflect*(v, n: Vec3): Vec3 =
#  result = v - 2 * v.dot(n) * n
#
#proc refract*(uv, n: Vec3, etapOverEta: float): Vec3 =
#  let cosTheta = min(dot(-uv, n), 1.0)
#  let r_out_perp = etapOverEta * (uv + cosTheta * n)
#  let r_out_parallel = -sqrt(abs(1.0 - r_out_perp.length_squared())) * n
#  result = r_out_perp + r_out_parallel

template length_squared*(v: Vec3d): float = v.length2()
template length_squared*(v: Point): float = v.Vec3d.length2()
template length_squared*(v: Color): float = v.Vec3d.length2()

proc randomVec*(min = 0.0, max = 1.0): Vec3d =
  ## generate a random 3 vector
  result = vec3(rand(min .. max), rand(min .. max), rand(min .. max))

proc randomInUnitSphere*(): Vec3d =
  while true:
    let p = randomVec(-1, 1)
    if p.length_squared >= 1: continue
    return p

proc randomUnitVector*(): Vec3d =
  result = unitVector(randomInUnitSphere())

proc randomInHemisphere*(normal: Vec3d): Vec3d =
  let inUnitSphere = randomInUnitSphere()
  if inUnitSphere.dot(normal) > 0.0: # same hemisphere as the normal
    result = inUnitSphere
  else:
    result = -inUnitSphere

proc randomInUnitDisk*(): Vec3d =
  while true:
    let p = vec3(rand(-1.0 .. 1.0), rand(-1.0 .. 1.0), 0)
    if p.length_squared() >= 1.0: continue
    return p

template borrowOps(typ: typed): untyped =
  proc `[]=`*(v: var typ; ix: int; c: float): void {.inline, borrow.}
  proc `x=`*(v: var typ; c: float): void {.inline, borrow.}
  proc `y=`*(v: var typ; c: float): void {.inline, borrow.}
  proc `z=`*(v: var typ; c: float): void {.inline, borrow.}
  proc `w=`*(v: var typ; c: float): void {.inline, borrow.}
  proc `[]`*(v: typ; ix: int): float {.inline, borrow.}
  # following cannot be borrowd
  # proc `[]`*(v: var Color; ix: int): var float {.inline, borrow.}
  proc dot*(v, u: typ): float {.borrow.}
  proc cross*(v, u: typ): typ {.borrow.}
  #proc length_squared*(v: typ): float {.inline, borrow.}
  proc length*(v: typ): float {.inline, borrow.}
  proc normalize*(v: typ): typ {.inline, borrow.}
  proc `+`*(v: typ): typ {.inline, borrow.}
  proc `-`*(v: typ): typ {.inline, borrow.}
  proc `+=`*(v: var typ, u: typ) {.inline, borrow.}
  proc `-=`*(v: var typ, u: typ) {.inline, borrow.}


borrowOps(Color)
borrowOps(Point)

proc `$`*(v: Point): string =
  result = &"(Point: [{v[0]}, {v[1]}, {v[2]}])"
proc `$`*(v: Color): string =
  result = &"(Color: [{v[0]}, {v[1]}, {v[2]}])"

template makeMathBorrow(typ, op: typed): untyped {.dirty.} =
  proc `op`*(v, u: typ): typ {.inline, borrow.}
  proc `op`*(v: typ; val: float): typ {.inline, borrow.}
  proc `op`*(val: float; v: typ): typ {.inline, borrow.}
makeMathBorrow(Color, `+`)
makeMathBorrow(Color, `-`)
makeMathBorrow(Color, `/`)
makeMathBorrow(Color, `*`)
makeMathBorrow(Point, `+`)
#makeMathBorrow(Point, `-`)
makeMathBorrow(Point, `/`)
makeMathBorrow(Point, `*`)

proc `+`*(p: Point, d: Vec3d): Point =
  result = Point(p.Vec3d + d)

proc `-`*(p1, p2: Point): Vec3d =
  result = p1.Vec3d - p2.Vec3d

proc `+.`*(p1, p2: Point): Point =
  result = Point(p1.Vec3d + p2.Vec3d)

proc `-.`*(p1, p2: Point): Point =
  result = Point(p1.Vec3d - p2.Vec3d)

proc initRay*(origin: Point, direction: Vec3d): Ray =
  result = Ray(orig: origin, dir: direction)

proc at*(r: Ray, t: float): Point = result = (r.orig + t * r.dir)
