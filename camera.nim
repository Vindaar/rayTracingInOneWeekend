import basetypes, math

type
  Camera* = object
    origin: Point
    lowerLeftCorner: Point
    horizontal: Vec3
    vertical: Vec3
    lensRadius: float
    u, v, w: Vec3

proc initCamera*(lookFrom, lookAt: Point, vup: Vec3, vfov, aspectRatio: float,
                 aperture: float = 0.0,
                 focusDist: float): Camera =
  let theta = degToRad(vfov)
  let h = tan(theta/2.0)
  let viewHeight = 2.0 * h
  let viewWidth = aspectRatio * viewHeight

  result.w = unitVector((lookFrom - lookAt).Vec3)
  result.u = unitVector(cross(vup, result.w))
  result.v = cross(result.w, result.u)

  result.origin = lookFrom
  result.horizontal = focusDist * viewWidth * result.u
  result.vertical = focusDist * viewHeight * result.v

  result.lowerLeftCorner = result.origin - result.horizontal.Point / 2.0 -
    result.vertical.Point / 2.0 - focusDist * result.w.Point

  result.lensRadius = aperture / 2.0


proc getRay*(c: Camera, s, t: float): Ray =
  let rd = c.lensRadius * randomInUnitDisk()
  let offset = c.u * rd.x + c.v * rd.y
  result = initRay(c.origin + offset.Point,
                   c.lowerLeftCorner.Vec3 + s * c.horizontal +
                     t * c.vertical - c.origin.Vec3 - offset)
