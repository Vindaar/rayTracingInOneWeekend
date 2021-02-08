import basetypes, math

type
  Camera* = ref object
    origin: Point
    lowerLeftCorner: Point
    horizontal: Vec3
    vertical: Vec3
    lensRadius: float
    u, v, w: Vec3
    lookFrom*: Point
    lookAt*: Point
    vup: Vec3
    vfov: float
    aspectRatio: float
    aperture: float
    focusDist: float
    yaw*, pitch*, roll*: float

proc initCamera*(lookFrom, lookAt: Point, vup: Vec3, vfov, aspectRatio: float,
                 aperture: float = 0.0,
                 focusDist: float): Camera =
  result = new Camera
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


  ## store args
  result.lookFrom = lookFrom
  result.lookAt = lookAt
  result.vup = vup
  result.vfov = vfov
  result.aspectRatio = aspectRatio
  result.aperture = aperture
  result.focusDist = focusDist
  ## compute pitch and yaw for this
  result.pitch = -arcsin(result.w[1])
  result.yaw = -arctan(result.w[2] / result.w[0])

proc initCamera*(pos: Point, yaw, pitch, roll: float, vup: Vec3, vfov, aspectRatio: float,
                 aperture: float = 0.0,
                 focusDist: float): Camera =
  result = new Camera
  let theta = degToRad(vfov)
  let h = tan(theta/2.0)
  let viewHeight = 2.0 * h
  let viewWidth = aspectRatio * viewHeight

  let lookFrom = pos
  let lookAt = lookFrom.Vec3 + vec3(cos(-yaw) * cos(pitch), sin(pitch), sin(-yaw) * cos(pitch))

  result.w = unitVector((lookFrom.Vec3 - lookAt).Vec3)
  result.u = unitVector(cross(vup, result.w))
  result.v = cross(result.w, result.u)

  result.origin = lookFrom
  result.horizontal = focusDist * viewWidth * result.u
  result.vertical = focusDist * viewHeight * result.v

  result.lowerLeftCorner = result.origin - result.horizontal.Point / 2.0 -
    result.vertical.Point / 2.0 - focusDist * result.w.Point

  result.lensRadius = aperture / 2.0


  ## store args
  result.lookFrom = lookFrom
  result.lookAt = lookAt.Point
  result.vup = vup
  result.vfov = vfov
  result.aspectRatio = aspectRatio
  result.aperture = aperture
  result.focusDist = focusDist
  result.yaw = yaw
  result.pitch = pitch
  result.roll = roll

proc initCamera*(lookFrom: Point, dir: Vec3, focusIn: float, vup: Vec3, vfov, aspectRatio: float,
                 aperture: float = 0.0,
                 focusDist: float): Camera =
  let lookAt = (dir.unitVector() * focusIn).Point
  result = initCamera(lookFrom, lookAt, vup, vfov,
                      aspectRatio, aperture, focusDist)

proc copyFields(c, mc: Camera) =
  c.origin = mc.origin
  c.lowerLeftCorner = mc.lowerLeftCorner
  c.horizontal = mc.horizontal
  c.vertical = mc.vertical
  c.lensRadius = mc.lensRadius
  c.u = mc.u
  c.v = mc.v
  c.w = mc.w
  c.lookFrom = mc.lookFrom
  c.lookAt = mc.lookAt
  c.vup = mc.vup
  c.vfov = mc.vfov
  c.aspectRatio = mc.aspectRatio
  c.aperture = mc.aperture
  c.focusDist = mc.focusDist
  c.yaw = mc.yaw
  c.pitch = mc.pitch
  c.roll = mc.roll

proc updateLookFrom*(c: Camera, lookFrom: Point) =
  var mc = initCamera(lookFrom, c.lookAt, c.vup, c.vfov, c.aspectRatio, c.aperture, c.focusDist)
  c.copyFields(mc)

proc updateLookFromAt*(c: Camera, lookFrom, lookAt: Point) =
  var mc = initCamera(lookFrom, lookAt, c.vup, c.vfov, c.aspectRatio, c.aperture, c.focusDist)
  c.copyFields(mc)

proc updateLookFromFront*(c: Camera, lookFrom: Point) =
  var mc = initCamera(lookFrom, lookFrom.normalize().Vec3, (c.lookFrom - c.lookAt).length(), c.vup, c.vfov, c.aspectRatio, c.aperture, c.focusDist)
  c.copyFields(mc)

proc updateLookFromAtFront*(c: Camera, lookFrom, lookAt: Point) =
  var mc = initCamera(lookFrom, lookAt.normalize().Vec3, (c.lookFrom - c.lookAt).length(), c.vup, c.vfov, c.aspectRatio, c.aperture, c.focusDist)
  c.copyFields(mc)

proc updateYawPitchRoll*(c: Camera, lookFrom: Point, yaw, pitch, roll: float) =
  let yaw = yaw mod (2 * PI)
  let pitch = clamp(pitch, -PI/2.0, PI/2.0)
  var mc = initCamera(lookFrom, yaw, pitch, roll, c.vup, c.vfov, c.aspectRatio, c.aperture, c.focusDist)
  c.copyFields(mc)

proc getRay*(c: Camera, s, t: float): Ray =
  let rd = c.lensRadius * randomInUnitDisk()
  let offset = c.u * rd.x + c.v * rd.y
  result = initRay(c.origin + offset.Point,
                   c.lowerLeftCorner.Vec3 + s * c.horizontal +
                     t * c.vertical - c.origin.Vec3 - offset)
