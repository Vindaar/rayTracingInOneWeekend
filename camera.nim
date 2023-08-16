import basetypes, math

type
  Camera* = ref object
    center: Point
    width: int
    pixel00_loc: Point
    defocus_disk_u: Vec3d
    defocus_disk_v: Vec3d
    pixel_delta_u: Vec3d
    pixel_delta_v: Vec3d
    defocusAngle: float

    u, v, w: Vec3d
    lookFrom*: Point
    lookAt*: Point
    vup: Vec3d
    vfov: float
    aspectRatio: float
    focusDist: float
    yaw*, pitch*, roll*: float

proc initCamera*(lookFrom, lookAt: Point, vup: Vec3d, vfov, aspectRatio: float,
                 width: int,
                 defocusAngle: float,
                 focusDist: float,
                 yaw = NaN, pitch = NaN, roll = NaN): Camera =
  result = new Camera
  let height = width.float / aspect_ratio

  var lookAt = lookAt
  let hasAngles = classify(yaw) != fcNaN and classify(pitch) != fcNaN
  if hasAngles:
    lookAt = Point(lookFrom.Vec3d + vec3(cos(-yaw) * cos(pitch), sin(pitch), sin(-yaw) * cos(pitch)))
    result.yaw = yaw
    result.pitch = pitch
    result.roll = roll
  result.center = lookFrom

  let theta = degToRad(vfov)
  let h = tan(theta/2.0)
  let viewHeight = 2.0 * h * focusDist
  echo viewHeight
  let viewWidth = aspectRatio * viewHeight
  echo viewWidth

  result.w = unitVector((lookFrom - lookAt).Vec3d)
  result.u = unitVector(cross(vup, result.w))
  result.v = cross(result.w, result.u)

  # Calculate the vectors across the horizontal and down the vertical viewport edges.
  let viewport_u = viewWidth * result.u # Vector across viewport horizontal edge
  let viewport_v = viewHeight * -result.v # Vector down viewport vertical edge

  # Calculate the horizontal and vertical delta vectors to the next pixel.
  result.pixel_delta_u = viewport_u / width.float
  result.pixel_delta_v = viewport_v / height.float

  # Calculate the location of the upper left pixel.
  let viewport_upper_left = result.center.Vec3d - (focus_dist * result.w) - viewport_u / 2.0 - viewport_v / 2.0
  result.pixel00_loc = Point(viewport_upper_left + 0.5 * (result.pixel_delta_u + result.pixel_delta_v))

  # Calculate the camera defocus disk basis vectors.
  let defocus_radius = focus_dist * tan(degToRad(defocus_angle / 2))
  result.defocus_disk_u = result.u * defocus_radius
  result.defocus_disk_v = result.v * defocus_radius

  if not hasAngles:
    ## compute pitch and yaw for this
    result.pitch = -arcsin(result.w[1])
    ## Choose how we calculate the yaw based on the "direction" of `w`.
    if result.w[2] / result.w[0] > 0.0:
      result.yaw = (-arctan(result.w[2] / result.w[0]) + 2 * PI) mod (2 * PI)
    else:
      result.yaw = (-arctan(result.w[2] / result.w[0]) + PI) mod (2 * PI)
    echo "Init camera at yaw = ", result.yaw, ", pitch = ", result.pitch

  ## store args
  result.width = width
  result.lookFrom = lookFrom
  result.lookAt = lookAt
  result.vup = vup
  result.vfov = vfov
  result.aspectRatio = aspectRatio
  result.focusDist = focusDist

proc initCamera*(lookFrom: Point, yaw, pitch, roll: float, vup: Vec3d, vfov, aspectRatio: float,
                 width: int,
                 defocusAngle: float,
                 focusDist: float): Camera =
  result = initCamera(lookFrom, point(0, 0, 0), vup, vfov, aspectRatio, width,
                      defocusAngle, focusDist,
                      yaw = yaw, pitch = pitch, roll = roll)

proc initCamera*(lookFrom: Point, dir: Vec3d, focusIn: float, vup: Vec3d, vfov, aspectRatio: float,
                 width: int,
                 defocusAngle: float,
                 focusDist: float): Camera =
  let lookAt = (dir.unitVector() * focusIn).Point
  result = initCamera(lookFrom, lookAt, vup, vfov,
                      aspectRatio, width, defocusAngle, focusDist)

proc copyFields(c, mc: Camera) =
  c.center = mc.center
  c.pixel00_loc = mc.pixel00_loc
  c.defocus_disk_u = mc.defocus_disk_u
  c.defocus_disk_v = mc.defocus_disk_v
  c.pixel_delta_u = mc.pixel_delta_u
  c.pixel_delta_v = mc.pixel_delta_v
  c.defocusAngle = mc.defocusAngle
  c.width = mc.width
  c.u = mc.u
  c.v = mc.v
  c.w = mc.w
  c.lookFrom = mc.lookFrom
  c.lookAt = mc.lookAt
  c.vup = mc.vup
  c.vfov = mc.vfov
  c.aspectRatio = mc.aspectRatio
  c.focusDist = mc.focusDist
  c.yaw = mc.yaw
  c.pitch = mc.pitch
  c.roll = mc.roll

proc clone*(c: Camera): Camera =
  result = Camera()
  result.copyFields(c)

proc updateLookFrom*(c: Camera, lookFrom: Point) =
  var mc = initCamera(lookFrom, c.lookAt, c.vup, c.vfov, c.aspectRatio, c.width, c.defocusAngle, c.focusDist)
  c.copyFields(mc)

proc updateLookFromAt*(c: Camera, lookFrom, lookAt: Point) =
  var mc = initCamera(lookFrom, lookAt, c.vup, c.vfov, c.aspectRatio, c.width, c.defocusAngle, c.focusDist)
  mc.pitch = c.pitch
  mc.yaw = c.yaw
  c.copyFields(mc)

proc updateLookFromFront*(c: Camera, lookFrom: Point) =
  var mc = initCamera(lookFrom, lookFrom.normalize().Vec3d, (c.lookFrom - c.lookAt).length(), c.vup, c.vfov, c.aspectRatio, c.width, c.defocusAngle, c.focusDist)
  c.copyFields(mc)

proc updateLookFromAtFront*(c: Camera, lookFrom, lookAt: Point) =
  var mc = initCamera(lookFrom, lookAt.normalize().Vec3d, (c.lookFrom - c.lookAt).length(), c.vup, c.vfov, c.aspectRatio, c.width, c.defocusAngle, c.focusDist)
  c.copyFields(mc)

proc updateYawPitchRoll*(c: Camera, lookFrom: Point, yaw, pitch, roll: float) =
  var yaw = yaw
  if yaw < 0.0:
    yaw += 2 * PI
  yaw = yaw mod (2 * PI)
  let pitch = clamp(pitch, -PI/2.0, PI/2.0)
  var mc = initCamera(lookFrom, yaw, pitch, roll, c.vup, c.vfov, c.aspectRatio, c.width, c.defocusAngle, c.focusDist)
  c.copyFields(mc)

import std/random
proc pixelSampleSquare(c: Camera): Vec3d =
  # Returns a random point in the square surrounding a pixel at the origin.
  let px = rand(-0.5 .. 0.5)
  let py = rand(-0.5 .. 0.5)
  result = (px * c.pixel_delta_u) + (py * c.pixel_delta_v)

proc defocusDiskSample(c: Camera): Point =
  # a random point in the camera defocus disk.
  let p = randomInUnitDisk()
  result = c.center + ((p[0] * c.defocus_disk_u) + (p[1] * c.defocus_disk_v))

proc getRay*(c: Camera, i, j: int): Ray =
  # Get a randomly-sampled camera ray for the pixel at location i,j, originating from
  # the camera defocus disk.
  # (Updated from RT in one Weekend V4)
  let pixel_center = c.pixel00_loc + (i.float * c.pixel_delta_u) + (j.float * c.pixel_delta_v)
  let pixel_sample = pixel_center + c.pixel_sample_square()

  let ray_origin = if c.defocusAngle <= 0.0: c.center else: c.defocus_disk_sample()
  let ray_direction = pixel_sample - ray_origin

  result = initRay(ray_origin, ray_direction)
