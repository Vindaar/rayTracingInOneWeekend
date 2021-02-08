import strformat, os, terminal, macros, math, random, times

import basetypes, hittables, camera

import arraymancer

import sdl2 except Color, Point


proc rayColor*(r: Ray, world: HittablesList, depth: int): Color =
  var rec: HitRecord

  if depth <= 0:
    return color(0, 0, 0)

  if world.hit(r, 0.001, Inf, rec):
    var scattered: Ray
    var attenuation: Color
    if rec.mat.scatter(r, rec, attenuation, scattered):
      return attenuation * rayColor(scattered, world, depth - 1)
    return color(0, 0, 0)

  let unitDirection = unitVector(r.dir)
  let t = 0.5 * (unitDirection.y + 1.0)
  result = (1.0 - t) * color(1.0, 1.0, 1.0) + t * color(0.5, 0.7, 1.0)

proc writeColor*(f: File, color: Color, samplesPerPixel: int) =
  let scale = 1.0 / samplesPerPixel.float
  let
    r = sqrt(color.r * scale)
    g = sqrt(color.g * scale)
    b = sqrt(color.b * scale)
  f.write(&"{(256 * r.clamp(0, 0.999)).int} {(256 * g.clamp(0, 0.999)).int} {(256 * b.clamp(0, 0.999)).int}\n")

proc writeColor*(f: File, color: Color) =
  f.write(&"{color.r} {color.g} {color.b}\n")

proc toColor(u: uint32 | int32): Color =
  result[0] = ((u and 0xFF0000) shr 16).float / 256.0
  result[1] = ((u and 0x00FF00) shr 8).float / 256.0
  result[2] = (u and 0x0000FF).float / 256.0

proc toUInt32(c: ColorU8): uint32
proc toColorU8(c: Color, samplesPerPixel: int = 1): ColorU8 {.inline.} =
  let scale = 1.0 / samplesPerPixel.float
  let
    r = 256 * clamp(c.r * scale, 0, 0.999)
    g = 256 * clamp(c.g * scale, 0, 0.999)
    b = 256 * clamp(c.b * scale, 0, 0.999)
  result = (r: r.uint8, g: g.uint8, b: b.uint8)
  #echo c.repr, " and result ", result, " and asuint32 ", result.toUint32, " and back ", result.toUint32.toColor.repr

proc gammaCorrect*(c: Color): Color =
  result[0] = sqrt(c.r)
  result[1] = sqrt(c.g)
  result[2] = sqrt(c.b)

proc toUInt32(c: ColorU8): uint32 =
  result = (#255 shl 24 or
            c.r.int shl 16 or
            c.g.int shl 8 or
            c.b.int).uint32

proc render*(img: Image, f: string, world: var HittablesList,
             camera: Camera,
             samplesPerPixel, maxDepth: int) =
  ## Write a ppm file to `f`
  var f = open(f, fmWrite)
  f.write(&"P3\n{img.width} {img.height}\n255\n")
  for j in countdown(img.height - 1, 0):
    stderr.write(&"\rScanlines remaining: {j}")
    for i in 0 ..< img.width:
      var pixelColor = color(0, 0, 0)
      for s in 0 ..< samplesPerPixel:
        let u = (i.float + rand(1.0)) / (img.width - 1).float
        let v = (j.float + rand(1.0)) / (img.height - 1).float
        let r = camera.getRay(u, v)
        pixelColor += rayColor(r, world, maxDepth)
      f.writeColor(pixelColor, samplesPerPixel)
  f.close()

proc renderMC*(img: Image, f: string, world: var HittablesList,
               camera: Camera,
               samplesPerPixel, maxDepth: int) =
  ## Write a ppm file to `f`
  var f = open(f, fmWrite)
  f.write(&"P3\n{img.width} {img.height}\n255\n")
  var numRays = samplesPerPixel * img.width * img.height
  var buf = newTensor[Color](@[img.height, img.width])
  var counts = newTensor[int](@[img.height, img.width])
  var idx = 0
  while idx < numRays:
    let x = rand((img.width).float)
    let y = rand((img.height).float)
    let r = camera.getRay(x / (img.width - 1).float,
                          y / (img.height - 1).float)
    let color = rayColor(r, world, maxDepth)
    buf[y.int, x.int] = buf[y.int, x.int] + color
    counts[y.int, x.int] = counts[y.int, x.int] + 1
    inc idx
    if idx mod (img.width * img.height) == 0:
      let remain = numRays - idx
      stderr.write(&"\rRays remaining: {remain}")
  for j in countdown(img.height - 1, 0):
    stderr.write(&"\rScanlines remaining: {j}")
    for i in 0 ..< img.width:
      f.writeColor(buf[j, i], counts[j, i])
  f.close()

proc renderSdlFrame(buf: var Tensor[uint32], counts: var Tensor[int],
                    window: SurfacePtr, numRays, width, height: int,
                    camera: Camera, world: HittablesList, maxDepth: int) =
  var idx = 0
  while idx < numRays:
    let x = rand((width).float)
    let y = rand((height).float)
    #if x.int >= window.w: continue
    #if y.int >= window.h: continue
    let r = camera.getRay(x / (width - 1).float,
                          y / (height - 1).float)
    let color = rayColor(r, world, maxDepth)
    let yIdx = height - y.int - 1
    let xIdx = x.int
    counts[yIdx, xIdx] = counts[yIdx, xIdx] + 1
    let curColor = buf[yIdx, xIdx].toColor
    let delta = (color.gammaCorrect - curColor) / counts[yIdx, xIdx].float
    let newColor = curColor + delta
    let cu8 = toColorU8(newColor)
    let sdlColor = sdl2.mapRGB(window.format, cu8.r.byte, cu8.g.byte, cu8.b.byte)
    buf[yIdx, xIdx] = sdlColor
    inc idx

proc copyBuf(bufT: Tensor[uint32], window: SurfacePtr) =
  var surf = fromBuffer[uint32](window.pixels, @[window.h.int, window.w.int])
  if surf.shape == bufT.shape:
    surf.copyFrom(bufT)
  else:
    echo "Buffer and window size don't match, slow copy!"
    ## have to copy manually, because surf smaller than bufT
    for y in 0 ..< surf.shape[0]:
      for x in 0 ..< surf.shape[1]:
        surf[y, x] = bufT[y, x]

proc renderSdl*(img: Image, world: var HittablesList,
                camera: Camera,
                samplesPerPixel, maxDepth: int) =
  discard sdl2.init(INIT_EVERYTHING)
  var screen = sdl2.createWindow("Ray tracing".cstring,
                                 SDL_WINDOWPOS_UNDEFINED,
                                 SDL_WINDOWPOS_UNDEFINED,
                                 img.width.cint, img.height.cint,
                                 SDL_WINDOW_OPENGL);
  var renderer = sdl2.createRenderer(screen, -1, 1)
  if screen.isNil:
    quit($sdl2.getError())

  var mouseModeIsRelative = false
  var movementIsFree = false

  var quit = false
  var event = sdl2.defaultEvent

  var window = sdl2.getsurface(screen)

  # store original position from and to we look to reset using `backspace`
  let origLookFrom = camera.lookFrom
  let origLookAt = camera.lookAt

  template resetBufs(bufT, counts: untyped): untyped {.dirty.} =
    bufT.setZero()
    when not defined(threads):
      counts.setZero()

  var bufT = newTensor[uint32](@[img.height, img.width])
  when not defined(threads):
    var counts = newTensor[int](@[img.height, img.width])

  let width = img.width
  let height = img.height
  while not quit:
    while pollEvent(event):
      case event.kind
      of QuitEvent:
        quit = true
      of KeyDown:
        const dist = 1.0
        case event.key.keysym.scancode
        of SDL_SCANCODE_LEFT, SDL_SCANCODE_RIGHT, SDL_SCANCODE_A, SDL_SCANCODE_D:
          let cL = (camera.lookFrom - camera.lookAt).Vec3
          let zAx = vec3(0, 1, 0)
          let newFrom = cL.cross(zAx).normalize().Point
          var nCL: Point
          var nCA: Point
          if event.key.keysym.scancode in {SDL_SCANCODE_LEFT, SDL_SCANCODE_A}:
            nCL = camera.lookFrom + newFrom
            nCA = camera.lookAt + newFrom
          else:
            nCL = camera.lookFrom - newFrom
            nCA = camera.lookAt - newFrom
          camera.updateLookFromAt(nCL, nCA)
          resetBufs(bufT, counts)
        of SDL_SCANCODE_UP, SDL_SCANCODE_DOWN, SDL_SCANCODE_W, SDL_SCANCODE_S:
          var cL = (camera.lookFrom - camera.lookAt).Vec3
          if not movementIsFree:
            cL[1] = 0.0
          cL = cL.normalize()
          var nCL: Point
          var nCA: Point
          if event.key.keysym.scancode in {SDL_SCANCODE_UP, SDL_SCANCODE_W}:
            nCL = camera.lookFrom - cL.Point
            nCA = camera.lookAt - cL.Point
          else:
            nCL = camera.lookFrom + cL.Point
            nCA = camera.lookAt + cL.Point

          camera.updateLookFromAt(nCL, nCA)
          resetBufs(bufT, counts)
        of SDL_SCANCODE_BACKSPACE:
          echo "Resetting view!"
          camera.updateLookFromAt(origLookFrom, origLookAt)
          resetBufs(bufT, counts)
        of SDL_SCANCODE_ESCAPE:
          ## deactivate relative mouse motion
          if mouseModeIsRelative:
            discard setRelativeMouseMode(False32)
            mouseModeIsRelative = false
        of SDL_SCANCODE_N:
          ## activate free movement (n for noclip ;))
          movementIsFree = not movementIsFree
        else: discard
      of MousebuttonDown:
        ## activate relative mouse motion
        if not mouseModeIsRelative:
          discard setRelativeMouseMode(True32)
          mouseModeIsRelative = true
      of WindowEvent:
        freeSurface(window)
        window = sdl2.getsurface(screen)
      of MouseMotion:
        ## for now just take a small fraction of movement as basis
        let yaw = -event.motion.xrel.float / 1000.0
        var pitch = -event.motion.yrel.float / 1000.0
        var newLook: Vec3
        if not movementIsFree:
          ## TODO: fix me
          newLook = (camera.lookAt - camera.lookFrom).Vec3.rotateAround(camera.lookAt, yaw, 0, pitch)
          camera.updateLookFrom(Point(newLook))
        else:
          camera.updateYawPitchRoll(camera.lookFrom, camera.yaw + yaw, camera.pitch + pitch, 0.0)
        resetBufs(bufT, counts)

      else: echo event.kind
    discard lockSurface(window)

    ## rendering of this frame
    when not defined(threads):
      let width = img.width
      let height = img.height
      let numRays = 10_000 #samplesPerPixel * width * height
      renderSdlFrame(bufT, counts, window, numRays, width, height, camera, world, maxDepth)
      copyBuf(bufT, window)
    else:
      echo "unequal"
      for y in 0 ..< surf.shape[0]:
        for x in 0 ..< surf.shape[1]:
          surf[y, x] = bufT[y, x]
    let t1 = cpuTime()
    echo "copying took ", t1 - t0, " s"
    unlockSurface(window)
    #sdl2.clear(arg.renderer)
    sdl2.present(renderer)
  sdl2.quit()

proc sceneRedBlue(): HittablesList =
  result = initHittables(0)
  let R = cos(Pi/4.0)

  #world.add Sphere(center: point(0, 0, -1), radius: 0.5)
  #world.add Sphere(center: point(0, -100.5, -1), radius: 100)

  let matLeft = initMaterial(initLambertian(color(0,0,1)))
  let matRight = initMaterial(initLambertian(color(1,0,0)))

  result.add Sphere(center: point(-R, 0, -1), radius: R, mat: matLeft)
  result.add Sphere(center: point(R, 0, -1), radius: R, mat: matRight)

proc mixOfSpheres(): HittablesList =
  result = initHittables(0)
  let matGround = initMaterial(initLambertian(color(0.8, 0.8, 0.0)))
  let matCenter = initMaterial(initLambertian(color(0.1, 0.2, 0.5)))
  # let matLeft = initMaterial(initMetal(color(0.8, 0.8, 0.8), 0.3))
  let matLeft = initMaterial(initDielectric(1.5))
  let matRight = initMaterial(initMetal(color(0.8, 0.6, 0.2), 1.0))

  result.add Sphere(center: point(0, -100.5, -1), radius: 100, mat: matGround)
  result.add Sphere(center: point(0, 0, -1), radius: 0.5, mat: matCenter)
  result.add Sphere(center: point(-1.0, 0, -1), radius: 0.5, mat: matLeft)
  result.add Sphere(center: point(-1.0, 0, -1), radius: -0.4, mat: matLeft)
  result.add Sphere(center: point(1.0, 0, -1), radius: 0.5, mat: matRight)

proc randomScene(): HittablesList =
  result = initHittables(0)

  let groundMaterial = initMaterial(initLambertian(color(0.5, 0.5, 0.5)))
  result.add Sphere(center: point(0, -1000, 0), radius: 1000, mat: groundMaterial)

  var smallSpheres = initHittables(0)
  for a in -11 ..< 11:
    for b in -11 ..< 11:
      let chooseMat = rand(1.0)
      var center = point(a.float + 0.9 * rand(1.0), 0.2, b.float + 0.9 * rand(1.0))

      if (center - point(4, 0.2, 0)).length() > 0.9:
        var sphereMaterial: Material
        if chooseMat < 0.8:
          # diffuse
          let albedo = randomVec().Color * randomVec().Color
          sphereMaterial = initMaterial(initLambertian(albedo))
          smallSpheres.add Sphere(center: center, radius: 0.2, mat: sphereMaterial)
        elif chooseMat < 0.95:
          # metal
          let albedo = randomVec(0.5, 1.0).Color
          let fuzz = rand(0.0 .. 0.5)
          sphereMaterial = initMaterial(initMetal(albedo, fuzz))
          smallSpheres.add Sphere(center: center, radius: 0.2, mat: sphereMaterial)
        else:
          # glass
          sphereMaterial = initMaterial(initDielectric(1.5))
          smallSpheres.add Sphere(center: center, radius: 0.2, mat: sphereMaterial)

  result.add initBvhNode(smallSpheres)

  let mat1 = initMaterial(initDielectric(1.5))
  result.add Sphere(center: point(0, 1, 0), radius: 1.0, mat: mat1)

  let mat2 = initMaterial(initLambertian(color(0.4, 0.2, 0.1)))
  result.add Sphere(center: point(-4, 1, 0), radius: 1.0, mat: mat2)

  let mat3 = initMaterial(initMetal(color(0.7, 0.6, 0.5), 0.0))
  result.add Sphere(center: point(4, 1, 0), radius: 1.0, mat: mat3)

proc sceneCast(): HittablesList =
  result = initHittables(0)

  let groundMaterial = initMaterial(initLambertian(color(0.2, 0.7, 0.2)))
  let EarthR = 6_371_000.0
  result.add Sphere(center: point(0, -EarthR, 0), radius: EarthR, mat: groundMaterial)

  #let concrete = initMaterial(initLambertian(color(0.5, 0.5, 0.5)))
  #let airportWall = initXyRect(-10, 0, 0, 10, 10, mat = concrete)
  #result.add airportWall

  let strMetal = initMaterial(initMetal(color(0.6, 0.6, 0.6), 0.2))
  let telBox = initBox(point(-2, 1.5, 4), point(0, 1.75, 5.5), strMetal)
  result.add telBox

  let concreteMaterial = initMaterial(initLambertian(color(0.6, 0.6, 0.6)))
  let controlRoom = initBox(point(1, 0.0, 0.0), point(4, 2.2, 2.2), concreteMaterial)
  result.add controlRoom

  let floorMaterial = initMaterial(initLambertian(color(0.7, 0.7, 0.7)))
  let upperFloor = initBox(point(-4, 0.0, -100), point(20, 2.0, 0), floorMaterial)
  result.add upperFloor

  let glass = initMaterial(initDielectric(1.5))
  let railing = initBox(point(-4, 2.0, -0.1), point(10, 2.6, 0), floorMaterial)
  result.add railing

  let SunR = 695_700_000.0
  let AU = 1.496e11
  let pos = point(AU / 10.0, AU / 10.0, AU).normalize * AU
  echo pos.repr
  let sunMat = initMaterial(initLambertian(color(1.0, 1.0, 0.0)))
  result.add Sphere(center: pos, radius: SunR, mat: sunMat)

  #result.add Disk(distance: 3.3, radius: 10.0, mat: concreteMaterial)

  for x in result:
    echo x.repr

proc sceneDisk(): HittablesList =
  result = initHittables(0)
  let groundMaterial = initMaterial(initLambertian(color(0.2, 0.7, 0.2)))
  result.add Disk(distance: 1.5, radius: 1.5, mat: groundMaterial)

proc main =
  # Image
  const ratio = 3.0 / 2.0 #16.0 / 9.0
  const width = 1200
  let img = Image(width: width, height: (width / ratio).int)
  let samplesPerPixel = 500
  let maxDepth = 50

  # World
  var world = randomScene() #sceneCast() #randomScene()

  # Camera
  #let lookFrom = point(-1, 5.0, -4) #point(-0.5, 3, -0.5)#point(3,3,2)
  #let lookAt = point(1.0, 3.0, 2.0) #point(0, 1.5, 2.5)#point(0,0,-1)
  let lookFrom = point(3,3,2)
  let lookAt = point(0,0,-1)
  let vup = vec3(0,1,0)
  let distToFocus = 10.0 #(lookFrom - lookAt).length()
  let aperture = 0.0
  let camera = initCamera(lookFrom, lookAt, vup, vfov = 90,
                          aspectRatio = ratio,
                          aperture = aperture, focusDist = distToFocus)

  # Rand seed
  randomize(0xE7)

  # Render (image)
  #img.renderMC("/tmp/test_cast.ppm", world, camera, samplesPerPixel, maxDepth)
  img.renderSdl(world, camera, samplesPerPixel, maxDepth)

when isMainModule:
  main()
