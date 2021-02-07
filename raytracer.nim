import strformat, os, terminal, macros, math, random

import basetypes, hittables, camera, hitrecord

proc rayColor*(r: Ray, world: var HittablesList, depth: int): Color =
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

proc main =
  # Image
  const ratio = 3.0 / 2.0 #16.0 / 9.0
  const width = 1200
  let img = Image(width: width, height: (width / ratio).int)
  let samplesPerPixel = 500
  let maxDepth = 50

  # World
  var world = randomScene()

  # Camera
  let lookFrom = point(13, 2, 3)#point(3,3,2)
  let lookAt = point(0, 0, 0)#point(0,0,-1)
  let vup = vec3(0,1,0)
  let distToFocus = 10.0 #(lookFrom - lookAt).length()
  let aperture = 0.1
  let camera = initCamera(lookFrom, lookAt, vup, vfov = 20,
                          aspectRatio = ratio,
                          aperture = aperture, focusDist = distToFocus)

  # Rand seed
  randomize(0xE7)

  # Render
  img.render("/tmp/test_small.ppm", world, camera, samplesPerPixel, maxDepth)

when isMainModule:
  main()
