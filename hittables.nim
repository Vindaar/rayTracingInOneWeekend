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
