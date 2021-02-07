import basetypes, math

type
  AABB* = object
    minimum*: Point
    maximum*: Point

proc initAabb*(a, b: Point): AABB =
  result = AABB(minimum: a, maximum: b)

proc hit*(b: AABB, r: Ray, t_min, t_max: float): bool =
  for a in 0 ..< 3:
    let invD = 1.0 / r.dir[a]
    var t_0 = (b.minimum[a] - r.orig[a]) * invD
    var t_1 = (b.maximum[a] - r.orig[a]) * invD
    if invD < 0.0:
      swap(t_0, t_1)
    let t_min = if t0 > t_min: t_0 else: t_min
    let t_max = if t1 < t_max: t_1 else: t_max
    if t_max <= t_min:
      return false
  result = true

proc surroundingBox*(box0, box1: AABB): AABB =
  let small = point(min(box0.minimum.x, box1.minimum.x),
                    min(box0.minimum.y, box1.minimum.y),
                    min(box0.minimum.z, box1.minimum.z))
  let big = point(max(box0.maximum.x, box1.maximum.x),
                  max(box0.maximum.y, box1.maximum.y),
                  max(box0.maximum.z, box1.maximum.z))
  result = initAabb(small, big)
