package elodin.opt

import elodin.global.api.*

import elodin.dom.api.*
import elodin.dom.svg.*

import api.*

val testProgram1: String = """
parameters {
  base = 100
  height = 100
  offset = 100
  scale = 1.2
  n = 10
}

constrain(base > 10)
k = (base * scale)
l = (height * scale)
pts1 = Seq(pt(k, 0), pt(k+l,k), pt(k+l, k+l))
pts2 = Seq(pt(0, k), pt(k, k+l), pt(k+l, k+l))
  .map(p => p.translate(offset,offset))

a = Polygon(pts1)
b = Polygon(pts2)
c = Line(pt(0,1000),pt(500,500))

draw(a, style(translucent, dotted))
draw(b, style(orange))
draw(c)

root = pt(base, height)
sep = 800 / n
tabulate(n, (i =>
  tabulate(n, (j =>
    draw(
      Circle(root.translate(i*sep,j*sep),scale*10),
      style(translucent, dotted)
    )
  ))))
  """.trim.stripMargin

val testProgram2: String = """
parameters {
  w1 = 200
  w2 = 200
  h1 = 200
  h2 = 200
  offx = 80
  offy = 80
  r = 40
}

constrain(offx > 0)
constrain(offy > 0)
constrain(r > 10)
z = 50

// Create two rectangles, offset by some amount
a = Rectangle(pt(z,z), w1, h1)
b = Rectangle(pt(z+offx,z+offy), w2, h2)

// Subtract one from the other
diff = Difference(a, b)
intersectionPoints = query(diff.points, fromAll(a, b))
chamfered = ChamferAll(diff, intersectionPoints, r)

draw(chamfered, style(translucent))
draw(a,style(translucent, dotted))
draw(b,style(dotted))
chamfered
""".trim.stripMargin

val rq1 = """parameters {
  w1 = 200
  h1 = 100
  w2 = 241.9
}

base = pt(100, 100)
r1 = Rectangle(base, w1, h1)

draw(r1, style(translucent(0.25)))

a = query(r1.points, from(r1.topLeft))
b = single(query(r1.points, from(r1.topRight)))
c = query(r1.edges, from(r1.topLeft))
d = query(r1.edges, from(r1.left))

draw(a, color(green))
draw(b, color(blue))
draw(c, color(red))
draw(d, color(purple))
"""

val circleRectExample = """parameters {
  r2 = 40
  loc = 300
  locR = 105
  filletR = 5.0
}
a = Rectangle(pt(100, 100), 300, 150)
b = Circle(pt(400-(75-r2), 175),r2)

d = Difference(a,b)
draw(d)

dps = query(d.points, from(b))
draw(dps, color(blue))
dps.map(d => draw(Circle(d,10),color(red)))

a = Rectangle(pt(200,300), 200, 200)
b = Circle(pt(loc, 400), locR)
diff = Difference(a, b)

ppts = diff.polygons.map(p => query(p.points, from(b)))
ppts.map(p => draw(p,color(blue)))
polys = diff.polygons.map(p => Fillet(p, query(p.points, from(b)), filletR))
polyEs = polys.map(p => draw(Extrude3D(p,100.0),style(translucent)))

// e = Extrude3D(diff, 100.0)
// draw(e, color(black), style(translucent))
"""

val cheese = """parameters {
  w = 150
  h = 70
  fR = 1.5
  x1 = 33
  y1 = 30
  r1 = 17
  x2 = 18
  y2 = 50
  r2 = 15.6
  x3 = 60
  y3 = 36.5
  r3 = 25
  x4 = 89
  y4 = 40
  r4 = 25
}

base = pt(100,100)
a = Rectangle(pt(100, 100), w, h)
c1 = Circle(base.translate(x1,y1),r1)
c2 = Circle(base.translate(x2,y2),r2)
c3 = Circle(base.translate(x3,y3),r3)
c4 = Circle(base.translate(x4,y4),r4)

draw(a, style(dotted))

d = Difference(a, c1)
e = Difference(d, c2)
f = Difference(e, c3)
g = Difference(f, c4)

// badPoly = query(g.positivePolygons, from(c4))
// draw(badPoly, color(red), style(translucent(0.25)))

innerPoly = single(query(g.negativePolygons, contains(from(c1)) and contains(from(c2)) and contains(from(c3)) and contains(from(c4)))) 
pts = innerPoly.points

r = Fillet(innerPoly, pts, fR)
dr = Difference(a, r)

draw(dr)
"""

val circleDiff = """parameters {
  innerR = 40
  outerR = 83.2
  h = 20.0
  d = 100
  filletR = 5 < 35 < 100
}

a = Rectangle(pt(100, 100), 325, 150)

center = pt(a.bottom.at(0.80).x, a.left.midpoint.y)
b = Circle(center, innerR)
c = Circle(center, outerR)

positive = Union(a, c).single()
leftEdge = single(query(positive.edges, from(a.left)))
dt = h / a.left.length
inset = InsetEdge(positive, leftEdge, 250, 0.5-dt, 0.5+dt)

draw(a, style(dotted))
// draw(b, style(dotted))
// draw(c, style(dotted))
// draw(inset.negative, style(dotted))

cut = Difference(inset, b).single()
draw(cut, color(red), style(translucent))
draw(cut.points)

inner = query(cut.points, fromAll(inset.negative, b)) // Query
draw(inner, color(blue))

// Uncomment "rest" of program (3D)
// f = Fillet(cut, inner, filletR)
// e = Extrude3D(f, d)
// draw(e, color(grey), style(translucent))
"""

val arcRectExample = """parameters {
  rectWidth = 300
  center = 341.9
  height = 200
  radius = 80
  ratio = 1.0
  offset = 10
}

e = Circle(pt(center,height), radius)
f = Rectangle(pt(200,height-radius/2), rectWidth, radius)
u = Union(e,f).single

draw(e, style(dotted))
draw(f, style(dotted))

innerX = center+radius+(offset*4.0)
g = Circle(pt(innerX,height), radius * ratio)
draw(g, style(dotted))
v = Difference(u, g)

inner = Circle(pt(innerX-radius, height), 10)
draw(inner, style(dotted))

x = Difference(v, inner).single
draw(x, color(green), style(translucent(0.5)))

// It works if we help it along on intermediary sets
l1 = query(u.edges, from(f))
l2 = query(v.edges, fromAny(l1))
l3 = query(x.edges, fromAny(l2))

// And also, directly
l4 = query(x.edges, from(f))
draw(l4, color(red))

pts = query(x.points, from(g))
draw(pts, color(red))
p1 = query(v.points, from(g))
p2 = query(x.points, fromAny(p1))
draw(p2, color(red)) // Indirectly, too

upper = query(x.edges, from(g))
draw(upper, color(orange))

// Cherry on top: semantics compose.
upts = query(x.points, fromAny(upper))
draw(upts, color(blue))
"""

val insertExtrude = """parameters { 
  w = 150 
  h = 250
  e = 45
  cr = 60
  midW = 20
  midT = 0.4
  offset = 20
  offSize = 30
} 

r = Rectangle(pt(200,200), w, h) 

bs = 0.3 
bw = 0.5
ee = ExtrudeEdge(r, r.bottom, e, bs, bs+bw).single() // from the BOTTOM

ls = 0.6
lh = 0.2
e1 = single(query(ee.edges, from(r.right)))
ie1 = InsetEdge(ee, e1, e, 0.40, 0.55) // from the RIGHT

e2 = single(query(ie1.edges, from(r.left)))
ie2 = InsetEdge(ie1.single(), e2, e, ls, ls+lh) // from the LEFT

cf1 = Chamfer(ie2.single(), query(ie2.points, from(r.topRight)), cr)
draw(cf1, style(dotted))

draw(cf1, style(translucent(0.25)))
draw(cf1.points, color(orange))
cpt = /* QUERY HERE                                                                           */ query(cf1.points, fromAll(ie2.negative.back, r.left))
// cpt = query(cf1.points, all()) // <-- Uncomment to begin #1 
draw(cpt, color(blue))

// Uncomment next block:
cf2 = Chamfer(cf1, cpt, e / 1.5)
draw(cf2, style(translucent(0.25)))
midEdge = /* QUERY HERE                                                                       */ query(cf2.edges, from(r.right) and contains(from(r.bottomRight)))
// midEdge = query(cf2.edges, all()) // <-- Uncomment to begin #2
draw(midEdge, color(blue))


// // Uncomment rest of program to see model! 

// finalSide = ExtrudeEdge(cf2, single(midEdge), midW, 0.0, midT)
// oppSide = finalSide.mirror(finalSide.positive.top)

// p0x = single(query(oppSide.points, fromAll(ie1.negative.back, r.right)))
// p0y = single(query(oppSide.points, from(ie2.negative.front.start)))
// p1x = single(query(oppSide.points, fromAll(ie2.negative.back, ie2.negative.top)))
// p1y = single(query(oppSide.points, from(r.topLeft)))

// p0 = pt(p0x.x,p0y.y)
// p1 = pt(p1x.x,p1y.y)
// draw(p0, p1, color(red))
// draw(Line(p0, p0.translate(200,0)), color(red), style(dotted))
// draw(Line(p1, p1.translate(0,-200)), color(red), style(dotted))

// diff1 = Rectangle(pt(p0x.x+offset, p0y.y), offSize, -offSize)
// diff2 = Rectangle(pt(p1x.x,p1y.y-offset), offSize, -offSize)

// result = Union(finalSide, Difference(Difference(oppSide, diff1), diff2))
// draw(result, color(purple), style(translucent))
"""

val divotBoolean = """parameters {
  e = 300
  r = 48
  ext = 0.8
  filletR = 18
  holeR = 16
  insetHole = 0 < 9 < 20
}

s = Square(200, 200, e)
c0 = Circle(pt(0,0), r)
s0 = Rectangle(c0.center - pt(r, 0), r*2, -r * ext)
divot = Union(c0, s0)
draw(c0, s0, style(translucent(0.25)))

d0 = divot.translate(s.bottom.midpoint - s0.bottom.midpoint)
d1 = d0.rotateDeg(s.center, -90)
d2 = d0.rotateDeg(s.center, -180)
// draw(d0, d1, d2, style(dotted))

srf = Difference(Difference(Difference(s, d0), d1), d2).single()
// srf = Difference(s, Union(Union(d0, d1), d2)).single() 
draw(srf, style(translucent(0.25)))
draw(srf.points, color(orange))


/*                                                                                         */// filletPts = query(srf.points, or(and(from(s0),not(from(c0))),from(s.left)))  // Query 0 
/*                                                                                         */// filletPts = query(srf.points, or(and(from(s0),not(fromAny(c0.edges))),from(s.left))) // Query 1
filletPts = /* QUERY HERE                                                                   */ query(srf.points, and(from(s),not(from(s.right))))  // Query 2
// filletPts = query(srf.points, all()) // <-- Uncomment to begin # 1
draw(filletPts,color(blue))

// draw(srf.edges, color(gray))
redEdges = /*  QUERY HERE                                                                   */ query(srf.edges, from(c0))
// redEdges = query(srf.edges, all()) // <-- Uncomment to begin # 2
draw(redEdges, color(red)) 


srf = Fillet(srf, filletPts, filletR)
// draw(srf, style(translucent))

/*  Optional, if time                                                                                       */// pH0 = query(d0.points, and(from(c0),not(from(s0.bottom))))  // Query 1
pointQ = /* QUERY HERE                                                                                      */ fromAll(c0, s0.left)
p0 = single(query(d0.points, pointQ))  
p1 = single(query(d1.points, pointQ))  
p2 = single(query(d2.points, pointQ))  

ih = insetHole
h0 = Circle(pt(p1.x,p0.y).translate(ih, ih), holeR)
h1 = Circle(pt(p1.x,p2.y).translate(ih,-ih), holeR)

finalSolid = Difference(srf, Union(h0,h1))
draw(finalSolid, color(green), style(translucent))
"""

val rflx = """parameters {
  h1 = 100
  h2 = 70
  ex1 = 200
  c = 40
  dt = 0.5
  rd = 20
}

root = pt(400,400)
r0 = Rectangle(root, -h1, h1)
center = pt(r0.top.midpoint.x, r0.right.midpoint.y)

l1 = Line(r0.topRight, pt(r0.topRight.x, r0.right.at(dt).y))
r1 = ExtrudeCurve(l1, ex1, pt(-2, 1))
r2 = ExtrudeCurve(r1.top, h2)

rflLine = r1.front.translate(rd, rd)

draw(rflLine, color(orange), style(dotted))
draw(r1, style(dotted))
draw(r1.top.start, r1.top.end, color(red))

u1 = Union(Union(r0, r1), r2)
r4 = r0.scale(center, 0.5)
r4c = Chamfer(r4, query(r4.points, from(r0.bottomRight)), c)
// draw(r4c, color(red))

u2 = Difference(u1, r4c)
u3 = u2.mirror(rflLine)
draw(u2, u3, style(translucent(0.25)))

innerEdgeQuery = /* QUERY HERE                                                                 */ and(from(r1.front))
innerEdgeQuery = /* QUERY HERE                                                                 */ and(derivedFrom(r1.front) and not(derivedFrom(r2)))
// innerEdgeQuery = all() // <-- Uncomment to begin

inE1 = query(u2.edges, innerEdgeQuery)
inE2 = query(u3.edges, innerEdgeQuery)
draw(inE1, inE2, color(blue))
"""

val stackedPlates = """parameters{
    plateH = 20
    plateL = 50
    innerR = 6
    outerR = 12
    dH = 9
    thk = 5
    partDeg = 90
    t0 = 0 < 0 < 360 
    t1 = 0 < 0 < 360
}

// Construct rectangle to become plate
plateBase = Rectangle(pt(0,0), plateH, plateL)
plate = Fillet(plateBase, plateBase.points, plateH/2.0 - 0.001)

// Construct centers of rotation
rotCen = single(query(plate.edges, derivedFrom(plateBase.topLeft))).center
arcCen = rotCen + ((plateBase.topLeft - rotCen) * 0.25)
outerRotCen = single(query(plate.edges, derivedFrom(plateBase.bottomLeft))).center
    .rotateDeg(rotCen, t0)

// Make base 2D Shapes
plateTurned = plate.rotateDeg(rotCen, partDeg)
outerRotCenTurned = outerRotCen.rotateDeg(rotCen, partDeg)
baseSmall = Circle(rotCen, innerR)
baseLarge = Circle(arcCen, outerR)

// Extrude Plate Shapes
dZ = dH + thk
lowerPlate = Extrude3D(plate, thk)
midPlate = Extrude3D(Union(Union(baseLarge, plate), plateTurned), thk).translateZ(dZ)
upperPlate = Extrude3D(plateTurned, thk).translateZ(dZ * 2)

// Create slot in bottom pole and extrude into 3d
baseSmallNeg = Rectangle(pt(0,0), innerR*2, innerR/2)
baseSmallNeg = baseSmallNeg.translate(rotCen - baseSmallNeg.center)
baseSmallTrim = Difference(baseSmall, baseSmallNeg)
lowerPole1 = Extrude3D(baseSmall, -dH*2)
lowerPole2 = Extrude3D(baseSmallTrim, -dH*2).translateZ(-dH*2)

// Create and position rotation shafts
innerPole = Extrude3D(Circle(outerRotCen, innerR), dH)
midPole = innerPole.translateZ(thk)
upperPole = innerPole.move(outerRotCen, outerRotCenTurned).translateZ(dZ + thk)
topPole = innerPole.move(outerRotCen, rotCen)
                    .translateZ(2*dZ + thk)
                    .rotateDeg(outerRotCenTurned, -t1)

// draw(plate, plateTurned, style(dotted))
// draw(baseSmall, baseLarge, style(translucent))
draw(lowerPlate.rotateDeg(rotCen, t0), 
     midPlate.rotateDeg(rotCen, t0), 
     upperPlate.rotateDeg(rotCen, t0).rotateDeg(outerRotCenTurned, -t1), 
     lowerPole1, lowerPole2, midPole, upperPole, topPole,
    color(lightblue), stroke(grey))
"""

val jamesBench = """parameters{
    legHeight = 60
    legWidth = 40
    circRadius = 25
    inset = 5
    tableWidth = 80
    tableOffset = 5
    thk = 5
}

// Sketch out the leg of the table
legBase = Rectangle(pt(0,0), legWidth, legHeight)
legSubL = Circle(legBase.center, circRadius)
            .translate((legWidth / 2) + (circRadius - inset), 0)
legSubR = legSubL.rotateDeg(legBase.center, -180)
legSubB = legSubL.rotateDeg(legBase.center, -90)
                 .translate(0, - (legHeight - legWidth) / 2.0)
legSubCen = Circle(legBase.center, thk/2)
                 .translate(0, legHeight / 8)
tableLeg = DifferenceAll(legBase, Seq(legSubL, legSubB, legSubR, legSubCen))
leg3d = Extrude3D(tableLeg, thk)

// Sketch out the top
tableTopBase = Rectangle(pt(0,0), tableWidth, legWidth + (tableOffset * 2))
tableTop = Fillet(tableTopBase, tableTopBase.points, tableOffset)
tableTop3d = Extrude3D(tableTop, thk)

// Position the legs
legSeparation = single(query(tableTop.edges, from(tableTopBase.bottom))).length - thk
leg3dL = leg3d.rotateXDeg(90).rotateZDeg(90)
leg3dR = leg3dL.translate(legSeparation, 0, 0)

// Position the tabletop
tableBasePoint = single(query(tableTop3d.baseEdges, from(tableTopBase.left))).end
tableTargetPoint = single(query(leg3dL.basePoints, from(legBase.topLeft)))
tableTop3d = tableTop3d.move(tableBasePoint, tableTargetPoint).translate(-tableOffset, 0, 0)

// Construct the middle frame using references from the 3D geometry
q = from(legBase.top)
midLpt = single(query(leg3dL.outerEdges, q)).midpoint
midRpt = single(query(leg3dR.baseEdges, q)).midpoint
midW = midRpt.x - midLpt.x
midY = midLpt.y - (thk / 2)
midR = Rectangle(pt(midLpt.x, midY), midW, thk)
mid3d = Extrude3D(midR, legHeight / 4).translate(0, 0, legHeight / 2)

draw(leg3dR, leg3dL, tableTop3d, mid3d, color(beige), stroke(gray), style(translucent))
"""

val test = """parameters {
 holeR = 9.235199999999896
 gap = 4
 lowerOffset = 12.0 < 17 < 25
 thk = 5
 h = 10.0
 pinR = 2.0
 outerOffset = 0.5
 softenR = 0.8
}


// Create main areas and boolean together
circCen = pt(0, lowerOffset + holeR)
c1 = Circle(circCen, holeR)
c2 = Circle(circCen, holeR + thk)

rectH = lowerOffset + thk
r1 = Rectangle(pt(-gap/2, 0), gap, rectH)
r2 = Rectangle(r1.bottomLeft - pt(thk,0), gap + thk*2, rectH)
mainBase = Difference(
    Union(r2, c2), 
    Union(r1, c1)
)

p1 = single(query(mainBase.points, from(r1.bottomRight)))
r3 = Rectangle(p1,thk*2,thk/2)
mainBase = Union(mainBase, r3)

// Compute radius bounds for fillets and apply them
filletPts = query(mainBase.points, from(r2.right))
vEdge = single(query(mainBase.edges, from(r2.right)))
hEdge = single(query(mainBase.edges, from(r3.top)))
fR = min(vEdge.length / 2.0, hEdge.length - softenR - 0.001)
mainBaseHard = Fillet(mainBase.single(), filletPts, fR)
restPts = query(mainBaseHard.points, not(fromAny(filletPts))) // Useful, but maybe replace with ".filletPoints"
mainBase = Fillet(mainBaseHard, restPts, softenR)


// Position the clamp circle relative to the other elements
p0y = single(query(mainBaseHard.edges, from(r1.right))).midpoint.y
p0x = r2.right.midpoint.x
outerPoint = single(c2.edges).start
dx = (outerPoint.x+thk - p0x) / 2
p0 = pt(p0x, max(p0y, r3.topRight.y + dx))
clampCen = p0.translate(dx, 0).translate(outerOffset, 0)

// Construct the clamp geometry and union it together
clampC = Circle(clampCen, dx)
pinC = Circle(clampCen, pinR)
ix = Intersection(mainBase, clampC) // Assert it doesn't hit anything
empty(ix.polygons)
clampRect = Rectangle(single(clampC.points), -thk, outerPoint.y - p0.y)
clampBase = Union(clampRect, clampC).single()
clampBase = Fillet(clampBase, query(clampBase.points, from(clampC, clampRect.left)), holeR)
rectSize = clampRect.topRight.x
r0 = Rectangle(clampRect.topRight, -rectSize, rectSize)
clampArc = Circle(circCen, rectSize)
clampArc = Difference(Intersection(clampArc, r0), Circle(circCen, holeR+thk+outerOffset)).single()

extEdge = single(query(clampArc.edges, from(r0.left)))
clampArc = ExtrudeEdge(clampArc, extEdge, thk)
roundEnds = query(clampArc.points, from(clampArc.positive.top))
clampArc = Fillet(clampArc.single(), roundEnds, softenR)

cu = Union(clampBase, clampArc)
draw(cu.edges)
"""

val intersectLinks = """parameters { 
  x = 100 
  w = 200
  k = 25 < 50 < 200
  l = 25 < 50 < 100
}

r = Rectangle(pt(x,x), w, 100)
sq = r.scale(r.center, 0.5) 
inset = ExtrudeCurve(r.right, k, pt(-1, 0))
  .scale(r.right.at(0.5), 0.4)

outset = ExtrudeCurve(sq.right, l)
  .scale(sq.right.at(0.5), 0.5)

d0 = Difference(r, sq)
d1 = d0.translate(60, 30)

u = Union(d1, d0)

draw(u.edges, style(dotted))
draw(u, style(translucent(0.25)))

pts = query(u.points, from(r) and not(from(sq)) and not(fromAny(r.points)))
draw(pts, color(red))
"""

val bell = """parameters {
  r = 200
  f = 100
}


// Todo: much more elegant if we can revolve around a point
bell = {
  bellThickness = 10
  bellOuter = Circle(pt(0,0), r)
  bellInner = Circle(pt(0,0), r-bellThickness)
  negTop = Square(pt(0,0), -r*2)
  negBottom = Rectangle(negTop.bottomLeft, r*4, r*2)
  negative = Union(Union(negTop, negBottom), bellInner)
  bellArc = Difference(bellOuter, negative).single()
  bellProfile = Fillet(bellArc, query(bellArc.points, from(negBottom)), bellThickness * 0.4)
  axis = Line(negTop.right.end, negTop.right.start) // Todo: when/why is this flip needed
  draw(axis, color(red))
  Revolve(bellProfile, axis)
}

clockCenter = pt(0,r*3)
bellLeft = bell.rotateDeg(clockCenter,  20)
bellRight = bell.rotateDeg(clockCenter, -20)

draw(bellLeft, bellRight, color(gray))
"""

val hex = """parameters { 
  r = 33
  px = 100
  py = 94
  ns = 6
  finalRot = 60.0
  sc = 0.1 < 0.6 < 0.8
} 

base = pt(px, py)
h0 = Polygon(base, ns, r) 

p0 = base.translate(0,-r)
p2 = p0.rotateDeg(base, -120)
p4 = p0.rotateDeg(base, 120)
dx = p2.x - p4.x 
dy = p2.y - p0.y

rectW = 7*dx 
rectH = 4*dy

row0 = Tabulate(10, i => base.translate(i*dx, 0))
rows = Tabulate(5, k => row0.map(c => c.translate((p2 - p0)*k)))
flatRows = rows.flatten.map(center => 
  angle = (center.distance(base) / rectW) * finalRot
  Polygon(center, ns, r * sc).rotateDeg(center, angle)
)

r0 = Rectangle(base + pt(2*dx,0), rectW, rectH)
s1 = DifferenceAll(r0, flatRows)

draw(rows, style(dotted))
draw(Extrude3D(s1, 10), style(translucent(0.25)))
draw(p0, p2, color(red))
draw(r0, style(translucent))
"""

val revolve = """parameters { 
  r = 80
  px = 400
  py = 200
  fillR = 20.0
  d = 0 < 5 < 180
  rr = 0 < 0.785 < 3.14159
  qq = 0 < 1.571 < 3.14159
  tt = 0 < 32.6 < 80.0
}

size = 240 
p0 = pt(px, py)
r0 = Rectangle(p0, size, size)
r1 = Rectangle(p0.translate(size/4,size/4), size/2, size/2)

axis = r0.left.translate(-r, 0).rotateDeg(r0.center, d)

rf0 = Fillet(r0, r0.points, fillR)
rf1 = Fillet(r1, r1.points, fillR)
s0 = Difference(rf0, rf1)

// draw(s0, color(blue))
draw(axis, color(red))

s = Revolve(s0, axis, 0, 90)
draw(s, style(translucent(0.1)))

// s0 = s0.rotate(r0.center,rr)

e = Extrude3d(s0, 40)
      // .translate(0, 0, 5)
      // .translate(0, 0, 5)
      // .rotateZ(rr)

eStart = single(query(s0.edges, from(r0.topLeft))).start
// rotAxis = single(query(e.verticalEdges,from(eStart)))
// draw(rotAxis, color(red))

e1 = e.translate(0, 0, tt)
      .translate(-r0.center.x, -r0.center.y, 0.0)
      .rotateZ(rr)
      .translate(r0.center.x, r0.center.y, 0.0)
draw(e1.basePoints, e1.outerPoints)

// draw(query(e.outerPoints, from(r0.left)), color(red))
// draw(query(s0.points, from(r0.left)), color(red))

// lilEdge = single(query(e.outerEdges, from(r1.top, r1.right)))
// draw(lilEdge.start, lilEdge.end, lilEdge, color(red))

draw(e1, style(translucent(0.1)))

// eStart2 = single(query(s0.edges, from(r0.bottom) and not(fromAny(r0.points))))

// draw(eStart2, color(red))
// rotAxis2 = single(query(e1.baseEdges, from(eStart2)))


crossAxisA = single(query(e1.baseEdges, from(r0.bottomRight))).at(0.5)
crossAxisB = single(query(e1.baseEdges, from(r0.topLeft))).at(0.5)
draw(crossAxisA, crossAxisB, color(orange))

e2 = e1.rotate(crossAxisB, crossAxisA, qq)
draw(e2, color(green), style(translucent(0.1)))


// draw(
//   e.move(rotAxis.start, rotAxis.end)
//    .move(lilEdge.start, lilEdge.end),
//   color(green), style(translucent(0.25)))
"""

val circleMidpoint = """parameters { 
  lng = 100
  r = 100
  deg = 0 < 20 < 360
}

o = pt(200,200)

c1 = Circle(o, r)
p2 = o.translate(lng, 0).rotateDeg(o, deg)
c2 = Circle(p2, r)

u = Union(c1, c2)
ps = query(u.points, fromAll(c1,c2))
l = Line(c1.center, c2.center)

draw(c1, c2, style(dotted))
draw(l, color(blue), style(dotted))
// draw(ps, color(red))

lx = l.end.x - l.start.x 
ly = l.end.y - l.start.y

filterX = p => 
  px = l.start.x - p.x
  py = l.start.y - p.y
  ((lx*py) - (px*ly)) > 0

qs = ps.filter(filterX)
draw(qs, color(red))
"""

val tetris = """parameters {
  w1 = 140
  h1 = 360
  w2 = 240
  h2 = 120
  x2 = 75
  cr = 40
}

base1 = pt(50,50)
base2 = pt(x2,150)

r1 = Rectangle(base1, w1, h1)
r2 = Rectangle(base2, w2, h2)
draw(r1, r2, style(dotted))

u = Union(r1, r2).single()

ps = single(query(u.points, fromAll(r1.right, r2.bottom)))
draw(ps, color(purple)) // Query 1

edge = query(u.edges, from(r1.right) and contains(from(r1.bottomRight))) 
draw(edge, color(orange))  // Query 2

c = Chamfer(u, ps, cr)
draw(c, style(translucent(0.25))) 

// otherEdges = query(c.edges, from(r1)) // Query 3
// // otherEdges = query(c.edges, derivedFrom(r1)) // Uncomment to use derived semantics
// draw(otherEdges, color(green)) 
"""

val chairFillet = """parameters {
  thk = 50
  lng = 300
  a1 = 100
  a2 = 260
  fr = 50
  tx = 0 < 16 < 200
}

o = pt(300, 300)
rr = Rectangle(o, lng, thk)
c = rr.left.midpoint

r0 = rr.translate(-tx, 0)
r1 = Rectangle(o, lng, thk).rotateDeg(c, a1)
r2 = Rectangle(o, lng, thk).rotateDeg(c, a2)
draw(r0, r1, r2, style(dotted))

u = Union(Union(r0,r1),r2).single()

draw(u, style(translucent(0.25)))
draw(u.points, color(orange))
q1 = /*  QUERY HERE (must match on tx = 16 and tx = 116)                                                   */ query(u.points, or(fromAll(r0, r1), fromAll(r0, r2), fromAll(r1,r2)))
// q1 = query(u.points, all()) // <-- Uncomment to begin
draw(q1, color(blue))

// f = Fillet(u, q1, fr)
// draw(f, style(translucent(0.25)))
"""

val twoSquareRotateEx = """parameters {
  r1 = 200
  r2 = 200
  offset = 100
  theta = 0.25
}

a = Square(100, 100, r1)
b = Square(100 + offset, 100 - offset, r2)
draw(a, b, style(dotted))

c = b.rotate(b.topLeft, theta)
u = Union(a, c)
draw(u, style(translucent(0.25)))

draw(u.edges)
success = query(u.edges, from(b.bottom)) // Query
draw(success, color(orange))
"""

val twoSquareDiffEx = """parameters {
  y = 50 < 100 < 250
  widthA = 275
  heightA = 275
  widthB = 275
  heightB = 275
}

KERNEL.point_size(8.0)

x = 200
a = Rectangle(pt(100,x), widthA, heightA)
b = Rectangle(pt(x,y), widthB, heightB)

diff = Difference(a, b)

draw(diff, style(translucent(0.25)))
draw(a, b, style(dotted))

draw(diff.points)
pts = query(diff.points, from(b)) // Query
// Uncomment here to change selection:
// pts = query(diff.points, fromAll(a.bottom, b.left))
// pts = query(diff.points, fromAll(a.bottom, b)) // What happens when we widen A?

draw(pts, color(orange))  

lns = query(diff.edges, from(b))
// draw(lns, color(orange))
"""

val rq5 = """parameters {
    offset = 100
    theta = 15
}

radius = 200
r2 = radius * 0.5

a = Square(100, 100, radius)
op = 100 + offset
b = Rectangle(pt(op, op), radius, r2)

// draw(a, style(translucent))
// draw(b, style(dotted))

rotP = pt(op+r2, op+r2)
c = b.rotateDeg(rotP, theta)
diff = Difference(a, c)

draw(diff, style(translucent))
draw(c, style(dotted))

selPt = query(diff.points, fromAll(a.right, c))
draw(selPt, color(red))

pc = query(c.points, from(b.bottomLeft))
draw(pc, color(blue))
draw(pt(200, 290), color(green))
print(pc) // Move from (203, 325) -> (200, 290)

c
"""

val squareLExample = """parameters {
    r = 200
    offset = 150
    theta = 12.5
}

r2 = r * 0.5
a = Square(100, 100, r)
b = Rectangle(pt(100 + offset, 50), r, r2)
c = Rectangle(b.topRight, -r2, r2)

l = Union(b, c)
    .rotateDeg(b.bottomLeft, theta)

draw(a, l, style(dotted))

d = Difference(a, l)
draw(d, style(translucent(0.25)))

draw(d.edges)

// Try lowering `theta` to make this assertion fail
ln = single(query(d.edges, from(a.right))) 
draw(ln, color(red))
"""

val diamondExample = """parameters{
  x1 = 100
  x2 = 300
  r = 250
}

a = Square(x1,x1,r); e = Rectangle(pt(x2, x2-x1), r, -r)

b = a.scale(a.topLeft, pt(2.0, 0.5))
c = Difference(a, e).single()

d = Union(b, c).single()

draw(a, b, e, style(dotted))
// draw(b, c, style(translucent(0.25)))
draw(d, style(translucent(0.25)))


rights = query(d.edges, from(a.right))
draw(rights, color(red))

botts = query(d.edges, from(a.bottom))
draw(botts, color(blue))

//   A
//  / \
// B   C
//  \ /
//   D

os = query(d.points, from(a.right))
draw(os, color(orange)) // Excludes corner
"""

val gear = """parameters {
 holeR = 10
 tThk = 2.4
 tOut = 4.0
 hexR = 3.0
}

c = Circle(pt(0,0), holeR)
t = Rectangle(pt(-tThk/2, -tOut/2), tThk, tOut)

tooth = t.translate(0, holeR)
cPointQuery = (from(t.right) or from(t.left)) and not(derivedFrom(t.bottom))

tooth = Chamfer(tooth, query(tooth.points, cPointQuery), tThk/3)
tooth = Chamfer(tooth, query(tooth.points, cPointQuery), tThk/4)
tooth = Chamfer(tooth, query(tooth.points, cPointQuery), tThk/4)

nTeeth = 18 
teeth = Tabulate(nTeeth, i => tooth.rotateDeg(pt(0,0), 360 * (i / nTeeth)))
gear = UnionAll(c, teeth)

hex = Polygon(pt(0,0), 6, hexR)
key = Rectangle(hex.edges(1).midpoint, 2, 1).translate(-1, -0.5)
gear = Difference(gear, Union(hex,key))

draw(Extrude3D(gear, 4.0), style(translucent))
"""

val clampPin = """parameters {
 holeR = 10
 gap = 4
 lowerOffset = 12.0 < 17 < 25
 thk = 5
 h = 10.0
 pinR = 2.0
 outerOffset = 0.5
 softenR = 0.8
}


// Create main areas and boolean together
circCen = pt(0, lowerOffset + holeR)
c1 = Circle(circCen, holeR)
c2 = Circle(circCen, holeR + thk)

rectH = lowerOffset + thk
r1 = Rectangle(pt(-gap/2, 0), gap, rectH)
r2 = Rectangle(r1.bottomLeft - pt(thk,0), gap + thk*2, rectH)
mainBase = Difference(
    Union(r2, c2), 
    Union(r1, c1)
)

p1 = single(query(mainBase.points, derivedFrom(r1.bottomRight)))
r3 = Rectangle(p1,thk*2,thk/2)
mainBase = Union(mainBase, r3)

// Compute radius bounds for fillets and apply them
filletPts = query(mainBase.points, from(r2.right))
vEdge = single(query(mainBase.edges, from(r2.right)))
hEdge = single(query(mainBase.edges, from(r3.top)))
fR = min(vEdge.length / 2.0, hEdge.length - softenR - 0.001)
mainBaseHard = Fillet(mainBase.single(), filletPts, fR)
restPts = query(mainBaseHard.points, not(derivedFromAny(filletPts))) // Useful, but maybe replace with ".filletPoints"
mainBase = Fillet(mainBaseHard, restPts, softenR)


// Position the clamp circle relative to the other elements
p0y = single(query(mainBaseHard.edges, derivedFrom(r1.right))).midpoint.y
p0x = r2.right.midpoint.x
outerPoint = single(c2.edges).start
dx = (outerPoint.x+thk - p0x) / 2
p0 = pt(p0x, max(p0y, r3.topRight.y + dx))
clampCen = p0.translate(dx, 0).translate(outerOffset, 0)

// Construct the clamp geometry and union it together
clampC = Circle(clampCen, dx)
pinC = Circle(clampCen, pinR)
ix = Intersection(mainBase, clampC) // Assert it doesn't hit anything
empty(ix.polygons)
clampRect = Rectangle(single(clampC.points), -thk, outerPoint.y - p0.y)
clampBase = Union(clampRect, clampC).single()
clampBase = Fillet(clampBase, query(clampBase.points, from(clampC, clampRect.left)), holeR)
clampRing = {
    rectSize = clampRect.topRight.x
    r0 = Rectangle(clampRect.topRight, -rectSize, rectSize)
    clampArc = Circle(circCen, rectSize)
    clampArc = Difference(Intersection(clampArc, r0), Circle(circCen, holeR+thk+outerOffset)).single()
    
    extEdge = single(query(clampArc.edges, from(r0.left)))
    clampArc = ExtrudeEdge(clampArc, extEdge, thk)
    roundEnds = query(clampArc.points, from(clampArc.positive.top))
    clampArc = Fillet(clampArc.single(), roundEnds, softenR)
}
clamp = Difference(Union(clampBase, clampRing), pinC)
pinNeg = Rectangle(clampRect.bottomRight.translate(0,pinR+1), -dx*2, -dx*2)
clampClear = Difference(clamp, pinNeg)


// Lift everything into 3D
main3 = Extrude3D(mainBase, h)
clamp3L = Extrude3D(clamp, h/4).translateZ(h/8)
clamp3M = Extrude3D(clampClear, h/4).translateZ(3*h/8)
clamp3U = clamp3L.translateZ(h/2)
clampPin = Extrude3D(pinC, h)

// Positioning helper for the bottom pin
lowerPinEdge = single(query(mainBaseHard.edges, from(r2.left)))
positionToEdge = (geo) => 
    geo
     .rotateYDeg(-90)
     .translate(lowerPinEdge.midpoint, h/2)

// Construct the individual lower pin elements
lowerPinProf = Circle(pt(0,0), h/2)
lowerHexProf = Polygon(pt(0,0), 12, h/2 * 1.2)
lowerPin = positionToEdge(Extrude3D(lowerPinProf,thk))

// Right now this takes a bit of math. Instead we'd like to be able 
// to just refer to the elements and do a plane2plane transform
midPin = positionToEdge(Extrude3D(lowerPinProf, gap)).translateX(thk+gap)
lowerHex = positionToEdge(Extrude3D(lowerHexProf, thk)).translateX(-thk)

lowerConeR = Rectangle(pt(0,0), h/2, h/4)
lowerConeProf = Chamfer(lowerConeR, lowerConeR.topRight, h/5)
lowerCone = Revolve(lowerConeProf, lowerConeR.left).rotateXDeg(90)
lowerCone = positionToEdge(lowerCone).translateX(-2*thk)

geometry = Seq(main3, clamp3L, clamp3M, clamp3U, clampPin, lowerPin, midPin, lowerHex, lowerCone)
draw(geometry.map(g => g.rotateYDeg(-90).translateX(h)),
     color(gray), stroke(black), style(translucent(0.9)))
"""

val quad = """parameters {
  w1 = 200
  armW = 20
  outerR = 30
  dR = 20
  deg = 90
  armF = 0.8
  baseW = 200
  baseH = 200
  thk = 20
}

center = pt(500, 200)
arm = Rectangle(center.translate(0,-armW/2), w1, armW)
r1 = arm.rotateDeg(center, -45)

r1Right = single(query(r1.edges, from(arm.right)))
r1End = r1Right.midpoint

innerR = outerR - dR
cOuter = Circle(r1End, outerR)
cInner = Circle(r1End, innerR)
armBR = Difference(Union(r1,cOuter), cInner)

armTop = Difference(cOuter.scale(r1End,armF),cInner)
draw(armTop, color(red))

be = Extrude3d(armBR,thk)
te = Extrude3d(armTop,thk).translate(0,0,thk)

arm = (angle) =>
  Seq(be.rotateDeg(center, angle), te.rotateDeg(center, angle))

arms = Tabulate(4, i => arm(i*deg))
draw(arms, style(translucent))
draw(r1End, color(blue))

w = baseW
h = baseH
b = Rectangle(center.translate(-w/2,-h/2),w,h)
base = Extrude3d(Fillet(b, b.points, 20),thk).translateZ(thk)
draw(base, style(translucent))
"""

val squiggle = """parameters {
  w1 = 200
  w2 = 75
  h1 = 65
  h2 = 250
  h3 = 75
  w3 = 250
  w4 = 200
  w5 = 50
  chmf = 40.0
}


base = pt(50, 500)

operation program(H1, H2) {
  r1 = Rectangle(base, w1, H1)
  r2 = Rectangle(r1.bottomRight, -w2, H2)
  r3 = Rectangle(r2.topLeft, w3, -h3)
  r4 = Rectangle(r3.topRight, -w2, -H2)
  r5 = Rectangle(r4.bottomLeft, w4, H1)

  u = Union(r1, Union(r2, Union(r3, Union(r4, r5))))
  // In practice, need to use derived from becuase edges get merged.
  cp = query(u.points, derivedFrom(r3.top) and not(derivedFromAny(r1, r5)))
}

p = program(h1, h2)
// p = program(100, 60)

draw(p.r1, p.r2, p.r3, p.r4, p.r5, style(dotted))
draw(p.u, style(translucent(0.1)))
draw(p.cp, color(blue))
c = Chamfer(p.u.single(), p.cp, chmf)
draw(c, style(translucent))
"""

val filletTest = """parameters {
  x = 400
  r = 100
  rc2 = 20
  theta = 6
  theta2 = 8.36
  t = 0.274
}

center = pt(200,200).translate(x/2,x/2)
a = Rectangle(pt(200, 200), x, x).rotate(center, theta)
c1 = Fillet(a, a.points, r)
// println()
// draw(c1, style(translucent))

b = Rectangle(center, x, x).rotate(center, theta2)
d = Difference(c1, b).single()
// draw(d, style(translucent))

dpts = query(d.points, fromAny(b.points))
c2 = Fillet(d, dpts, r)
draw(c2, style(translucent))

dp2 = query(d.points, and(fromAny(b.edges),not(fromAny(b.points))))
// draw(dp2, color(blue))
c3 = Fillet(c2, dp2, r*t)
draw(c3, style(translucent))

"""

val arcRectFail = """parameters {
  rectWidth = 300
  center = 341.9
  height = 200
  radius = 80
  ratio = 1.0
  offset = 10
}

e = Circle(pt(center,height), radius)
f = Rectangle(pt(200,height-radius/2), rectWidth, radius)
u = Union(e,f).single()

draw(e, style(dotted))
draw(f, style(dotted))

innerX = center+radius+(offset*4.0)
g = Circle(pt(innerX,height), radius * ratio)
draw(g, style(dotted))
v = Difference(u, g)

inner = Circle(pt(innerX-radius, height), 10)
draw(inner, style(dotted))

x = Difference(v, inner).single()
"""

val chamferTest = """parameters {
  x = 400
  r = 100
  rc2 = 20
  crossR = 20
  crossAng = 20
}

a = Rectangle(pt(200, 200), x, x)
c1 = Chamfer(a, a.points, r)
draw(c1, style(translucent))

corner = Circle(a.topRight, x / 2)
d = Difference(a, corner).single()
cornerPts1 = query(d.points, fromAll(corner, a))
c2 = Chamfer(d, cornerPts1, rc2)
// draw(cornerPts1, color(red))
draw(c2, style(translucent))

u = Union(a, corner).single()
cornerPts2 = query(u.points, fromAll(corner,a))
// draw(u, color(black))

c3 = Chamfer(u, cornerPts2, rc2)
draw(cornerPts2, color(red))
draw(c3, style(translucent))


df = Difference(c3, c2) // Wow, it works...
// draw(df, color(red))
dfe = Extrude3d(df, 100.0)
draw(dfe, style(translucent))

center = pt(200+x/2,200+x/2)
h = Rectangle(pt(200,200+x/2),x,x/4)
v = Rectangle(pt(200+x/2,200),x/4,x)
u = Union(h,v).single().rotateDeg(center, crossAng)
// draw(u, color(blue))
innerPts = query(u.points, fromAll(h,v))
hvc = Chamfer(u, innerPts, crossR)
draw(hvc, color(blue))


// draw(query(c1.edges, fromAny(a.edges)), color(red))
"""

val latheCollet = """parameters {
  thk = 20.0
  gap = 10.0
  r0 = 30.0
  h = 20
  thetaBase = 0.7
  thetaDiff = 1.0
  filletR = 5.0
  notchW = 26.5
  notchBase = 3.14
  notchDiff = 1.0
  notchBase2 = 2.48
  notchDiff2 = 2.0
}

center = pt(500,200)
r1 = r0+thk; r2 = r1+gap
r3 = r2+thk; r4 = r3+gap
r5 = r4+thk

t0 = thetaBase
t1 = t0 + thetaDiff
t2 = t1 + thetaDiff

rc1 = center.translate(0,-h/2)
subRect = (r, t) =>
  Rectangle(rc1, r + thk + gap/2, h).rotate(center, t)

sub1 = subRect(r0, t0)
sub2 = subRect(r2, t1)
sub3 = subRect(r4, t2)

i1 = Union(Circle(center, r0), sub1)
i2 = Union(Circle(center, r2), sub2)
i3 = Union(Circle(center, r4), sub3)

c1 = Difference(Circle(center, r1), i1)
c2 = Difference(Circle(center, r3), i2)
c3 = Difference(Circle(center, r5), i3)

draw(query(c3.points, from(sub3)), color(orange))

filletC = (c, sub) => 
 Fillet(c.single(), query(c.points, from(sub)), filletR)

f1 = filletC(c1, sub1)
f2 = filletC(c2, sub2)
f3 = filletC(c3, sub3)

n0 = notchBase;  n1 = n0 + notchDiff; n2 = n1 + notchDiff
m0 = notchBase2; m1 = m0 + notchDiff2; m2 = m1 + notchDiff2

ins0 = Circle(center, r1-thk/4)
ins1 = Circle(center, r3-thk/4)
ins2 = Circle(center, r5-thk/4)

notchBase = (r) => 
  Rectangle(center.translate(r+thk/4,-notchW/2), thk, notchW)
notch = (r, theta, negative) => 
  Difference(notchBase(r).rotate(center, theta), negative).single()

notch1 = notch(r0, n0, ins0)
notch2 = notch(r0, m0, ins0)
notch3 = notch(r2, n1, ins1)
notch4 = notch(r2, m1, ins1)
notch5 = notch(r4, n2, ins2)
notch6 = notch(r4, m2, ins2)

g1 = Difference(Difference(f1, notch1), notch2)
g2 = Difference(Difference(f2, notch3), notch4) // single(query(.positivePolygons, from(f2)))
g3 = Difference(Difference(f3, notch5), notch6)

ps1 = query(g1.points, fromAny(notch1, notch2) and not(from(ins0)))
draw(Extrude3D(g1, 125), Extrude3D(g2, 100), Extrude3D(g3, 75), style(translucent))
draw(ps1, color(blue))
"""

val ctrl1 = """parameters {
  w0 = 250
  h0 = 100
  w1 = 200
  h1 = 120
  inset = 37
  angle = 45
  round = 50
  lower = 80
}

basePt = pt(500, 200)
top = Rectangle(basePt, w0, h0)
rightR = Rectangle(top.topRight, w1, h1)
rightT = rightR.translate(-inset,0)
leftR = Rectangle(top.topLeft, -w1, h1)
leftT = leftR.translate(inset, 0)

right = rightT.rotateDeg(top.topRight, -angle)
left = leftT.rotateDeg(top.topLeft, angle)

u = Union(Union(top, right),left).single()
draw(u, style(translucent))
draw(rightR.right, leftR.left, color(red))
draw(query(rightT.points, from(rightR.right)), color(red))
"""

val controller3d = """parameters {
  w0 = 250
  h0 = 100
  w1 = 200
  h1 = 120
  inset = 37
  angle = 45
  round = 50
  lower = 17
}

basePt = pt(200, 200)
top = Rectangle(basePt, w0, h0)
rightR = Rectangle(top.topRight, w1, -h1)
rightT = rightR.translate(-inset,0)
leftR = Rectangle(top.topLeft, -w1, -h1)
leftT = leftR.translate(inset, 0)

right = rightT.rotateDeg(top.topRight, -angle)
left = leftT.rotateDeg(top.topLeft, angle)

u = Union(Union(top, left), right).single()

outerPts = query(u.points, fromAny(rightR.right, leftR.left))
f = Fillet(u, outerPts, round)

topPts = query(f.points, fromAny(rightR.topLeft, leftR.topRight))
g = Fillet(f, topPts, round * 0.25)

innerPts = query(g.points, from(top))

h = Fillet(g, innerPts, round * 0.5)
c0 = Circle(basePt.translate(0, lower), 25)
c1 = Circle(basePt.translate(w0,lower), 25)

ip = Difference(h, c0)
print(ip)
i = Difference(ip, c1)

empty(query(i.positivePolygons, fromAny(c0,c1)))
draw(i, color(red), style(translucent))

j = Extrude3d(i, 50.0)
draw(j, color(pink), style(translucent))
"""

val controller = """parameters {
  w0 = 250
  h0 = 100
  w1 = 200
  h1 = 120
  inset = 37
  angle = 45
  round = 50
  lower = 17
}

basePt = pt(200, 200)
top = Rectangle(basePt, w0, h0)
rightR = Rectangle(top.topRight, w1, -h1)
rightT = rightR.translate(-inset,0)
leftR = Rectangle(top.topLeft, -w1, -h1)
leftT = leftR.translate(inset, 0)

right = rightT.rotateDeg(top.topRight, -angle)
left = leftT.rotateDeg(top.topLeft, angle)

draw(top, right, left, style(dotted))
u = Union(Union(top, left), right).single()
// draw(u, style(translucent))

outerPts = query(u.points, fromAny(rightR.right, leftR.left))
// draw(outerPts, color(blue))
f = Fillet(u, outerPts, round)

topPts = query(f.points, fromAny(rightR.topLeft, leftR.topRight))
// draw(topPts, color(blue))
g = Fillet(f, topPts, round * 0.25)

draw(g, style(translucent(0.25)))
draw(g.points, color(orange))

innerPts = /*   QUERY HERE                                                                    */ query(g.points, from(top))
// innerPts = query(g.points, ???) // <-- Uncomment to begin
draw(innerPts, color(blue))
"""

val mergeRects = """parameters {
  r = 200
  f = 0.75
  x1 = 0 < 0 < 100.0
  x2 = 0 < 1 < 20.0
  x3 = 150 
  y3 = 150
}

s0 = Rectangle(pt(0,0), r, r)
s1 = Rectangle(s0.topRight, -r*f, r*f).translate(-x1,0)
draw(s0, s1, style(dotted))
u = Union(s0, s1)

// draw(u, style(translucent(0.25)))
// draw(u.points)
// draw(query(u.edges, from(s0.right)), color(orange))
// draw(query(u.edges, directFrom(s0.right)), color(blue))

s2 = Rectangle(pt(x3, y3), 100, 100)
d  = Difference(u, s2)

draw(d, style(translucent(0.25)))
draw(query(d.edges, from(s0.right)), color(orange))
draw(query(d.edges, directFrom(s0.right)), color(blue))


// s2 = Rectangle(s1.bottomLeft, -r*f, -r*f).translate(-x2, 0)
// d = Difference(u, s2)
// draw(u, s2, style(dotted))
// draw(d, style(translucent(0.25)))
// draw(d.points)
// draw(query(d.edges, from(s1.left)), color(orange))
// draw(query(d.edges, directFrom(s1.left)),color(blue))
"""
