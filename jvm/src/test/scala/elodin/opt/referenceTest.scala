package elodin.opt

import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import org.scalatest.funsuite.*

import Kernel.*
import RegionIntersection.*
import api.*
import elodin.geometry.*

class TestReference extends AnyFunSuite:

  test("Reference through Union") {
    program {
      // This scene depicts a tetris T piece with r1 vertical,
      // and r2 projecting out horizontally from the right side.
      val base1 = pt(10, 10)
      val w1: Double = 20; val h1: Double = 70
      val base2 = pt(15, 30)
      val w2: Double = 70; val h2: Double = 10

      val r1 = Rectangle(base1, w1, h1)
      val r2 = Rectangle(base2, w2, h2)

      val unionResult = UnionPolygons(r1, r2)
      assert(unionResult.polygons.size == 1)
      val u = unionResult.polygons.list(0)

      // Should be one line
      val farRight = single(u.edges.query(from(r2.right)))

      // Should be two lines, above and below the split.
      val bothRight = u.edges.query(from(r1.right))
      assert(bothRight.size == 2)

      // Should be the lower of the above two lines.
      val bottomRight = single(u.edges.query(and(from(r1.right), contains(All, from(r1.bottomRight)))))

      // 4 points
      val middlePoints = u.points.query(from(r1.right))
      assert(middlePoints.size == 4)

      // Query using a *polygon*, not just points and lines.
      val r2pts = u.points.query(from(r2))
      assert(r2pts.size == 4) // Right points and two intersects

      // Intersects middlePoints and r2points
      val logicalIntersect = u.points.query(fromAll(r2, r1.right))
      assert(logicalIntersect.size == 2)
    }
  }

  test("Reference through transforms") {
    program {
      val a = Square(10, 10, 50)
      val b = Square(30, 30, 50)

      val c = b.rotate(Point(25, 25), 0.25)

      val u = UnionPolygons(a, c).single()

      // For now, this requires a transitive reference.
      val _bbottom = c.edges.query(from(b.bottom))
      assert(_bbottom.size == 1)

      val ref = u.points.query(fromAll(_bbottom))
      assert(ref.size == 2)
    }
  }

  ignore("Intersection Example") {
    program {
      val base1 = pt(10, 10)
      val r1 = Rectangle(base1, 50, 50)

      val base2 = pt(30, 30)
      val r2 = Rectangle(base2, 50, 50)

      val result = SubtractPolygons(r1, r2)
      val diff = result.single()

      val pts1 = diff.points.query(
        fromAll(r1.bottom, r2.left) // Single point
      )
      assert(pts1.size == 1)

      val pts2 = diff.points.query(
        from(r2) // Set of 3 points (intersections + inner corner)
      )
      assert(pts2.size == 3)

      val pts3 = diff.points.query(
        fromAll(r1.bottom, r2) // Single point, but could vary if the parameters change.
      )
      assert(pts3.size == 1)
    }
  }

  ignore("Backlinks") {
    program {
      val x = 100
      val y = 200
      val rad = 275

      val a = Rectangle(pt(x, x), rad, rad)
      val b = Rectangle(pt(y, y), rad, rad)
      val diff = SubtractPolygons(a, b)

      val pts = diff.points.query(fromAll(a.bottom, b))
      assert(pts.size == 1)
      val lns = diff.edges.query(fromAll(pts))
      assert(lns.size == 2)

      val poly = diff.single()
      val ptsp = poly.points.query(fromAll(a.bottom, b))
      assert(ptsp.size == 1)
      val lnsp = poly.edges.query(fromAll(pts))
      assert(lnsp.size == 2)
    }
  }

class WIPTest extends AnyFunSuite:
  ignore("Controller Transforms") {
    program {
      val thk = 20.0
      val gap = 10.0
      val r0 = 30.0
      val h = 20
      val thetaBase = 0.25
      val t0 = thetaBase

      val center = pt(500, 200)
      val r1 = r0 + thk
      val c1 = SubtractPolygons(Circle(center, r1), Circle(center, r0))

      val sub1 = Rectangle(center, r0 + thk + gap / 2, h).rotate(center, t0)
      val c4 = SubtractRegions(c1, sub1.asRegion())

      val ps = c4.points.query(fromAny(sub1.edges))
      assert(ps.size == 4)
    }
  }

class TestFind extends AnyFunSuite:
  ignore("Basic Transform") {
    program {
      val b = Square(30, 30, 50)
      val c = b.rotate(Point(25, 25), 0.25)
      val cpts = c.points.query(from(b.bottom))
      assert(cpts.size == 2)
    }
  }

  ignore("Transform Transitive") {
    program {
      val a = Square(10, 10, 50)
      val b = Square(30, 30, 50)
      val c = b.rotate(Point(25, 25), 0.25)
      val u = UnionPolygons(a, c).single()

      val cpts = c.points.query(from(b.bottom))
      assert(cpts.size == 2)
      val upts = u.points.query(fromAny(cpts))
      assert(upts.size == 2)

      // However, that doesn't mean we can't do it in one step,
      // using `from()` to automate the transitive references.
      // println(u.points)
      val succPoints = u.points.query(from(b.bottom))
      assert(succPoints.size == 2)
    }
  }

  ignore("Transitive Diamond") {
    program {
      val a = Square(10, 10, 50)
      val e = Square(30, 30, 50) // Extraneous

      val b = a.scale(a.topRight, Point(2.0, 0.5))
      val c = SubtractPolygons(a, e).single()
      // println(s"c Op   = ${c.operation}}")
      // println(s"c Args = ${c.operation.get.opArgs}")

      val d = UnionPolygons(b, c).single()
      // println(s"d Op   = ${d.operation}}")
      // println(s"d Args = ${d.operation.get.opArgs}")

      val qB0 = b.edges.query(from(a.left))
      val qC0 = c.edges.query(from(a.left))
      assert(qB0.size == 1)
      assert(qC0.size == 1)

      val qB1 = b.edges.query(from(a.left))
      val qC1 = c.edges.query(from(a.left))
      assert(qB1.size == 1)
      assert(qC1.size == 1)

      assert(qB0 == qB1)
      assert(qC0 == qC1)

      val bLeft = qB0.list(0)
      // println(s"Line(operation = ${bLeft.operation}, parents = ${bLeft.parents.map(_.id)})")
      val qBFrom = d.edges.query(fromAll(qB1))
      val qBFind = d.edges.query(fromAll(qB1))

      val qCFrom = d.edges.query(fromAll(qC1))
      val qCFind = d.edges.query(fromAll(qC1))

      assert(qBFrom.size == 1)
      assert(qBFrom == qBFind)
      assert(qCFrom.size == 1)
      assert(qCFrom == qCFind)

      val qf0 = d.edges.query(or(fromAll(qB1), fromAll(qC1)))
      val qf1 = d.edges.query(fromAny(qB1, qC1))
      assert(qf0.size == 2)
      assert(qf0 == qf1)
      // println(qf0)

      val leftLines = d.edges.query(from(a.left))
      assert(leftLines.size == 2)

      val topLines = d.edges.query(from(a.top))
      val rightLines = d.edges.query(from(a.right))
      assert(topLines.size == 1)
      assert(rightLines.size == 1)
    }
  }

class TestRegionBoolean extends AnyFunSuite:
  ignore("Subtract Pos/Neg") {
    program {
      val a = Square(10, 10, 10)
      val b = Square(15, 15, 10)
      val d = SubtractPolygons(a, b)

      assert(d.polygons.size == 1)
      assert(d.positivePolygons.size == 1)
      assert(d.negativePolygons.size == 0)
    }
  }

  ignore("Subtract Pos/Neg II") {
    program {
      val w2 = 241.9; val h0 = 100.0; val r = 40.0;
      val e = Circle(pt(w2, h0), r)
      val f = Rectangle(pt(100, h0 - r / 2), 200, r)
      val u = UnionPolygons(e, f).single()
      val g = Circle(pt(300, h0), r)
      val v = SubtractPolygons(u, g)

      assert(v.polygons.size == 1)
      assert(v.positivePolygons.size == 1)
      assert(v.negativePolygons.size == 0)
    }
  }

  ignore("Subtract Pos/Neg III") {
    program {
      val a = Square(10, 10, 100)
      val b = Circle(pt(55, 55), 20) // Completely inside
      val d = SubtractPolygons(a, b)

      assert(d.polygons.size == 2)
      assert(d.positivePolygons.size == 1)
      assert(d.negativePolygons.size == 1)
    }
  }

class TestRegionReference extends AnyFunSuite:
  ignore("Region Ref") {
    program {
      val s1 = Square(10, 10, 10)
      val s2 = Square(15, 15, 10)
      val r1 = PolygonRegion(s1)
      val r2 = PolygonRegion(s2)

      val u = UnionRegions(r1, r2)

      val l1 = u.edges.query(from(s1.top))
      val l2 = u.edges.query(from(s2.bottom))
      assert(l1.size == 1)
      assert(l2.size == 1)

      val p1 = u.points.query(from(s1.top))
      val p2 = u.points.query(from(s2.bottom))
      assert(p1.size == 2)
      assert(p2.size == 2)
    }
  }

  // This isn't actually a bug with the region; it was with the arcs not propagating
  // information correctly when copied during splitPolygon
  ignore("Complex Region Ref") {
    program {
      val rectWidth = 300
      val center = 341.9
      val height = 200
      val radius = 80
      val ratio = 1.0
      val offset = 10

      val e = Circle(pt(center, height), radius)
      val f = Rectangle(pt(200, height - radius / 2), rectWidth, radius)
      val u = UnionPolygons(e, f).single()

      val innerX = center + radius + (offset * 4.0)
      val g = Circle(pt(innerX, height), radius * ratio)
      val v = SubtractPolygons(u, g)
      val inner = Circle(pt(innerX - radius, height), 10)

      val x = SubtractRegions(v, PolygonRegion(inner)).single()
      val y = SubtractPolygons(v.single(), inner).single()

      // It works if we help it along
      val l1 = u.edges.query(from(f))
      val l2 = v.edges.query(fromAny(l1))
      val l3 = x.edges.query(fromAny(l2))
      assert(l3.size == 3)

      // And directly? Yes!
      val l4 = x.edges.query(from(f))
      assert(l4.size == 3)

      val p1 = v.points.query(from(g))
      assert(p1.size == 2)
      val p1s = v.single().points.query(from(g))
      assert(p1s.size == 2)

      // extension (set: Set[Reference]) //
      //   def show() = s"{${set.map(_.id).mkString("; ")}}"
      // println(s"Y = ${y.points.list.map(_.parents.ofType[Point].show()).mkString(" & ")} ")
      // println(s"Y = ${y.points}")
      // println(s"P1 = ${p1.list.toSet.show()}")
      // println(s"P1S = ${p1s.list.toSet.show()}")

      // What about on Y?
      val p2y = y.points.query(fromAny(p1))
      assert(p2y.size == 2)

      val p2 = x.points.query(fromAny(p1))
      assert(p2.size == 2)

      // and, directly.
      val pts = x.points.query(from(g))
      assert(pts.size == 2)
    }
  }
