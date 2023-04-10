package elodin.opt

import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import org.scalatest.funsuite.*

import Kernel.*
import RegionIntersection.*
import elodin.geometry.*
import Operations.*

class TestArcBooleans extends AnyFunSuite:
  import Queries.*

  test("Rectangle/Circle Boolean") {
    program {
      val e = Circle(pt(100, 300), 40)
      val f = Rectangle(pt(100, 280), 200, 40)

      val ix1 = e.curve.intersect(f.bottom)
      val ix2 = e.curve.intersect(f.top)
      assert(ix1.size == 1)
      assert(ix2.size == 1)

      val ix3 = f.bottom.intersect(e.curve)
      val ix4 = f.top.intersect(e.curve)
      assert(ix1.size == 1)
      assert(ix2.size == 1)

      val u = UnionPolygons(e, f).single()
      // println(u)
      // println(u.points.list)
      // println(u.edges.list)

      assert(u.points.size == 4)
      assert(u.edges.size == 4)
    }
  }

  test("Rectangle/Circle Boolean II") {
    program {
      val e = Circle(pt(150, 300), 40)
      val f = Rectangle(pt(100, 280), 200, 40)
      val u = UnionPolygons(e, f).single()

      val ln = Line(pt(115.35898384862244, 280.0), pt(100.0, 280.0))
      val r = containsPoint(e, ln.eval(0.5))
      assert(r == false)

      // println(u)
      // println(u.points.list)
      // println(u.edges.list)

      assert(u.points.size == 8)
      assert(u.edges.size == 8)
    }
  }

  test("Rectangle/Circle Boolean III") {
    program {
      val e = Circle(pt(122.99999999999999, 300), 40)
      val f = Rectangle(pt(100, 280), 200, 40)

      val ix1 = e.curve.intersect(f.bottom)
      val ix2 = e.curve.intersect(f.top)
      assert(ix1.size == 1)
      assert(ix2.size == 1)

      val ix3 = f.bottom.intersect(e.curve)
      val ix4 = f.top.intersect(e.curve)
      assert(ix1.size == 1)
      assert(ix2.size == 1)

      val u = UnionPolygons(e, f).single()
      // println(u)
      // println(u.points.list)
      // println(u.edges.list)

      assert(u.points.size == 4)
      assert(u.edges.size == 4)
    }
  }

  test("Circle/Circle Boolean I") {
    program {
      val e = Circle(pt(206.39999999999998, 300), 40)
      val f = Rectangle(pt(100, 280), 200, 40)
      val u = UnionPolygons(e, f).single()
      val g = Circle(pt(300, 300), 40)
      val v = SubtractPolygons(u, g).single()
    }
    // println(v)
    // println(v.edges)
  }

  test("Rectangle/Circle Boolean IV") {
    program {
      val innerR = 40
      val outerR = 110
      val h = 19.84
      val a = Rectangle(pt(100, 100), 325, 150)
      val r = Rectangle(pt(100, 175 - h), 250, 40)
      val center = pt(400 - (75 - innerR), 175)
      val b = Circle(center, innerR)
      val c = Circle(center, outerR)

      val removal = UnionPolygons(r, b).single()
      val d1 = SubtractPolygons(c, removal).single()
      val d2 = SubtractPolygons(a, removal).single()
      val u = UnionPolygons(d1, d2).single()

      assert(u.edges.size == 9)
      val inner = u.points.query(fromAll(r, b))
      assert(inner.size == 2)

      val r2 = UnionPolygons(r, b)
      val d3 = SubtractRegions(c.asRegion(), r2)
      val d4 = SubtractRegions(a.asRegion(), r2)
      val u2 = UnionRegions(d3, d4)
      assert(u2.polygons.size == 1)

      assert(u2.edges.size == 9)
      val inner2 = u2.points.query(fromAll(r, b))
      assert(inner2.size == 2)
    }
  }

class TestArc1 extends AnyFunSuite:
  test("Circle/Circle Difference") {
    program {
      val c1 = Circle(pt(50, 50), 20)
      val c2 = Circle(pt(50, 50), 10)

      val d = SubtractPolygons(c1, c2)
      assert(d.polygons.size == 2)
      assert(d.positivePolygons.size == 1)
      assert(d.negativePolygons.size == 1)

      val e = UnionPolygons(c1, c2)
      assert(e.polygons.size == 1)
      assert(d.positivePolygons.size == 1)
    }
  }

class TestLineArc extends AnyFunSuite:
  val epsilon = 1e-5
  given Equality[Double] = TolerantNumerics.tolerantDoubleEquality(epsilon)

  test("Line-Arc 1 Intersect") {
    program {
      val line1 = Line(pt(5.0, 5.0), pt(5.0, 10.0)) // vertical
      val line2 = Line(pt(5.0, 5.0), pt(10.0, 5.0)) // horizontal

      val arc = Arc(pt(3.0, 3.0), radius = 3.0, theta0 = 0.2, theta1 = 1.0)

      val result1 = arc.intersect(line1)
      val result2 = arc.intersect(line2)

      // println(result1)
      assert(result1.size == 1)
      assert(result1(0).point.dist(pt(5.0, 5.236068)) === 0.0)
      assert(result2.size == 1)
      assert(result2(0).point.dist(pt(5.236068, 5.0)) === 0.0)
    }
  }

  test("Line-Arc 2 Intersect") {
    program {
      val line = Line(pt(0.0, 5.0), pt(5.0, 6.0))
      val arc = Arc(pt(3.0, 3.0), radius = 3.0, theta0 = 1.0, theta1 = 3.0)

      val result = arc.intersect(line)
      // println(result)
      assert(result.size == 2)

      val expected = Seq(pt(4.050434, 5.810087), pt(0.949566, 5.189913))
      assert(result.forall(r => expected.exists(e => r.point.dist(e) === 0.0)))
    }
  }

class TestEdgeCases extends AnyFunSuite:
  test("Arc containment") {
    program {
      val arc1 = Arc(pt(241.9, 300), 40, 2.617993877991495, 2.3836473440103054)
      val arc2 = Arc(pt(241.9, 300), 40, 3.8995379631692813, 3.665191429188092)
      val circ = Circle(pt(300, 300), 40)

      // This is.. true, the lines don't intersect
      assert(!containsPoint(circ, arc1.eval(0.5)))
      assert(!containsPoint(circ, arc2.eval(0.5)))
    }
  }

class TestArcArc extends AnyFunSuite:
  test("Arcs do not intersect") {
    program {
      val arc1 = Arc(pt(1, 1), radius = 2.0, theta0 = 0.1, theta1 = 3.0)
      val arc2 = Arc(pt(1, 1), radius = 1.0, theta1 = 3.1, theta0 = 6.2)
      val result = arc1.intersect(arc2)
      // println(result)
      assert(result.size == 0)

      val arc3 = Arc(pt(0, 0), radius = 1.0, theta0 = 0.0, theta1 = math.Pi)
      val arc4 = Arc(pt(1, 0), radius = 1.0, theta0 = math.Pi, theta1 = 2.0 * math.Pi)

      val result2 = arc3.intersect(arc4)
      val result3 = arc4.intersect(arc3)
      assert(result2.size == 0)
      assert(result3.size == 0)
    }
  }

  test("Arcs are tangent") {
    program {
      val p1 = pt(4.09, 4.8); val p2 = pt(7.11, 3.54)
      val dist = p1.dist(p2)
      val arc1 = Arc(p1, radius = dist * 0.75, theta0 = 4.74, theta1 = 6.12)
      val arc2 = Arc(p2, radius = dist * 0.25, theta0 = 1.0, theta1 = 3.2)
      val result = arc1.intersect(arc2)
      // println(result)
      assert(result.size == 1)
    }
  }

  test("Arcs Intersect twice") {
    program {
      val arc1 = Arc(pt(1.20, 1.65), radius = 1.0, theta0 = 0.72, theta1 = 2.29)
      val arc2 = Arc(pt(1.03, 2.84), radius = 0.5, theta0 = 3.14, theta1 = 5.96)

      val result = arc1.intersect(arc2)
      // println(result)
      assert(result.size == 2)
    }
  }

  test("Circles intersect twice") { // but the arcs do not :)
    program {
      val arc1 = Arc(pt(1.20, 1.65), radius = 1.0, theta0 = 0.72, theta1 = 2.29)
      val arc2 = Arc(pt(1.03, 2.84), radius = 0.5, theta0 = 3.14, theta1 = 5.34)

      val result = arc1.intersect(arc2)
      // println(result)
      assert(result.size == 1)
    }
  }
