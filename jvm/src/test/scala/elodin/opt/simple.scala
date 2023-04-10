package elodin.opt

import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import org.scalatest.funsuite.*

import Kernel.*
import RegionIntersection.*
import api.*
import elodin.geometry.*

class TestSimpleReference extends AnyFunSuite:
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
      println(s"Rectangles: $r1 $r2")

      val unionResult = UnionPolygons(r1, r2)
      println(s"Union Result: $unionResult")

      assert(unionResult.polygons.size == 1)
      val u = unionResult.polygons.list(0)

      // Should be one line
      val farRight = single(u.edges.query(from(r2.right)))
      println(s"farRight = $farRight")

      // Should be two lines, above and below the split.
      val bothRight = u.edges.query(from(r1.right))
      assert(bothRight.size == 2)
      println(s"bothRight = $bothRight")

      // Should be the lower of the above two lines.
      val bottomRight = single(
        query(u.edges, contains(from(r1.right), from(r1.bottomRight)))
      )
      println(s"bottomRight = $bottomRight")

      // 4 points
      val middlePoints = u.points.query(from(r1.right))
      assert(middlePoints.size == 4)
      println(s"middlePoints = $middlePoints")

      // Query using a *polygon*, not just points and lines.
      val r2pts = u.points.query(from(r2))
      assert(r2pts.size == 4) // Right points and two intersects

      // Intersects middlePoints and r2points
      val logicalIntersect = query(u.points, fromAll(r2, r1.right))
      assert(logicalIntersect.size == 2)
    }
  }

  test("Reference through transforms") {
    program {
      val a = Square(10, 10, 50)
      val b = Square(30, 30, 50)

      val c = b.rotate(pt(25, 25), 0.01)
      val u = single(UnionPolygons(a, c).polygons)

      // Single-step
      val _bbottom = c.edges.query(from(b.bottom))
      assert(_bbottom.size == 1)
      val ref = query(u.points, fromAll(_bbottom))
      assert(ref.size == 2)

      // Transitive reference
      val points = u.points.query(from(b.bottom))
      assert(points.size == 2)
    }
  }
