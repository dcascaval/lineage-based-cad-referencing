package elodin.opt

import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import org.scalatest.funsuite.*

import Kernel.*
import RegionIntersection.*
import api.*
import elodin.geometry.*

class TestSubgraphReference extends AnyFunSuite:
  test("Subgraph References") {
    program {
      val a = Square(10, 10, 50)
      val b = Square(30, 30, 50)
      val c = Square(60, 60, 50)

      val u = UnionRegions(UnionPolygons(a, b), c.asRegion())
      assert(u.polygons.size == 1)
    }
  }

  test("Subtract Polygons Positive") {
    program {
      val a = Square(10, 10, 50)
      val b = Square(30, 30, 50)

      val s = SubtractPolygons(a, b)
      assert(s.polygons.size == 1)
      assert(s.positivePolygons.size == 1)
      assert(s.negativePolygons.size == 0)
    }
  }

  test("Subtract Polygons Negative") {
    program {
      val a = Square(10, 10, 50)
      val b = Square(20, 20, 20)

      val s = SubtractPolygons(a, b)
      assert(s.polygons.size == 2)
      assert(s.positivePolygons.size == 1)
      assert(s.negativePolygons.size == 1)
    }
  }
