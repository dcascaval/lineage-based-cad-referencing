package elodin.global

import annotation.targetName
import collection.mutable.{Buffer, ArrayBuffer}

object api:
  export elodin.global.SequenceMethods.{given, *}
  export elodin.global.Conversions.{given, *}
  export elodin.global.Numeric.{given, *}
  export elodin.global.PointMethods.{given, *}

object SequenceMethods:
  extension [T](seq: Seq[T])
    // TODO: This is NOT a cheap function...
    def dedupDouble(f: T => Double, tolerance: Double) =
      val buf = collection.mutable.Buffer[T]()
      val set = collection.mutable.Buffer[Double]()
      for element <- seq do
        val fE = f(element)
        set.find(d => math.abs(d - fE) <= tolerance) match
          case Some(_) => ()
          case None =>
            set += fE
            buf += element
      buf

  extension [T](seq: collection.mutable.Buffer[T])
    def dedupDouble(f: T => Double, tolerance: Double) =
      val buf = collection.mutable.Buffer[T]()
      val set = collection.mutable.Buffer[Double]()
      for element <- seq do
        val fE = f(element)
        set.find(d => math.abs(d - fE) <= tolerance) match
          case Some(_) => ()
          case None =>
            set += fE
            buf += element
      buf

  extension [A](seq: Seq[A])
    /** Rotate the list to the left by `offset`. Negative offset will rotate the corresponding amount to
      * the right.
      */
    def rotate(offset: Int) =
      // Rotate positive means that we take one off the front
      // and add it to the end.
      if offset > 0 then seq.drop(offset) ++ seq.take(offset)
      // Rotate negative means that we take some off the end
      // and add them to the front.
      else if offset < 0 then seq.takeRight(-offset) ++ seq.dropRight(-offset)
      else seq

    def shifted(offset: Int = 1, wrap: Boolean = true) =
      val result = seq.drop(offset)
      if (wrap) then result ++ seq.take(offset)
      else result

    def wrappedPairs =
      seq.zip(seq.shifted())

    def choose =
      val r = util.Random((math.random() * Integer.MAX_VALUE).toLong)
      seq(r.nextInt(seq.length))

    def chooseIndex =
      val r = util.Random((math.random() * Integer.MAX_VALUE).toLong)
      r.nextInt(seq.length)

  extension [A](buf: Buffer[A])
    def mapSequencePairs[B](f: (A, A) => B): Buffer[B] =
      val newBuf: Buffer[B] = new ArrayBuffer(buf.size - 1)
      var i = 0; val n = buf.size
      while (i + 1) < n do
        newBuf += f(buf(i), buf(i + 1))
        i += 1
      newBuf

def dbg[T](expr: => T): T =
  val result = expr
  println(result.toString)
  result

object Conversions:
  extension (a: Any) //
    inline def as[T] = a.asInstanceOf[T]

object Numeric:
  val tau = 2.0 * math.Pi

  def deg2rad(t: Double): Double =
    (t / 360.0) * 2.0 * math.Pi

  def clamp(i: Double, min: Double, max: Double) =
    math.max(min, math.min(i, max))

  extension (x: Double)
    inline def mod(m: Double): Double =
      val r = x % m
      if r < 0.0 then r + m else r

  extension (x: Int)
    def mod(m: Int): Int =
      val r = x % m
      if r < 0 then r + m else r

object PointMethods:
  extension (p: (Double, Double))
    inline def +(q: (Double, Double)) =
      (p(0) + q(0), p(1) + q(1))
    inline def -(q: (Double, Double)) =
      (p(0) - q(0), p(1) - q(1))
    inline def *(q: Double) =
      (p(0) * q, p(1) * q)
    def rawSVG =
      s"${p(0)} ${p(1)}"
    inline def x = p(0)
    inline def y = p(1)

    inline def dot(other: (Double, Double)): Double =
      x * other.x + y * other.y

    inline def rotate(theta: Double): (Double, Double) =
      val sint = math.sin(theta)
      val cost = math.cos(theta)
      (x * cost - y * sint, x * sint + y * cost)

    inline def spread(other: (Double, Double)): Double =
      val vu = p.dot(other)
      val uu = other.dot(other)
      val vv = p.dot(p)
      1.0 - ((vu * vu) / (uu * vv))

    // Approximate vector angle as described here:
    // https://www.freesteel.co.uk/wpblog/2009/06/05/encoding-2d-angles-without-trigonometry/
    inline def diamondAngle =
      if p.y >= 0 then //
        if p.x >= 0 then p.y / (p.x + p.y) else 1 - p.x / (-p.x + p.y)
      else //
      if p.x < 0 then 2 - p.y / (-p.x - p.y)
      else 3 + p.x / (p.x - p.y)

    inline def withLength(newLength: Double): (Double, Double) =
      val ratio = newLength / length
      (ratio * x, ratio * y)

    inline def length: Double = math.sqrt(lengthSquared)
    inline def lengthSquared: Double = (x * x) + (y * y)

    inline def distSquared(other: (Double, Double)) =
      val dx = other.x - p.x
      val dy = other.y - p.y
      (dx * dx) + (dy * dy)

  extension (p: (Double, Double, Double))
    inline def +(q: (Double, Double, Double)) =
      (p(0) + q(0), p(1) + q(1), p(2) + q(2))
    inline def -(q: (Double, Double, Double)) =
      (p(0) - q(0), p(1) - q(1), p(2) - q(2))
    inline def *(q: Double) =
      (p(0) * q, p(1) * q, p(2) * q)
    def rawSVG =
      s"${p(0)} ${p(1)}"
    inline def x = p(0)
    inline def y = p(1)
    inline def z = p(2)

    inline def negate(): (Double, Double, Double) =
      (-p.x, -p.y, -p.z)

    inline def cross(q: (Double, Double, Double)): (Double, Double, Double) =
      (y * q.z - z * q.y, -(x * q.z - z * q.x), x * q.y - y * q.x)

    inline def withLength(newLength: Double): (Double, Double, Double) =
      val ratio = newLength / length
      (ratio * x, ratio * y, ratio * z)

    inline def dot(q: (Double, Double, Double)): Double =
      x * q.x + y * q.y + z * q.z

    inline def length: Double = math.sqrt(lengthSquared)
    inline def lengthSquared: Double = (x * x) + (y * y) + (z * z)
    inline def dist(q: (Double, Double, Double)) =
      val dx = q.x - p.x; val dy = q.y - p.y; val dz = q.z - p.z
      math.sqrt((dx * dx) + (dy * dy) + (dz * dz))
