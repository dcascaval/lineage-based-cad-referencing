package elodin.three

import scalajs.js
import org.scalajs.dom.{document, window}

import prelude.*
import typings.three.materials.*
import typings.three.lights.*

import elodin.global.api.*

extension [T <: BufferGeometry](geo: T)
  def attribute(attr: String) =
    geo.attributes(attr).asInstanceOf[BufferAttribute with InterleavedBufferAttribute]

extension [T <: BufferAttribute](attr: T)
  def float32Array =
    attr.array.asInstanceOf[Float32Array]
