package elodin.opt

import collection.mutable.Map

class Counter(prefix: String):
  var _id = -1
  def temp() = { _id += 1; s"$prefix${_id}" }
  def clear() = _id = -1

class MultiCounter():
  val mapping = Map[String, Int]()
  def temp(prefix: String) =
    val id = mapping.getOrElseUpdate(prefix, 0)
    mapping(prefix) += 1
    if id == 0 then prefix else s"$prefix$id"
  def clear() = mapping.clear()
