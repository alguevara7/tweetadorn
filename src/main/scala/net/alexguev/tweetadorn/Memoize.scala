package net.alexguev.tweetadorn

import scala.collection.mutable

class Memoize [-T, +R](f: T => R, private[this] val vals: mutable.Map[T, R]) extends (T => R) {

  def apply(x: T): R = {
    if (vals.contains(x)) {
      vals(x)
    }
    else {
      val y = f(x)
      vals.put(x, y)
      y
    }
  }
}

object Memoize {
  def apply[T, R](f: T => R) = memoize(f)
  def memoize[T, R](f: T => R) = new Memoize(f, mutable.Map.empty[T, R])
  def memoize[T, R](f: T => R, m: mutable.Map[T, R]) = new Memoize(f, m)
}