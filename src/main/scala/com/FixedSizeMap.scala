package com.github.sophiecollard

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag

sealed abstract class FixedSizeMap[K, V](contents: Array[List[(K, V)]])
  extends Map[K, V] {

  final val isEmpty: Boolean =
    contents.map(_.isEmpty).fold(true)(_ && _)

  private def hash(key: K): Int =
    key.hashCode % contents.length

  final def get(key: K): Option[V] =
    getRecurse(key, contents(hash(key)))

  @tailrec
  private def getRecurse(key: K, entries: List[(K, V)]): Option[V] =
    entries match {
      case Nil =>
        None
      case (k, v) :: tail =>
        if (k == key) Some(v)
        else getRecurse(key, tail)
    }

  final def add(key: K, value: V): FixedSizeMap[K, V] = {
    val index = hash(key)
    new FixedSizeMap(
      unsafeUpdate(
        contents
      )(
        index,
        (key, value) :: contents(index).filterNot { case (k, v) => k == key }
      )
    ) {}
  }

  final def remove(key: K): FixedSizeMap[K, V] = {
    val index = hash(key)
    contents(index) match {
      case Nil =>
        this
      case entries =>
        new FixedSizeMap(
          unsafeUpdate(
            contents
          )(
            index,
            entries.filterNot{ case (k, v) => k == key }
          )
        ) {}
    }
  }

  private def unsafeUpdate[A: ClassTag](
    array: Array[A]
  )(
    index: Int,
    element: A
  ): Array[A] = {
    val builder = ArrayBuilder.make[A]
    builder.addAll(array.slice(0, index))
    builder.addOne(element)
    builder.addAll(array.slice(index + 1, array.length))
    builder.result
  }

  final def toList: List[(K, V)] =
    contents.flatten.toList

  override def toString: String = {
    val contentsAsString = contents
    .toList
    .flatten
    .map { case (k, v) => s"($k -> $v)" }
    .mkString(", ")
    "FixedSizeMap(" ++ contentsAsString ++ ")"
  }

}

object FixedSizeMap {

  def empty[K, V](size: Int): FixedSizeMap[K, V] =
    if (size < 1)
      new FixedSizeMap(Array.fill(1)(List.empty[(K, V)])) {}
    else
      new FixedSizeMap(Array.fill(size)(List.empty[(K, V)])) {}
    
  def fromIterable[K, V](iterable: Iterable[(K, V)]): FixedSizeMap[K, V] =
    iterable.foldLeft(
      empty[K, V](iterable.size)
    ) { case (map, (k, v)) =>
      map.add(k, v)
    }

  def apply[K, V](elements: (K, V)*): FixedSizeMap[K, V] =
    fromIterable(elements)

}
