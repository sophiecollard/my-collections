package com.github.sophiecollard

trait Map[K, V] {

	def isEmpty: Boolean

	def get(key: K): Option[V]

	def add(key: K, value: V): Map[K, V]

	def remove(key: K): Map[K, V]

	def toList: List[(K, V)]

}
