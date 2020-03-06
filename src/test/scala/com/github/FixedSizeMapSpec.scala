package com.github.sophiecollard

import org.scalatest._

class FixedSizeMapSpec extends WordSpec with Matchers {
  
  "FixedSizeMap#isEmpty" should {
    "return true if empty, and false otherwise" in {
      FixedSizeMap[Int, String]().isEmpty should be(true)
      FixedSizeMap((1, "Garfield"), (2, "Odie")).isEmpty should be(false)
    }
  }

  "FixedSizeMap#get" should {
    "return the value corresponding to the specified key, if any" in {
      val map = FixedSizeMap((1, "Garfield"), (2, "Odie"))
      map.get(1) should be(Some("Garfield"))
      map.get(3) should be(None)
    }
  }

  "FixedSizeMap#add" should {
    "return a new instance with an added key-value pair" in {
      val map = FixedSizeMap((1, "Garfield"), (2, "Odie"))
      map.get(3) should be(None)
      val biggerMap = map.add(3, "Snoopy")
      biggerMap.get(3) should be(Some("Snoopy"))
    }

    "return a new instance with an overriden key-value pair" in {
      val map = FixedSizeMap((1, "Garfield"), (2, "Odie"))
      map.get(2) should be(Some("Odie"))
      val updatedMap = map.add(2, "Snoopy")
      updatedMap.get(2) should be(Some("Snoopy"))
      val smallerMap = updatedMap.remove(2)
      smallerMap.get(2) should be(None)
    }
  }

  "FixedSizeMap#remove" should {
    "return a new instance with a removed key-value pair" in {
      val map = FixedSizeMap((1, "Garfield"), (2, "Odie"))
      map.get(2) should be(Some("Odie"))
      val smallerMap = map.remove(2)
      smallerMap.get(2) should be(None)
    }
  }

  "FixedSizeMap#toList" should {
    "return all key-value pairs in a map as a list of tuples" in {
      val map = FixedSizeMap((1, "Garfield"), (2, "Odie"))
      val list = map.toList
      list should contain((1, "Garfield"))
      list should contain((2, "Odie"))
    }

    "return an empty list when the map is empty" in {
      val emptyMap = FixedSizeMap[Int, String]()
      emptyMap.toList === Nil
    }
  }

  "FixedSizeMap.empty" should {
    "return an empty instance of the specified size" in {
      val map = FixedSizeMap.empty[Int, String](4)
      map.isEmpty should be(true)
    }
  }

  "FixedSizeMap.fromIterable" should {
    "construct a new map instance from an Iterable of key-value pairs" in {
      val iterable = Iterable((1, "Garfield"), (2, "Odie"))
      val map = FixedSizeMap.fromIterable(iterable)
      map.get(1) should be(Some("Garfield"))
      map.get(2) should be(Some("Odie"))
    }
  }

  "FixedSizeMap.apply" should {
    "construct a new map instance from key-value pairs" in {
      val map = FixedSizeMap(
        (1, "Garfield"),
        (2, "Odie")
      )
      map.get(1) should be(Some("Garfield"))
      map.get(2) should be(Some("Odie"))
    }
  }

}
