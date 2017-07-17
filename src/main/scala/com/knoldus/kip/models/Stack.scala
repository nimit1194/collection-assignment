package com.knoldus.kip.models

trait Stack[Int] {
  val stackWithList: List[Int]

  def pop: List[Int] = {
    if (stackWithList.isEmpty) {
      throw new RuntimeException
    }
    else {
      stackWithList.tail
    }
  }

  def push(x: Int): List[Int] = {
    x :: stackWithList
  }
}