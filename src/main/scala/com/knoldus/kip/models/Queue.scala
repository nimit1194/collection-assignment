package com.knoldus.kip.models

class Queue(queueWithList: List[Int]) {

  def enqueue(x: Int): Queue = {
    val newQ = x :: queueWithList.reverse

    new Queue(newQ.reverse)
  }

  def dequeue: Queue = {
    new Queue(queueWithList.tail)

  }
}
