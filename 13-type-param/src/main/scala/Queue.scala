package com.tp

trait Queue[+T]:
  def head: T
  def tail: Queue[T]
  def enqueue[U >: T](x: U): Queue[U]

object Queue:
  def apply[T](xs: T*): Queue[T] = QueueImpl(xs.toList, Nil)

  private class QueueImpl[T](private val leading: List[T],
                    private val trailing: List[T]) extends Queue[T]:
    def mirror: QueueImpl[T] = if(leading.isEmpty) then QueueImpl(trailing.reverse, Nil) else this

    def head: T = mirror.leading.head

    def tail: QueueImpl[T] = 
      val q = mirror
      QueueImpl(q.leading.tail, q.trailing)

    def enqueue[U >: T](t: U): QueueImpl[U] = QueueImpl(leading, t::trailing)


@main def queueDemo = 
  val q:Queue[Int] = new Queue( 1, 2 ,3)
  val q4 = q.enqueue(4)
  println(q4)
  

