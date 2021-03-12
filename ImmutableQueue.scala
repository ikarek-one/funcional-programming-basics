package zad3b

class EmptyException(message :String) extends Exception(message)

class ImmutableQueue[+A] (private val list1 :List[A], private val list2: List[A]) {

  def enqueue[B >: A](elem :B): ImmutableQueue[B] =
    (list1, list2) match {
      case (List(), ys) => new ImmutableQueue[B](List(elem), ys)
      case (xs, ys) => new ImmutableQueue[B](xs, elem::ys)
    }

  def dequeue: ImmutableQueue[A] =
    (list1, list2) match {
      case (List(), List()) => new ImmutableQueue[A](List(), List())
      case (_ :: Nil, xs) => new ImmutableQueue[A](xs.reverse, List())
      case (_ :: t, xs) => new  ImmutableQueue[A](t, xs)
    }

    def first: A =
      if (isEmpty) throw new EmptyException("pusta kolejka!")
      else list1.head

    def isEmpty: Boolean =
      (list1, list2) == (List(), List())
}

object ImmutableQueue {
  def empty[A] = new ImmutableQueue[A](List(),List())
  def apply[A] (xs :A*) = new ImmutableQueue[A](xs.toList.reverse, List())
}

object ImmutableQueueTests {
  def main(args: Array[String]): Unit = {
    val q1 = ImmutableQueue.apply(90,91,92,93);
    println("Testy ImmutableQueue")
    println(q1.first == 93)
    println(q1.dequeue.dequeue.dequeue.first == 90)
    println(q1.dequeue.dequeue.dequeue.isEmpty == false)
    println(q1.dequeue.dequeue.dequeue.dequeue.isEmpty == true)
    println(ImmutableQueue.empty.isEmpty == true)
    println(q1.dequeue.dequeue.dequeue.dequeue.dequeue.dequeue.dequeue.isEmpty == true)
    println("Koniec testów ImmutableQueue")
  }
}



/*
module TLQ: QUEUE_FUN =
  struct
    type 'a t =  'a list * 'a list

    exception Empty of string


    let empty() = ([], [])


    let enqueue (e,q) =
      match q with
        ([], ys) -> ([e], ys)
      | (xs1, xs2) -> (xs1, e::xs2)


    let dequeue q  =
      match q  with
       ([], []) -> q
      | ([_] , y) -> (List.rev y, [])
      | (_::t, xs) -> (t, xs)


    let first q =
      match check q  with
         ([], []) -> raise (Empty "pusta kolejka TLQ!")
      |  (xh::xt, _) -> xh


    let isEmpty q  =
      q =  ([], [])

  end;;

 */