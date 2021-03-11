//zadanie 2
def fib(n:Int):Int = {
  if(n<0) throw new IllegalArgumentException

  if (n==0) 0
  else if (n==1) 1
  else fib(n-1) + fib(n-2) :Int
}

fib(0) == 0
fib(1) == 1
fib(2) == 1
fib(3) == 2
fib(4) == 3
fib(5) == 5
fib(6) == 8
fib(42) == 267914296

val fibTail = (n:Int) => {
  if(n<0) throw new IllegalArgumentException

  def fibT (a:Int, a1:Int, counter:Int): Int = {
    val a2 = a + a1 :Int

    if (counter == 0) a :Int
    else fibT(a1, a2, counter-1) :Int
  }

  fibT(0,1,n)
}

fibTail(0) == 0
fibTail(1) == 1
fibTail(2) == 1
fibTail(3) == 2
fibTail(4) == 3
fibTail(5) == 5
fibTail(6) == 8
fibTail(42) == 267914296



//zadanie 4
val lis1 = List(-2, -1, 0, 1, 2)
val _ :: _ :: x1 :: _ = lis1
x1 == 0

val lis2 = List((1, 2), (0,1))
val List( (_, _),(x2, _)) = lis2
x2 == 0



// zadanie 5

def initSegment [A](xs: List[A], ys: List[A]): Boolean = {
  if(xs == List()) true
  else if (ys == List()) false
  else {
    val a :: b = xs
    val c :: d = ys

    if (a != c) false
    else initSegment(xs.tail, ys.tail)
  }
}:Boolean

initSegment(List(), List()) == true
initSegment(List(), List(1,2,3)) == true
initSegment(List(1,2,3), List()) == false
initSegment(List(6,5,4,1), List(6,5,4,1,2,3,4)) == true
initSegment(List(6,5,4,1),List(6,5,4,3,3,3,3)) == false
initSegment(List(6,5,4,1,2,3,4),List(6,5,4,1)) == false


//zadanie 6

def replaceNth [A](xs: List[A], n: Int, x: A): List[A] = {
  if (n==0) x :: xs.tail
  else if (xs.tail != List()) xs.head :: replaceNth(xs.tail, n-1, x)
  else throw new IllegalArgumentException
}

replaceNth(List('o','l','a', 'm', 'a', 'k', 'o', 't', 'a'), 1, 's') == List('o','s','a','m','a','k','o','t','a')
replaceNth(List(10,20,30),0,99) == List(99,20,30)
replaceNth(List('a','b','c'),2,'x') == List('a','b','x')

