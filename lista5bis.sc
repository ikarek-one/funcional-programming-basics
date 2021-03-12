
//zadanie 1
def lrepeat [A] (k: Int, lxs: LazyList[A]) :LazyList[A] = {
  def rep1 (n:Int, lista:LazyList[A]) :LazyList[A]  =
    (n, lista) match {
      case (_, LazyList()) => LazyList()
      case (0, LazyList.cons(hd, tl)) => LazyList.cons(hd, rep1(k-1, tl))
      case (_, LazyList.cons(hd, tl)) => LazyList.cons(hd, rep1(n - 1, lista))
    }
  if(k>0) rep1 (k-1, lxs) else throw new IllegalArgumentException
}


val ll1 = LazyList(11,22,33,44)
lrepeat(2, ll1).toList == List(11, 11, 22, 22, 33, 33, 44, 44)

val ll2 = LazyList('x','y','z')
lrepeat(3,ll2).toList == List('x','x','x','y','y','y','z','z','z')

val ll3 = LazyList.from(5)
lrepeat(3,ll3).take(12) == List(5,5,5,6,6,6,7,7,7,8,8,8)


//zadanie 2
val lfib = {
  def fib2 (a:Int, b:Int): LazyList[Int]  = {
    LazyList.cons(a, fib2(b,a+b))
  }
  fib2(0,1)
}

lfib.take(12).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89)