// zadanie 1
def flatten [A] (xss: List[List[A]]): List[A] = {
  if (xss == List()) List()
  else xss.head ::: flatten(xss.tail)
}

flatten(List(List(1,2,9),List(3,5),List(7),List(9,1))) == List(1, 2, 9, 3, 5, 7, 9, 1)
flatten (List(List(5,6), List(1,2,3))) == List(5, 6, 1, 2, 3)
flatten (List(List("Ala", "ma", "kota"))) == List("Ala", "ma", "kota")
flatten(Nil) == List()



//zadanie 2
def count [A] (x: A, xs: List[A]): Int = {
  if (xs.length == 0) 0
  else if (xs.head == x) count(x, xs.tail) + 1
  else count(x, xs.tail)
}

count ('a', List('a', 'l', 'a', 'a', 'a')) == 4
count (34, (19 :: 25 :: 34 :: 69 :: 34 :: Nil)) == 2
count (7, List(1, 2, 7, 99, 7, 31, 7, 7, 7)) == 5
count (0, Nil) == 0
count (Nil, List(1, 2, 99, 31)) == 0



//zadanie 3
def replicate [A] (x: A, n: Int): List[A] = {
  if(n <= 0) Nil //List[A]()
  else  List(x)  ::: replicate(x, n-1) : List[A]
}

replicate ("la",3) == List("la", "la", "la")
replicate (7,0) == List()
replicate (1023, 3) == List(1023,1023,1023)



//zadanie 4
def sqrList (xs: List[Int]): List[Int] = {
  if (xs == List()) xs
  else List(xs.head * xs.head) ::: sqrList(xs.tail) : List[Int]
}

sqrList (List(1,2,3,-4)) == List(1, 4, 9, 16)
sqrList(List()) == List()
sqrList(List(-7, 7, 1, 0, 5)) == List(49, 49, 1, 0, 25)



//zadanie 5
def odwrocListe [A] (lista: List[A]) : List[A] = {
  if(lista.length == 0) lista
  else odwrocListe(lista.tail) ::: List(lista.head) : List[A]
}

def palindrome [A] (xs: List[A]): Boolean = (xs == odwrocListe(xs))

palindrome (List('a', 'l', 'a')) == true
//zakladam, ze listy 0- i 1-elementowe sa palindromami
palindrome(List()) == true
palindrome(List(1, 4, 8, 9, 7, 9, 8, 4, 1)) == true
palindrome(List(1, 2, 3, 1)) == false
palindrome(List(1, 2, 2, 1)) == true



//zadanie 6
def listLength [A](xs: List[A]): Int = {
  if(xs == List()) 0
  else listLength(xs.tail) + 1
}

listLength(List(11, 22 , 33, 44, 55, 66, 77)) == 7
listLength(List(-3, -3, -3, -3)) == 4
listLength(List()) == 0
listLength(List("a", "a", "w")) == 3

