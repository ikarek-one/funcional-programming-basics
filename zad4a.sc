

def copy[A] (src :scala.collection.mutable.Seq[A], dest :scala.collection.mutable.Seq[A]): Unit = {
  var i :Int = 0

  src.foreach(elem =>
  { if(i < dest.length)
    dest.update(i, elem)
    i = i+1 } )
}

def copyTest1 = {
  var a1 = Array(1,2,3,4);
  var a2 = Array(3,5,7,7,7,7,7);

  copy(a1,a2)
  println("copyTest1: " + (a2.toList == List(1,2,3,4,7,7,7)))
}

def copyTest2: Unit = {
  var a1 = Array(1,2,3,4);
  var a2 = Array(3,5,7,7,7,7,7);

  copy(a2, a1)
  println("copyTest2: " + (a1.toList == List(3,5,7,7)))
}

copyTest1
copyTest2

/*
var a1 = Array(1,2,3,4);
var a2 = Array(3,5,7,7,7,7,7);

println(a1.toList);
println(a2.toList);

copy(a2, a1)

println(a1.toList);
println(a2.toList);

*/
