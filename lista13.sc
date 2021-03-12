
// zadanie 1
class MyPair[A,B] (var fst:A, var snd:B) {
  override def toString: String = s"(${fst.toString}, ${snd.toString})"
}

val mp1 = new MyPair[Int, Boolean](120, true)
//val mp1: MyPair[Int,Boolean] = (120, true)
mp1.fst = 155
println(mp1.toString)



//zadanie 3
class Pracownik private (val nazwisko :String){
  private var zwolniony = false

  def zwolnij(): Unit = {
    Pracownik.iluPracownikow -= 1
    zwolniony = true
  }

  override def toString: String = s"Pracownik: (nazwisko: $nazwisko, czy zwolniony? : $zwolniony)"
}


object Pracownik {
  private var iluPracownikow = 0

  def apply(nazwisko: String): Pracownik = {
    iluPracownikow += 1
    new Pracownik(nazwisko)
  }

  def liczbaPracownikow = iluPracownikow
}

object PracownikTester {
  def main(args: Array[String]): Unit = {
    val p1 = Pracownik("Kowalska")
    val p2 = Pracownik("Nowak")
    val p3 = Pracownik("Sobieski")

    println(Pracownik.liczbaPracownikow == 3)

    p2.zwolnij()

    println(Pracownik.liczbaPracownikow == 2)
    println(p1.toString)
    println(p2.toString)

  }
}

PracownikTester.main(null)



// zadanie 5
import scala.collection.mutable

def wordCounter (text: String) :scala.collection.mutable.Map[String, Int] = {
  val separated = text.split(" ")
  val map = mutable.Map[String, Int]()

  separated.foreach(
    word => map.updateWith(word) {          //opt => opt match
      case Some(value) => Option(value + 1)
      case None => Option(1)
    } )

  map
}


val testText1 = "siedem siedem siedem trzy trzy trzy siedem siedem siedem siedem dwa jeden dwa"
val testMap1 = wordCounter(testText1)

testMap1.get("siedem") == Option(7)
testMap1.get("trzy") == Option(3)
testMap1.get("dwa") == Option(2)
testMap1.get("jeden") == Option(1)

val testMapEmpty = wordCounter("")
testMapEmpty.get("nieMaTakiegoWyrazu") == None



