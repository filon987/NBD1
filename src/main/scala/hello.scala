import scala.annotation.tailrec
import scala.math.abs

object Demo {
  def main(args: Array[String]) {
    //Zadanie 1
    val tydzien = List("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela")
     var tydzien_s1:String = ""
     var tydzien_s2:String = ""
     var tydzien_s3:String = ""

    //a
     for (dzien <- tydzien) tydzien_s1 += dzien + "; "

    //b
     for (dzien <- tydzien if dzien.startsWith("p")) tydzien_s2 += dzien + "; "
     var i = 0
     val s = tydzien.length

    //c
     while (i < s) {
       tydzien_s3 += tydzien(i) + "; "
       i += 1
     }
     println(tydzien_s1)
     println(tydzien_s2)
     println(tydzien_s3)


    //zadanie 2
    //a
     def printList1(myList: List[Any]) {
      if (!myList.isEmpty) {
        print(myList.head + ", ")
        printList1(myList.tail)
      }
    }
    printList1(tydzien)
    println()

    //b (2 opcje; pierwszą znalazłem w internecie; do drugiej doszedłem sam idąc za analogią działania pierwszej)
    def printList2[A](lista: List[String]): List[String] = lista match {
      case head :: tail => printList2(tail) ::: List(head)
      case Nil => Nil
    }
    val tydzien_od_tylu: List[String] = printList2(tydzien)
    println(tydzien_od_tylu)

    def printList2_1(myList: List[Any]) {
      if(! myList.isEmpty) {
        printList2_1(myList.tail)
        print(myList.head + ", ")
      }
    }
    printList2_1(tydzien)
    println()

    //Zadanie 3
    def tailRec(list: List[String]) = {
      @tailrec
      def rec(list: List[String], str: String): String = list match {
        case Nil => str
        case head :: tail => rec(tail, str + head + ", ")
      }
      rec(list, "")
    }

    println(tailRec(tydzien))

    //Zadanie 4
    //a
    val tydzien_1 = tydzien.foldLeft("")(_ + ", " + _).dropRight(0)
    println(tydzien_1)

    //b
    val tydzien_2 = tydzien.foldRight("")(_ + ", " + _)
    println(tydzien_2)

    //c

    //Zadanie 5
    val product_prices = Map("dlugopis" -> 2.29, "notatnik" -> 5.49, "kalkulator" -> 15.49)
    val product_prices_reduced = (product_prices.keys zip product_prices.values.map(_ * 0.9)).toMap

    println(product_prices)
    println(product_prices_reduced)

    //Zadanie 6
    def function(wiek:Int, imie:String, pelnoletni:Boolean): Unit = {
      println("Wiek: " + wiek + ", Imie: " + imie + ", Pełnoletni: " + pelnoletni)
    }

    function(12, "Filip", false)

    //Zadanie 7
    val dni_tygodnia = List("1", "poniedziale", "2", "wtorek", "3", "sroda", "4", "Czwartek", "5", "piatek", "6", "sobota", "7", "niedziela")

    def numerDnia(s: String): Option[Int] = {
      try {
        Some(Integer.parseInt(s))
      } catch {
        case _: Exception => None
      }
    }

    for(dzien <- dni_tygodnia){
      print(numerDnia(dzien) +", ")
    }
    println()

    //Zadanie 8
    def printList8(myList: List[Any]) {
      if(! myList.isEmpty) {
        if (myList.head != 0)
          print(myList.head + ", ")
        printList8(myList.tail)

      }
    }

    val lista8 = List(0, 0, 0, 0, 0, 1, 2, 3, 4, 5)

    printList8(lista8)
    println()

    //Zadanie 9
    def dodajJeden(lista: List[Int], n:Int = 1)= lista.map(_+n)

    println(dodajJeden(lista8))

    //Zadanie 10
    def inRange(lista: List[Double], n1:Int = -5, n2:Int = 12): List[Double] = {
      lista.filter(x => x >= n1).filter(x => x <= n2).map(abs(_)) //x => x.abs
    }

    val zadanie10 = List[Double](-11, 2, 3.14, -2.71, -4, -12, 55, 10, 11, 34, 18)
    println(inRange(zadanie10))

  }
}