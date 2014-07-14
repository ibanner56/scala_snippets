/**
 * Created by isaacbanner on 7/8/14.
 */

class Scott(name: String){
  def hello() = "Hi " + name
}

object Flatten{

  def flatten(list: List[Any]): List[Any] = {
    list match{
      case l if l.size == 0 => l
      case _ => list.head match{
        case l: List[Any] => flatten(l) ++ flatten(list.tail)
        case a: Any => List(a) ++ flatten(list.tail)
      }
    }
  }

  val aDomainError: PartialFunction[Int, String] = {
    case 1 => "In Domain"
  }

  val stillADomainError: PartialFunction[Int, String] = {
    case 2 => "Still in Domain"
  }

  val domainError = aDomainError orElse stillADomainError

  def id[T](x: T) = x

  val A = List("the", "a")
  val N = List("man", "woman", "ball", "table", "hat")
  val V = List("hit", "took", "saw", "liked")

  def randomElt(list: List[Any]) = list((Math.random()*list.length).toInt)
  def oneOf(list: List[String]) = List(randomElt(list))

  def sentence() = oneOf(A) ++ oneOf(N) ++ oneOf(V) ++ oneOf(A) ++ oneOf(N)

  def isMember(a: Any, list: List[Any]) = list.indexOf(a) != -1

  def isEmpty(list: List[Any]) = list.length == 0

  def isSubset[K](list1: List[K], list2: List[K]): Boolean = {
    list1 match{
      case l if l.length == 0 => true
      case _ => isMember(list1.head, list2) && isSubset(list1.tail, list2)
    }
  }

  def areEqualSets[K](list1: List[K], list2: List[K]): Boolean = {
    isSubset(list1, list2) && isSubset(list2, list1)
  }

  def union(list1: List[Int], list2: List[Int]): List[Int] = {
    list1 match{
      case l if l.length == 0 => list2
      case l if isSubset(l, list2) => list2
      case l if isSubset(list2, l) => l
      case l if isMember(l.head, list2) => union(l.tail, list2)
      case _ => (list1.head :: union(list1.tail, list2)).sortWith(_ < _)
    }
  }

  def intersect(list1: List[Int], list2: List[Int]): List[Int] = {
    list1 match{
      case l if l.length == 0 => l
      case l if isSubset(l, list2) => l
      case l if isSubset(list2, l) => list2
      case l if isMember(l.head, list2) => (l.head :: intersect(l.tail, list2)).sortWith(_ < _)
      case _ => intersect(list1.tail, list2)
    }
  }

  def difference(list1: List[Int], list2: List[Int]): List[Int] = {
    list1 match{
      case l if l.length == 0 => l
      case l if isSubset(l, list2) => List[Int]()
      case l if isMember(l.head, list2) => difference(l.tail, list2)
      case _ => list1.head :: difference(list1.tail, list2)
    }
  }

  def symmDiff(list1: List[Int], list2: List[Int]): List[Int] = {
    union(difference(list1, list2), difference(list2, list1))
  }

  def fib(i: Int): Int = {
    i match {
      case i if i <= 1 => i
      case _ => fib(i - 1) + fib(i - 2)
    }
  }

  def seive(list: List[Int]): List[Int] = {
    list match{
      case l if l.length == 0 => l
      case _ => list.head :: seive((for(i <- list.tail if i % list.head > 0) yield i))
    }
  }

  def primes(i: Int) = seive(for(j <- List.range(2, i + 1)) yield j)

  def primeFactors(i: Int) = for(x <- primes(i) if i % x == 0) yield x

  def relativePrime(i: Int, j: Int) = areEqualSets(union(primeFactors(i),
                                        primeFactors(j)), symmDiff(primeFactors(i),
                                         primeFactors(j)))

  def pythag(x: Int): List[Tuple3[Int, Int, Int]] = {
    pythagHelper(for(i <- List.range(1, x)) yield i)
  }

  def pythagHelper(list: List[Int]): List[Tuple3[Int, Int, Int]] = {

    list match{
      case l if l.length == 0 => List[Tuple3[Int, Int, Int]]()
      case _ => {
        val z = list.head + 1
        (for(x <- List.range(1, z); y <- List.range(x, z)
             if (math.pow(x, 2) + math.pow(y, 2) == math.pow(z, 2) &&
                  relativePrime(x, y)))
        yield ((x, y, z))) ++ pythagHelper(list.tail)
      }
    }
  }

  def main(args: Array[String]) {
    val newList = List(1, 2, List(3, List(4)), List(List(List(5))), 6, List(7, List(), List(8, 9)))

    val fList = flatten(newList)

    println(fList)

    // println(id("Hi:") + " " + id(5))

    // println(domainError(1))
    // println(domainError(2))
    // println(domainError(3))

    for(a <- 1 to 5) {
      val randSentence = sentence()
      randSentence.foreach(x => print(x + " "))
      println()
    }

    println()

    val list1 = List(1, 2, 3, 5)
    val list2 = List(1, 3, 4, 2)
    val list3 = List(1, 2, 3, 5)

    println(isSubset(list2, list1))

    println(areEqualSets(list1, list3))
    println(areEqualSets(list2, list3))

    union(list1, list2).foreach(x => print(x + " "))
    println()
    intersect(list1, list2).foreach(x => print(x + " "))
    println()

    union(list1, list2).foreach(x => {
      if(x < 5 && x != 3) print(x + " ") else print("[REDACTED] ")
    })
    println()

    print(fib(0))
    for(a <- 1 to 20){
      print(", ")
      print(fib(a))
    }
    println()

    val Scotty = new Scott("Scotty")
    val notScotty = new Scott("[Just Scott]")
    println(Scotty.hello())
    println(notScotty.hello())

    println()
    println(primes(47))

    println()
    println(pythag(25))
  }
}
