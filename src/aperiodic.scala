/**
 * Created by isaacbanner on 7/9/14.
 */
object aperiodic {

    ///////////////////////////////////////////////////////////////////////////
   ////////////////////////////// LISTS //////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////

  def last(list: List[Any]) = list(list.length - 1) // 1
  def penultimate(list: List[Any]) = list(list.length - 2) // 2
  def nth(i: Int, list: List[Any]) = list(i) // 3
  // Ok, we'll start doing these recursively...
  def length(list: List[Any]): Int = {
    list match{
      case Nil => 0
      case _ => 1 + length(list.tail)
    }
  } // 4
  def reverse[K](list: List[K]): List[K] = {
    list match{
      case Nil => List[K]()
      case _ => reverse(list.tail) ++ List(list.head)
    }
  } // 5
  def isPalindrome(list: List[Any]) = {
    palindromeHelper(list, reverse(list))
  } // 6
  def palindromeHelper(list1: List[Any], list2: List[Any]): Boolean = {
    list1 match{
      case Nil => true
      case l if l.head == list2.head => palindromeHelper(l.tail, list2.tail)
      case _ => false
    }
  } // 6
  def flatten(list: List[Any]): List[Any] = {
    list match{
      case l if l.size == 0 => l
      case _ => list.head match{
        case l: List[Any] => flatten(l) ++ flatten(list.tail)
        case a: Any => List(a) ++ flatten(list.tail)
      }
    }
  } // 7
  def compress[A](list: List[A]): List[A] = {
    list match{
      case l if length(l) <= 1 => l
      case l if l.head == l.tail.head => compress(l.tail)
      case _ => list.head :: compress(list.tail)
    }
  } // 8
  def pack[A](list: List[A]): List[List[A]] = {
    if(list.isEmpty) List(List())
    else {
      val (current, next) = list.span(_ == list.head)
      if (next == Nil) List(current)
      else current :: pack(next)
    }
  } // 9
  def encode[A](list: List[A]): List[Tuple2[Int, A]] = {
    if(list isEmpty) List()
    else {
      val(current, next) = list span {_ == list.head}
      if(next isEmpty) List((current.length, current.head))
      else (current.length, current.head) :: encode(next)
    }
  } // 10
  def decode[A](list: List[Tuple2[Int, A]]): List[A] = {
    if(list isEmpty) List()
    else {
      (for(i <- List.range(0, list.head._1)) yield list.head._2 ) ++ decode(list.tail)
    }
  } // 11
  def duplicateN[A](i: Int, list: List[A]): List[A] = {
    if(list isEmpty) List()
    else (for (j <- List.range(0, i)) yield list.head) ++ duplicateN(i, list.tail)
  } // 15
  def drop[A](i: Int, list: List[A]): List[A] = {
    for(j <- List.range(0, list.length) if (j+1) % i != 0) yield list(j)
  } // 16
  def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotate(nBounded + ls.length, ls) // Make negative rotations their positive counterparts.
    else (ls drop nBounded) ::: (ls take nBounded)
  } // 19
  def removeAt[A](i: Int, list: List[A]) = (list.take(i) ++ list.drop(i + 1), list(i)) // 20
  def insertAt[A](a: A, i: Int, list: List[A]) = list.take(i) ++ (a :: list.drop(i)) // 21
  def randomSelect[A](i: Int, list: List[A]): List[A] = {
    if(i == 0 || list.isEmpty) List()
    else {
      val next = removeAt((math.random * list.length).toInt, list)
      next._2 :: randomSelect(i - 1, next._1)
    }
  } // 23
  def lotto(count: Int, range: Int) = randomSelect(count, List.range(1, range)) // 24
  def randomPermute[A](list: List[A]): List[A] = randomSelect(length(list), list) // 25

    ///////////////////////////////////////////////////////////////////////////
   ////////////////////////////// MATH ///////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////

  def isPrime(num: Int) = (for(i <- List.range(2, math.sqrt(num).toInt + 1)
                               if num % i == 0) yield i) == Nil // 31
  def gcd(a: Int, b: Int): Int = {
    if(a == b) a
    else if(a < b) gcd(a, b-a)
    else gcd(a - b, b)
  } // 32
  def coprime(a: Int, b: Int) = gcd(a, b) == 1 // 33
  def totient(a: Int) = for(i <- List.range(1, a) if coprime(i, a)) yield i // 34
  def seive(list: List[Int]): List[Int] = {
    list match{
      case l if l.length == 0 => l
      case _ => list.head :: seive((for(i <- list.tail if i % list.head > 0) yield i))
    }
  } // 35
  def primes(i: Int) = seive(for(j <- List.range(2, i + 1)) yield j) // 35
  def primeFactors(i: Int) = for(x <- primes(i) if i % x == 0) yield x // 35
  def listPrimes(a: Int, b: Int) = for(i <- List.range(a, b + 1) if isPrime(i)) yield i
  def goldbach(a: Int) = for(x <- listPrimes(1, a); y <- listPrimes(x, a) if x + y == a) yield (x, y)


    ///////////////////////////////////////////////////////////////////////////
   ////////////////////////////// MAIN ///////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////

  def main(args: Array[String]) {
    /*
    val list = List(1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6)
    val pdrome = List(1, 2, 3, 2, 1)
    val chars = List('a, 'a, 'c, 'b, 'a, 'c, 'c, 'b, 'd, 'd, 'e, 'd)

    // 1
    println(last(list)) // 6
    // 2
    println(penultimate(list)) // 5
    // 3
    println(nth(3, list)) // 2
    // 4
    println(length(list)) // 12
    println(length(List())) // 0
    // 5
    println(reverse(list)) // List(6, 5, 5, 4, 4, 3, 3, 3, 2, 2, 1, 1)
    // 6
    println(isPalindrome(pdrome)) // true
    println(isPalindrome(list)) // false
    // 7 - See flatten.scala
    // 8
    println(compress(list)) // List(1, 2, 3, 4, 5, 6)
    // 9
    println(pack(list)) // List(List(1, 1), List(2, 2), List(3, 3, 3), List(4, 4), List(5, 5), List(6)
    // 10
    println(encode(list)) // List((2, 1), (2, 2), (3, 3), (2, 4), (2, 5), (1, 6)
    // 12
    println(decode(encode(chars))) // List('a, 'a, 'c, 'b, 'a, 'c, 'c, 'b, 'd, 'd, 'e, 'd)
    // 15
    println(duplicateN(2, pdrome)) // List(1, 1, 2, 2, 3, 3, 2, 2, 1, 1)
    // 16
    println(drop(3, chars)) // List('a, 'a, 'b, 'a, 'c, 'b, 'd, 'e)
    // 20
    println(removeAt(2, pdrome)) // List(1, 2, 2, 1)
    // 21
    println(insertAt('a, 3, pdrome)) // List(1, 2, 3, a, 2, 1)
    //23
    println(randomSelect(4, chars)) // List(4 random chars)
    //24
    println(lotto(6, 250))
    // 25
    println(randomPermute(chars))
    */

    // 31
    println(isPrime(67)) // true
    println(isPrime(25)) // false
    // 32
    println(gcd(36, 63)) // 9
    // 33
    println(coprime(7, 12)) // true
    // 34
    println(totient(35)) // List(bunch of even, odd and prime numbers not divisible by 5 or 7)
    // 39
    println(listPrimes(7, 31)) // List(7, 11, 13, 17, 19, 23, 29, 31)
    // 40
    println(goldbach(28))
  }
}
