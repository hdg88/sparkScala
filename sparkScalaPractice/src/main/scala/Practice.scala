object Practice extends App {

  def nthElement[A](element: Int, ls: List[A]): A =
    if(element >= 0) ls(element)
    else throw new NoSuchElementException

  println(nthElement(4, List(1, 1, 2, 3, 5, 8)))

  def reverse[A](ls: List[A]): List[A] = ls.reverse

  println(reverse(List(1, 2, 3)))

  def isPalindrome[A](ls: List[A]): Boolean =
    ls == ls.reverse

  println(isPalindrome(List(1,2,3,2,1)))

  def removeAt[A](element: Int, ls: List[A]): (List[A],A) =
    if (element < 0) throw new NoSuchElementException
    else (element, ls) match{
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (t,e) = removeAt(element - 1, ls.tail)
        (ls.head :: t, e)
      }
    }

  println(removeAt(1, List(1,2,3,4)))

  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  def isCoPrime(num1: Int, num2: Int): Boolean =
    if (gcd(num1, num2) == 1) true
    else false

  println (gcd(35, 15))
  println (isCoPrime(35,15))

}

