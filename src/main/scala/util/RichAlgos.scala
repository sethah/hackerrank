package util

object implicits {
  trait Integral[T] {
    def mod(a: T, b: T): T
    def gcd(a: T, b: T): T = {
      if (b == 0) a
      else gcd(b, mod(a, b))
    }

    def divide(a: T, b: T): T

    def lcm(a: T, b: T)(implicit num: Numeric[T]): T = {
      num.times(a, divide(b, gcd(a, b)))
    }
  }

  implicit object IntIntegral extends Integral[Int] {
    def mod(a: Int, b: Int): Int = a % b
    def divide(a: Int, b: Int): Int = a / b
  }

  implicit object LongIntegral extends Integral[Long] {
    def mod(a: Long, b: Long): Long = a % b
    def divide(a: Long, b: Long): Long = a / b
  }

  implicit class IntegralOps[T: Numeric](lhs: T)(implicit integral: Integral[T]) {
    def mod(rhs: T): T = integral.mod(lhs, rhs)
    def gcd(rhs: T): T = integral.gcd(lhs, rhs)
    def lcm(rhs: T): T = integral.lcm(lhs, rhs)
  }
}
