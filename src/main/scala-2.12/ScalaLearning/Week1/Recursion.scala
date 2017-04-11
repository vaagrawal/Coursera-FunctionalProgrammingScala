
object Recursion extends App {

  def squareRoot(x: Double) : Double = {

    def squareRootItr(guess: Double): Double = {
      if (isBetterGuess(guess)) guess
      else squareRootItr(improveGuess(guess))
    }

    def isGoodGuess(guess: Double): Boolean = {
      Math.abs(guess * guess - x) < 0.001
    }

    // This is better beacuse this solves the issue with small and very large numbers.
    // Previously, the abs function checks the difference and compares to epsilon where the issue was that the consecutive absolute
    // values had passed the epsilon and thus it was an unending loop
    def isBetterGuess(guess: Double): Boolean = {
      Math.abs(x / guess - guess) / guess < 0.00001
    }

    def improveGuess(guess: Double): Double = {
      (guess + x / guess) / 2
    }
    squareRootItr(1.0)

  }
 /* println(squareRoot(2))
  println(squareRoot(4))
  println(squareRoot(0.001))
  println(squareRoot(0.1e-20))
  println(squareRoot(1.0e20))
  println(squareRoot(1.0e60))
  */
  def tailRecursiveFactorial (x: BigInt) : BigInt = {
    def loop(acc: BigInt, x: BigInt) : BigInt =
      if(x==0) acc
      else loop(acc*x, x-1)
    loop(1,x)
  }
  println(tailRecursiveFactorial(4))


}