/*

1. Comparison of strict vs. lazy evaluation
2. Demonstrations of LazyList
3. Examples showing performance benefits with expensive computations
4. Using lazy val for calculation
5. Recursive lazy sequences with the Fibonacci example
6. Short-circuiting operations with calling parameters

 */

object LazyEvaluationDemo {
  def main(args: Array[String]): Unit = {
    println("Strict against Lazy evaluation in Scala - Topic 1")

    // Example 1: Lists evaluation (strict vs. lazy)

    println("\nStrict evaluation of lists:")

    val strictList = List(1, 2, 3, 4, 5)
    println(s"Original list: $strictList")

    val doubledStrict = strictList.map(_ * 2)
    println(s"Doubled list (strict): $doubledStrict")

    println("\nLazy evaluation of lists:")

    val lazyNumbers = LazyList.from(1)
    println("Created a lazy list of all positive integers")

    val firstFiveLazy = lazyNumbers.take(5).toList
    println(s"Original list: $firstFiveLazy")

    val doubledLazy = lazyNumbers.map(_ * 2).take(5).toList
    println(s"Doubled original list : $doubledLazy")

    println("\nLazy evaluation with expensive computation:")

    def expensiveComputation(n: Int): Int = {
      println(s"Computing expensive result for $n...")
      Thread.sleep(500)
      n * n
    }

    println("Strict evaluation:")
    val strictResults = List(1, 2, 3, 4, 5).map(expensiveComputation)
    println(s"First result: ${strictResults.head}")

    println("\nLazy evaluation:")
    val lazyResults = LazyList.from(1).map(expensiveComputation)
    println(s"First result: ${lazyResults.head}")
    println(s"Second result: ${lazyResults.tail.head}")

    println("\nLazy val example:")

    println("Defining a lazy val for expensive computation...")
    lazy val expensiveValue = {
      println("Executing expensive calculation...")
      Thread.sleep(1000)
      42
    }

    println("Lazy val defined, but not yet calculated")
    println("Accessing the lazy val for the first time:")
    println(s"Result: $expensiveValue")

    println("Accessing the lazy val for the second time:")
    println(s"Result: $expensiveValue")

    println("\nRecursive lazy sequence example:")

    lazy val fibs: LazyList[BigInt] = {
      def fibsFrom(a: BigInt, b: BigInt): LazyList[BigInt] = {
        a #:: fibsFrom(b, a + b)
      }

      fibsFrom(0, 1)
    }

    println("First ten fibonacci numbers:")
    fibs.take(10).toList.foreach(println)

    println("\nShort-circuiting with lazy evaluation:")

    def checkConditions(
        condition1: => Boolean,
        condition2: => Boolean
    ): Boolean = {
      println("Evaluating first condition")
      if (!condition1) {
        println("First condition is false, short-circuiting")
        false
      } else {
        println("First condition is true, evaluating second condition")
        condition2
      }
    }

    val result = checkConditions(
      { println("Calculating condition 1..."); true },
      { println("Calculating condition 2..."); false }
    )

    println(s"Final result: $result")
  }
}
