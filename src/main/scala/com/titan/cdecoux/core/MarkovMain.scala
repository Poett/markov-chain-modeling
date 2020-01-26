package com.titan.cdecoux.core

import scala.annotation.tailrec
import scala.util.{Failure, Random, Sorting, Success, Try}
import scala.runtime.ScalaRunTime._


object MarkovMain {

    def main(args: Array[String]): Unit = {

        val model = (
          (0.4, 0.6),
          (0.3, 0.7)
        )


        val label = Array("A", "B", "C")
        val prob = Array(0.1, 0.4, 0.5)

        // Less than 0.1, A. Less than 0.5, B. Less than 1, C.
        val values = new Array[String](10000).map(x => label(random(prob)))

        Sorting.quickSort(values)
        println(stringOf(values))

        val counts = values.groupBy(identity).transform( (x, a) => a.length)

        val total = counts.foldLeft(0)(_ + _._2)
        val a_percentage: Float = counts("A") / total.toFloat
        val b_percentage: Float = counts("B") / total.toFloat
        val c_percentage: Float = counts("C") / total.toFloat


        println(s"Total: $total")
        println(s"A Counts | Percentage: ${counts("A")} | $a_percentage")
        println(s"B Counts | Percentage: ${counts("B")} | $b_percentage")
        println(s"C Counts | Percentage: ${counts("C")} | $c_percentage")



    }




    def random(probabilities: Array[Double]): Int = random(probabilities.map(x => x.toFloat))

    @throws[RandomDistributionError]
    def random(probabilities: Array[Float]): Int = {

        // Check for incorrect format for probability model
        if(probabilities.sum != 1.0) throw new RandomDistributionError("Probability values summation is not 1.0.")


        // Generate a random float (0, 1.000]
        lazy val num: Float = {
            new Random().nextFloat() match {
                case x: Float => if (x != 0) x else num
            }
        }

        // Define control flow for checking distribution and the random number
        @tailrec
        def attempt(probabilities: Array[Float], index: Int, acc: Float, random: Float): Try[Any] = {
            Try {
                acc + probabilities(index)
            } match {
                case Success(x: Float) => if (num < x) Success(index) else attempt(probabilities, index + 1, x, random)
                case Failure(exception) =>
                    println(exception.getMessage)
                    Failure(new RandomDistributionError("Failed to retrieve a random number.", exception.getCause))
            }
        }

        // Initiate control flow and evaluate return value
        attempt(probabilities, 0, 0, num).get match {
            case x: Int => x
            case e: Exception => throw e
        }
    }

    class RandomDistributionError(msg: String) extends Exception(msg: String) {
        def this(msg: String, cause: Throwable) {
            this(msg)
            initCause(cause)
        }
    }
}
