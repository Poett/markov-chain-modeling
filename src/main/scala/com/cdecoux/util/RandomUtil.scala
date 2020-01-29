package com.cdecoux.util

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

object RandomUtil {

    /**
     * This function takes in an iterable object that pairs some object with a double value, which defines a custom random distribution for each object key
     * i.e Map(("a" -> 0.4), ("b" -> 0.3), ("c" -> 0.3)) defines 3 Strings "a", "b", and "c" are selected with 40%, 30%, and 30% chance respectively
     *
     * @param m Iterable object (like a Map) which pairs objects with a custom probability
     * @tparam T Object type to be paired with the probabilities, and returned as selection
     * @throws RandomDistributionError Returned from error in selecting from distribution. Likely cause is if the probabilities don't add to 100%. Double values need to add to 1
     * @return The randomly selected object
     */
    @throws[RandomDistributionError]
    def selectFromDistribution[T](m: Map[T, Double]): T = {

        // Check for incorrect format for probability model
        if(BigDecimal.decimal(m.values.sum).setScale(5, BigDecimal.RoundingMode.HALF_EVEN) != 1.0) throw new RandomDistributionError("Probability values summation is not 1.0.")


        // Generate a random double (0, 1.000]
        lazy val num: Double = {
            new Random().nextDouble() match {
                case x => if (x != 0) x else num
            }
        }

        // Define control flow for checking distribution and the random number
        @scala.annotation.tailrec
        def attempt(m: Iterator[(T, Double)], acc: Double, random: Double): Option[T] = {
            if(m.hasNext){
                m.next match {
                    case (k, v) => {
                        val limit = acc + v
                        if (random < limit) Some(k) else attempt(m, limit, random)
                    }
                }
            }
            else None
        }


        // Initiate control flow and evaluate return value or throw an error
        attempt(m.iterator, 0.0f, num).getOrElse(throw new RandomDistributionError("Failed to retrieve a random number."))
    }


    /**
     * This function takes in an array of doubles that defines a custom random distribution.
     * i.e [0.1, 0.5, 0.4] define 3 indexes, 0, 1, and 2 which are selected with 10%, 50%, and 40% chance respectively
     *
     * This is usually paired with a parallel array. The return index maps with the other returned array index. This function has no context of that array.
     * See also: selectFromDistribution[T](m: Map[T, Double]): T
     *
     * @param probabilities An array of doubles that define different distribution probabilities.
     * @throws RandomDistributionError Error when calculating random distribution. Most likely cause is double values do not add up to 1
     * @return The index that was randomly selected
     */
    @throws[RandomDistributionError]
    def selectFromDistribution(probabilities: Array[Double]): Int = {

        // Check for incorrect format for probability model
        if(probabilities.sum != 1.0) throw new RandomDistributionError("Probability values summation is not 1.0.")


        // Generate a random double (0, 1.000]
        lazy val num: Double = {
            new Random().nextDouble() match {
                case x => if (x != 0) x else num
            }
        }

        // Define control flow for checking distribution and the random number
        @tailrec
        def attempt(probabilities: Array[Double], index: Int, acc: Double, random: Double): Try[Any] = {
            Try {
                acc + probabilities(index)
            } match {
                case Success(x: Double) => if (num < x) Success(index) else attempt(probabilities, index + 1, x, random)
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
