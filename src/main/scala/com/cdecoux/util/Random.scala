package com.cdecoux.util

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

object Random {

    def selectFromDistribution(probabilities: Array[Double]): Int = selectFromDistribution(probabilities.map(x => x.toFloat))

    /**
     * This function takes in an iterable object that pairs some object with a float value, which defines a custom random distribution for each object key
     * i.e Map(("a" -> 0.4f), ("b" -> 0.3f), ("c" -> 0.3f)) defines 3 Strings "a", "b", and "c" are selected with 40%, 30%, and 30% chance respectively
     *
     * @param m Iterable object (like a Map) which pairs objects with a custom probability
     * @tparam T Object type to be paired with the probabilities, and returned as selection
     * @throws RandomDistributionError Returned from error in selecting from distribution. Likely cause is if the probabilities don't add to 100%. Float values need to add to 1
     * @return The randomly selected object
     */
    @throws[RandomDistributionError]
    def selectFromDistribution[T](m: Map[T, Float]): T = {

        // Check for incorrect format for probability model
        if(m.values.sum != 1.0) throw new RandomDistributionError("Probability values summation is not 1.0.")


        // Generate a random float (0, 1.000]
        lazy val num: Float = {
            new Random().nextFloat() match {
                case x => if (x != 0) x else num
            }
        }

        // Define control flow for checking distribution and the random number
        @scala.annotation.tailrec
        def attempt(m: Iterator[(T, Float)], acc: Float, random: Float): Option[T] = {
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
     * This function takes in an array of floats that defines a custom random distribution.
     * i.e [0.1f, 0.5f, 0.4f] define 3 indexes, 0, 1, and 2 which are selected with 10%, 50%, and 40% chance respectively
     *
     * This is usually paired with a parallel array. The return index maps with the other returned array index. This function has no context of that array.
     * See also: selectFromDistribution[T](m: Map[T, Float]): T
     *
     * @param probabilities An array of floats that define different distribution probabilities.
     * @throws RandomDistributionError Error when calculating random distribution. Most likely cause is float values do not add up to 1
     * @return The index that was randomly selected
     */
    @throws[RandomDistributionError]
    def selectFromDistribution(probabilities: Array[Float]): Int = {

        // Check for incorrect format for probability model
        if(probabilities.sum != 1.0) throw new RandomDistributionError("Probability values summation is not 1.0.")


        // Generate a random float (0, 1.000]
        lazy val num: Float = {
            new Random().nextFloat() match {
                case x => if (x != 0) x else num
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
