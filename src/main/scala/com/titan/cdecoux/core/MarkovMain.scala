package com.titan.cdecoux.core

import com.titan.cdecoux.core.util.Random

import scala.util.Sorting
import scala.runtime.ScalaRunTime._


object MarkovMain {

    def main(args: Array[String]): Unit = {

        val label = Array("A", "B", "C")
        val prob = Array(0.1, 0.4, 0.5)

        // Less than 0.1, A. Less than 0.5, B. Less than 1, C.
        val values = new Array[String](10000).map(_ => label(Random.selectFromDistribution(prob)))

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
}
