package com.cdecoux.model

import com.cdecoux.util.RandomUtil

class MarkovChain[T](val transition_map: Map[T, Map[T, Double]]) {



    /*
        TODO:
            Step functions
                - Output result from given input with randomness given its defined distributions of transitions
            Transformation operator functions
                - Need to be able to update from an existing chain, such as using + operator. +(): MarkovChain or something
     */

    def step(from: T): T = {
        RandomUtil.selectFromDistribution(transition_map(from))
    }

}

object MarkovChain {

    def apply[T](transition_map: Map[T, Map[T, Double]]): MarkovChain[T] = {
        new MarkovChain[T](transition_map)
    }

    /**
     * This is a helper function for generated a mapped matrix for counting transitions between the states
     * @param a Array of elements in which will be the key for the maps
     * @tparam T Type of the mapped keys
     * @return Two dimensional map / Matrix which each the cell [x][y] contains the counts where state x transitioned to state y.
     */
    def generateEmptyTransitionCounts[T](a: Array[T]): Map[T, Map[T, Int]] = {
        val state_transitions = a.foldLeft(Map[T, Int]()) {
            (m, s) => m + (s -> 0)
        }

        a.foldLeft(Map[T, Map[T, Int]]()) {
            (m, s) => m + (s -> state_transitions)
        }
    }


    def chainFromTransitionCounts[T](transition_map_counts: Map[T, Map[T, Int]]): MarkovChain[T] = {

        val transition_table = transition_map_counts map {
            case (k, m: Map[T, Int]) =>
                val total = m.values.sum
                (k, m map {
                    case (k: T, v: Int) =>
                        (k -> v.toDouble / total)

                })
        }
        MarkovChain[T](transition_table)
    }
}
