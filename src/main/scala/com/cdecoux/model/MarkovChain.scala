package com.cdecoux.model

class MarkovChain {
    // TODO evaluate helpfulness of instantiated Markov Chain object
}

object MarkovChain {

    /**
     * This is a helper function for generated a mapped matrix for counting transitions between the states
     * @param a Array of elements in which will be the key for the maps
     * @tparam T Type of the mapped keys
     * @return Two dimensional map / Matrix which each the cell [x][y] contains the counts where state x transitioned to state y.
     */
    private def generateTransitionCounts[T](a: Array[T]): Map[T, Map[T, Int]] = {
        val state_transitions = a.foldLeft(Map[T, Int]()) {
            (m, s) => m + (s -> 0)
        }

        a.foldLeft(Map[T, Map[T, Int]]()) {
            (m, s) => m + (s -> state_transitions)
        }
    }

    // TODO change from int to float for probabilities. Set here for compile
    def parseText(text_blob: String): Map[String, Map[String, Int]] ={

        val text_array = text_blob.split(" +")
        val text_array_distinct = text_array.distinct

        val transition_table = generateTransitionCounts[String](text_array.distinct)


        text_array.foldLeft((transition_table, ".")) {
            (t, word) => {
                val current_count = t._1(t._2)(word)
                val delta = (word -> (current_count + 1))

                val new_state_transitions = t._1(t._2) + delta
                val new_table = (t._2 -> new_state_transitions)
                val new_transition_table = t._1 + new_table

                (new_transition_table, word)
            }
        }._1

    }
}
