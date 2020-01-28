package com.cdecoux.model

import com.cdecoux.model.MarkovChain._

object MarkovTextModel {

    // TODO change from int to float for probabilities. Set here for compile
    def parseText(text_blob: String): MarkovChain ={

        val text_array = text_blob.split(" +")

        val transition_table = generateEmptyTransitionCounts[String](text_array.distinct)


        val text_transition_counts = text_array.foldLeft((transition_table, ".")) {
            (t, word) => {
                val current_count = t._1(t._2)(word)
                val delta = (word -> (current_count + 1))

                val new_state_transitions = t._1(t._2) + delta
                val new_table = (t._2 -> new_state_transitions)
                val new_transition_table = t._1 + new_table

                (new_transition_table, word)
            }
        }._1

        chainFromTransitionCounts(text_transition_counts)

    }
}
