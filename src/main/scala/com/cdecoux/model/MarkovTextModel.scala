package com.cdecoux.model

object MarkovTextModel {

    def parseText(text_blob: String): MarkovChain[String] ={

        val text_array = text_blob.split(" +")

        val transition_table = MarkovChain.generateEmptyTransitionCounts[String](text_array.distinct)


        val text_transition_counts = text_array.foldLeft((transition_table, ".")) {
            case ((table, prev_word), word) =>
                val current_count = table(prev_word)(word)
                val delta = (word -> (current_count + 1))
                val new_state_transitions = table(prev_word) + delta

                val new_transition_table = table + (prev_word -> new_state_transitions)

                (new_transition_table, word)
        }._1

        MarkovChain.chainFromTransitionCounts[String](text_transition_counts)

    }
}
