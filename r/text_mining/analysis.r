library(igraph)
library(ggraph)
library(tidytext)
library(widyr)

# Calculate the correlation of words based on either the paragraph or sentence.
# Either might be useful depending on the density of the text.
# 
fnGetPairwiseCount <- function(dfSanitizedSentences, paragraphAsID){
    dfWords <- dfSanitizedSentences %>%
        unnest_tokens(word, sentence)

    if(paragraphAsID == TRUE){
        dfWords %>% pairwise_count(word, "paragraph_num", sort = TRUE, upper = FALSE)
    }
    else{
        dfWords %>% pairwise_count(word, "sentence_num", sort = TRUE, upper = FALSE)
    }
}

fnGetPairwiseCor <- function(dfSanitizedSentences, paragraphAsID){
    dfWords <- dfSanitizedSentences %>%
        unnest_tokens(word, sentence)

    if(paragraphAsID == TRUE){
        dfWords %>% pairwise_cor(word, "paragraph_num", sort = TRUE, upper = FALSE)
    }
    else{
        dfWords %>% pairwise_cor(word, "sentence_num", sort = TRUE, upper = FALSE)
    }
}

fnGetPairwiseStats <- function(dfSanitizedSentences, paragraphAsID){
    inner_join(fnGetPairwiseCount(dfSanitizedSentences, paragraphAsID), 
                fnGetPairwiseCor(dfSanitizedSentences, paragraphAsID),
                by = c("item 1", "item 2"))
}

# Expects a data frame with id, term, n and tf(frequency) and uses
# either frequency if number of documents is too small or tf-idf if
# greater than 12
fnGetTopKeywords <- function(dfDocTerms,nTopWords){
    dfDocTerms %>% 
        bind_tf_idf(id, word, n) %>% 
        arrange(id, desc(tf_idf)) %>% 
        group_by(id) %>% 
        slice(1:nTopWords)
}