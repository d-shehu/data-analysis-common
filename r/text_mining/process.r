library(dplyr)
library(rvest)
library(tokenizers)
library(tidyr)
library(tidytext)

# This is essentially a fast tokenizer that uses tidy text mining approach
# to quickly parse common terms, trigrams, etc. Since Semantics parsing is
# more time consuming, we will make that a 2nd stage of processing and define
# it separately. 

# Tokenize paragraphs into sentences using tokenizer package
fnGetSentencesFromText <- function(dfParagraphs){
    dfParagraphs %>% 
        mutate(sentence = tokenize_sentences(text)) %>% 
        select(-text) %>% 
        unnest(sentence) %>% 
        mutate(sentence_num = row_number())
}

# Sanitize the sentences
fnGetSanitizedSentences <- function(dfSentences){
    # Unnest and remove stop words and then recombine into sentences
    dfSentences %>% 
        unnest_tokens(word, sentence, to_lower=TRUE) %>%
        # Remove single digit numbers
        filter(!grepl("^\\d$", word)) %>%
        anti_join(stop_words) %>% 
        group_by(paragraph_num, sentence_num) %>%
        summarize(sentence=str_c(word, collapse=" "), .groups="keep") %>%
        ungroup()
}

# Tokenize sanitied sentences into words. Sanitized sentences already
# have stop words removed and are lower case.
fnGetTerms <- function(dfSanitizedSentences){
    dfSanitizedSentences %>% 
        unnest_tokens(word, sentence) %>%
        select(word) %>%
        group_by(word) %>%
        summarize(n=n()) %>%
        arrange(desc(n)) %>%
        mutate(tf = n/sum(n)) %>%
        mutate(rank  = rank(-n, ties.method = "min"))
}

fnGetngramDataFrame <- function(dfSanitizedSentences, n){
    cWords <- sapply(1:n, function(x) paste0("w", x))
    dfSanitizedSentences %>% 
        unnest_tokens(trigram, sentence, token = "ngrams", n = n) %>%
        separate(trigram, cWords, sep=" ") %>%
        filter(!is.na(!!as.symbol(cWords))) %>%
        unite(trigram, cWords, sep=" ") %>%
        count(trigram, sort = TRUE)
}