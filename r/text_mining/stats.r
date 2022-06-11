library(dplyr)

fnGetBasicStats <- function(dfInSentences){
  
  numParagraphs <- nrow(dfInSentences %>% select(paragraph_num) %>% distinct())
  numSentences <- nrow(dfInSentences)
  
  dfWords <- dfInSentences %>% unnest_tokens(word, sentence)
  dfWordsPerSentence <- dfWords %>% count(sentence_num)
  
  numWords <- sum(dfWordsPerSentence$n)
  numUniqueWords <- length(unique(dfWords$word))
  maxWordPerSentence <- max(dfWordsPerSentence$n)
  avgWordPerSentence <- mean(dfWordsPerSentence$n)
  sdWordPerSentence <- sd(dfWordsPerSentence$n)
  
  maxWordLen <- max(str_length(dfWords$word))
  avgWordLen <- mean(str_length(dfWords$word))
  sdWordLen <- sd(str_length(dfWords$word))
  
  tibble(num_paragraphs = numParagraphs, num_sentences=numSentences, num_words=numWords,
         num_unique_words=numUniqueWords,
         max_word_sentence=maxWordPerSentence, avg_word_sentence=avgWordPerSentence, 
         sd_word_sentence=sdWordPerSentence,
         max_word_len=maxWordLen, avg_word_len=avgWordLen, sd_word_len=sdWordLen)
}