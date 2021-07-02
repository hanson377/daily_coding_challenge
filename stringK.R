# Given a string s and an integer k,
# break up the string into multiple lines such that each line has a length of k or less.
# You must break it up so that words don't break across lines. Each line has to have the maximum possible amount of words.
# If there's no way to break the text up, then return null.

# You can assume that there are no spaces at the ends of the string and that there is exactly one space between each word.

# For example, given the string "the quick brown fox jumps over the lazy dog" and k = 10,
# you should return: ["the quick", "brown fox", "jumps over", "the lazy", "dog"].
# No string in the list has a length of more than 10.

library(stringr)
library(dplyr)

sentence <- 'Let us give this a go; hopefully it will not be very difficult'

stringK <- function(s,k){

s_clean <- gsub("[^[:alnum:][:space:]]","",s) ## remove everything but spaces and alpha-numeric characters
length_of_string <- nchar(s_clean)
words <- str_count(s_clean, pattern = " ")+1
split_words <- str_split_fixed(sentence_clean, ' ', words)

if (length_of_string >= k) {

}
  else {return(NA)} ## return null if there is no way to break up the words

}
stringK(sentence,100)
stringK(sentence,50)


sentence <- 'Let us give this a go; hopefully it will not be very difficult, please dont let it be very difficult at all'
k <- 12

sentence_clean <- gsub("[^[:alnum:][:space:]]","",sentence)
words <- str_count(sentence_clean, pattern = " ")+1
list_words <- str_split_fixed(sentence_clean, ' ', words)
length(list_words)

finalDf <- NA

for (i in 1:length(list_words)) {

wordDf <- data.frame(word_number = i, word = list_words[[i]], characters = nchar(list_words[[i]]))
finalDf <- rbind(wordDf,finalDf)

}
finalDf <- finalDf %>% filter(is.na(word_number)==FALSE) %>% arrange(word_number) %>% mutate(cumulative_sum = cumsum(characters))
