## Write a program that checks whether an integer is a palindrome.
## For example, 121 is a palindrome, as well as 888. 678 is not a palindrome.
## Do not convert the integer into a string.

idPalindrome <- function(integer){

  length <- floor(log10(abs(integer))) + 1

  ## create array from integer
  integer_array <- numeric(length)

  for (i in 1:length){

     integer_array[i] <- substr(integer, i, i)

  }

## create data frame
test <- data.frame(forward = integer_array, backward = rev(integer_array))

test$binary <- ifelse(test$forward == test$backward,0,1)

test <- sum(test$binary)

## run logic test
if (test > 0){return('not palindrome')}
else if (test == 0){return('palindrome')}
else {return('broken logic')}

}
idPalindrome(300) ## should return not palindrome
idPalindrome(303) ## should return palindrome
idPalindrome(40404) ## should return palindrome
