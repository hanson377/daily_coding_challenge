# A Collatz sequence in mathematics can be defined as follows. Starting with any positive integer:

## if n is even, the next number in the sequence is n / 2
## if n is odd, the next number in the sequence is 3n + 1
## It is conjectured that every such sequence eventually reaches the number 1. Test this conjecture.

##Bonus: What input n <= 100000 gives the longest sequence?


library(dplyr)
library(ggplot2)

determineOddOrEven <- function(integer){

  if (integer/2 == round(integer/2,digits=0)){classification = 'even'}
  else {classification = 'odd'}

  return(classification)

}
determineOddOrEven(11)

runSequence <- function(starting_integer){

n <- starting_integer
counter <- 0

while (n != 1){

  if (determineOddOrEven(n) == 'even'){

    #print(n)
    n = n/2
    counter = sum(counter,1)
  }
    else if (determineOddOrEven(n) == 'odd'){

    #print(n)
    n = 3*n+1
    counter = sum(counter,1)
  }
}
return(counter)
}


##
max_n <- 100000

sequence_count <- numeric(max_n)
for (i in 1:max_n){

  sequence_count[i] <- runSequence(i)

}
starting_integer <- seq(1,max_n,1)

result <- data.frame(starting_integer,sequence_count)


## we find that the longest sequence starts with the integer 77031
## just for fun, lets plot this entire sequence

recordSequence <- function(starting_integer){

n <- starting_integer

list_of_n <- c()

while (n != 1){

  if (determineOddOrEven(n) == 'even'){

    print(n)
    n = n/2

    list_of_n <- c(list_of_n,n)

  }
    else if (determineOddOrEven(n) == 'odd'){

    print(n)
    n = 3*n+1

    list_of_n <- c(list_of_n,n)

  }
}
values <- list_of_n
results <- data.frame(starting_integer,sequence_n = seq(1,length(values),1), values)

return(results)
}

##
values <- recordSequence(77031)
ggplot(results,aes(x=sequence_n,y=values)) + geom_line()



## for fun, lets now write a loop that will calculate 100 walks using a random starting point and plot the results
##
final <- NA
for (i in 1:1000){

temp <- recordSequence(round(runif(1,1,25000),digits=0))
final <- rbind(temp,final)
}
ggplot(final,aes(x=sequence_n,y=values,colour=factor(starting_integer))) +
  geom_line(alpha=0.2) +
  theme(legend.position='none')
