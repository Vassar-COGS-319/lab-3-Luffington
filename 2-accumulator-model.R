# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct comlumn should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# rate.1 is the evidence accumulation rate for the correct response (default value is 40)
# rate.1 is the evidence accumulation rate for the incorrect response (default value is 40)
# criterion is the threshold for a response (default value is 3)

# one oddity: note that higher values for rate.1 and rate.2 will actually produce slower RTs.
# this is because the rate parameter is controlling the rate of decay of the exponential distribution,
# so faster rates mean that less evidence is likely to accumulate on each step. we could make
# these parameters more intuitive by taking 1/rate.1 and 1/rate.2 as the values to rexp().


#We want a function that takes four arguments and returns a data frame with two columns. 

accumulator.model <- function(samples, rate.1=40, rate.2=40, criterion=3){
  ##established a counter to keep track of the iterations

  ##accuracy and response time set to blank numeric arrays
  accuracy.array <- numeric()
  rt.array <- numeric()
  ##for loop to iterate through input number of samples
  for(i in 1:samples){
    ##evidence and response time set to 0
    evidence.1 <- 0
    evidence.2 <- 0
    rt <- 0
    ##while loop holds condition for evidence to exceed respective criterion
    while(evidence.1 < criterion && evidence.2 < criterion){
      evidence.1 <- evidence.1 + rexp(1, rate.1)
      evidence.2 <- evidence.2 +rexp(1, rate.2)
      ##adding more to rt
      rt <- rt + 1
    }
    rt.array[i] <- rt
    accuracy.array[i] <- (evidence.1 > evidence.2)
  }
  #Returning dataframe with both collected arrays
  
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  return(output)
  
    
  }
  
  
  
  
  
  
  ##NON-FUNCTIONAL CODE BEGINS HERE
  
  
  ##As before, with this code, my methodology for tackling the problem was scattered
##and not focused around function syntax
  
  iterations <- 0
  #accumulators to store the random values sampled from the exponental distribution
  evidence.accumulator(rate.1) <- 0
  evidence.accumulator(rate.2) <- 0
  #start a while loop that terminates when the threshold value is reaached
  while(evidence.accumulator < criterion){
  #sampling random values from an exponential distribution of the rates we set in the accumulator function
  random_values <- sample(rexp(rate.1, rate.2))
  #add the sampled value from each rate to the evidence accumulators using the sum function
  sum(evidence.accumulator(rate.1), random_values)
  sum(evidence.accumulator(rate.2), random_values)
  #add one to the iterations counter so we can see how many cycles it takes to reach the criterion value
  sum(iterations, 1)
  else
    return(output)
  print(iterations)
  
rt <- iterations
correct <- c(evidence.accumulator(rate.1), evidence.accumulator(rate2))
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  }
  return(output)
}

##BACK TO NORMALCY


# test the model ####

# if the model is working correctly, then the line below should generate a data frame with 
# 1000 samples and about half of the samples should be correct. the average rt will probably
# be around 112, but might vary from that by a bit.

initial.test <- accumulator.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

# we can use dplyr to filter the data and visualize the correct and incorrect RT distributions

library(dplyr)

correct.data <- initial.test %>% filter(correct==TRUE)
incorrect.data <- initial.test %>% filter(correct==FALSE)

hist(correct.data$rt)
hist(incorrect.data$rt)
