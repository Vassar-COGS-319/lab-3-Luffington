# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# drift is the drift rate (default value is 0)
# sdrw is the variability in the drift rate (default value is 0.3)
# criterion is the threshold for a response (default value is 3)

random.walk.model <- function(samples, drift=0, sdrw=0.3, criterion=3){

  
  
#set a threshold value
  drift <- 0
  sdrw <- 0.3
  criterion <- 3
  internal_evidence_interval <- 0
  #sample a null distribution and assign to new tag
  compare.to.criterion <- sample(rnorm(drift, sdrw))
  #if the compare.to.criterion value is less than the criterion, we want the function to continue running,
  #so we check them against each other
  while ((compare.to.criterion < criterion)||(compare.to.criterion > -criterion)){
  #add the value to the internal evidence interval 
  compare.to.criterion <- sum(z, internal.evidence.interval)
  #this keeps track of how many interations we've run through up to this point
  sum(internal_evidence_interval, 1)
  #If our compare value exceeds the criterion,m we want to break out of the while loop and return 
  #the number of trials it took to get here
  else 
    return(internal_evidence_interval)
    
  }
  #not sure how to incorporate this piece of the function, must be critical
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}

# test the model ####

# if the model is working correctly, then the line below should generate a data frame with 
# 1000 samples and about half of the samples should be correct. the average rt will probably
# be around 112, but might vary from that by a bit.

initial.test <- random.walk.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

# we can use dplyr to filter the data and visualize the correct and incorrect RT distributions

library(dplyr)

correct.data <- initial.test %>% filter(correct==TRUE)
incorrect.data <- initial.test %>% filter(correct==FALSE)

hist(correct.data$rt)
hist(incorrect.data$rt)
