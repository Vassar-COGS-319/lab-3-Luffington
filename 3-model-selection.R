# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 

set.seed(12604)

rw.model.result <- random.walk.model(1000, drift=0.012, sdrw=0.3, criterion = 4.8)
rw.model.result %>% group_by(correct) %>% summarize(mean.rt = mean(rt))
mean(rw.model.result$correct)

acc.model.result <- accumulator.model(1000, rate.1 = 85, rate.2 = 91, criterion = 3)
acc.model.result %>% group_by(correct) %>% summarize(mean.rt = mean(rt))
mean(acc.model.result$correct)



#NON-FUNCTIONAL CODE BEGINS HERE


#random-walk-model code(edited):

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

#The number of tests should be the same as the number of iterations it took for the criterion to be met
initial.test <- random.walk.model(internal_evidence_interval)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

#accumulator-model code (edited):

#I included the average response speeds as substitute values for the rates from the second model with the reasoning that 
#they represent the rate of response to both correct and incorrect responses. The criterion has been reset to
#0.8 because this is the accuracy level that must be acheived to maintain consistency between the model and the
#actual result. 

accumulator.model <- function(samples, rate.1=250, rate.2=246, criterion=0.8){
  
  ##established a counter to keep track of the iterations
  ##most of the rest of this code can be repurposed with the new values?
  
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

#Number of trials edited for the same reasons as in the model above
initial.test <- accumulator.model(iterations)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

#END NON-FUNCTIONAL CODE


# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:


# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.

layout(matrix(1:4, nrow=2, byrow=T))
hist((rw.model.result %>% filter(correct==TRUE))$rt, breaks=seq(0, 2000, 100), main="RW Model, correct", xlab="RT")
hist((rw.model.result %>% filter(correct==FALSE))$rt, breaks=seq(0, 2000, 100), main="RW Model, incorrect", xlab="RT")
hist((acc.model.result %>% filter(correct==TRUE))$rt, breaks=seq(0, 2000, 10), main="ACC Model, correct", xlab="RT")
hist((acc.model.result %>% filter(correct==FALSE))$rt, breaks=seq(0, 2000, 10), main="ACC Model, incorrect", xlab="RT")






rw.model.result %>% group_by(correct) %>% summarize(SD.rt = sd(rt))
acc.model.result %>% group_by(correct) %>% summarize(sd.rt = sd(rt))


