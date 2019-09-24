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

random.walk.model <- function(samples, drift=0.012, sdrw=0.3, criterion=4.9){
  accuracy <- logical()
  rt <- rep(0, samples)
  for(i in 1:samples) {
    evidence <- 0
    while(abs(evidence)<criterion){
      trial <- rnorm(1, drift, sdrw)
      evidence <- evidence + trial
      rt[i] <- rt[i] + 1
    }
    if(evidence>criterion) {
      accuracy[i] <- TRUE
    } else {
      accuracy[i] <- FALSE
    }
  }
  output <- data.frame(
    correct = accuracy,
    rt = rt
  )
  return(output)
}

fit.1 <- random.walk.model(1000)
sum(fit.1$correct) / length(fit.1$correct)
correct.data.1 <- fit.1 %>% filter(correct==TRUE)
incorrect.data.1 <- fit.1 %>% filter(correct==FALSE)
mean(correct.data.1$rt)
mean(incorrect.data.1$rt)

#-------------------------------------------------------------------------------------------

accumulator.model <- function(samples, rate.1=63, rate.2=69, criterion=3){
  accuracy <- logical()
  rt <- rep(0, samples)
  for(i in 1:samples) {
    evidence.1 <- 0
    evidence.2 <- 0
    while(evidence.1<criterion || evidence.2<criterion) {
      trial.1 <- rexp(1, rate=rate.1)
      trial.2 <- rexp(1, rate=rate.2)
      evidence.1 <- trial.1 + evidence.1
      evidence.2 <- trial.2 + evidence.2
      rt[i] <- rt[i] + 1
    }
    if(evidence.1>evidence.2) {
      accuracy[i] <- TRUE
    } else {
      accuracy[i] <- FALSE
    } 
  }
  output <- data.frame(
    correct = accuracy,
    rt = rt
  )
  return(output)
}

fit.2 <- accumulator.model(100)
sum(fit.2$correct) / length(fit.2$correct)
correct.data.2 <- fit.2 %>% filter(correct==TRUE)
incorrect.data.2 <- fit.2 %>% filter(correct==FALSE)
mean(correct.data.2$rt)
mean(incorrect.data.2$rt)
# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:


# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.
