library(tidyverse, quietly=TRUE)
load(url("https://github.com/brianlukoff/sta235-labs/raw/main/sta235.Rdata"))

check_numeric <- function(user_answer, target_value, tolerance) {
  return(abs(user_answer - target_value) <= tolerance)
}

check_multiple_choice <- function(user_answer, target_value) {
  return(trimws(tolower(user_answer)) == trim(tolower(target_value)))
}

# Lab 1 - Inference for regression

lab_1_question_1 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "C")) {
    cat("That's it!")
  } else {
    cat("That's not right -- Make sure you are reading the right line of the output and think about what confidence intervals mean.")
  }
}

lab_1_question_2 <- function(user_answer) {
  if (check_numeric(user_answer, 4.06477, 0.01)) {
    cat("You got it!")
  } else {
    cat("That's not it -- fill in the blanks using the right slope and intercept from the model.")
  }
}

lab_1_question_3 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "A")) {
    cat("Correct!")
  } else {
    cat("Be careful how you are interpreting R^2.")
  }
}

lab_1_question_4 <- function(user_answer) {
  if (check_numeric(user_answer, 2*0.5448611, 0.01)) {
    cat("You got it! If you used this model to predict evaluation scores, then 95% of the predictions would be accurate to within +/- 1.10 points.")
  } else {
    cat("Remember that the mean is 0 and the SD is 0.55.")
  }
}

popeqn <- function(n, e){
  b <- sample(profs$beauty, n, replace=TRUE)
  data.frame(beauty=b,
             eval=(pmin(pmax(4+b*.133 + rnorm(n,0,e),1),5)),
             model=paste0('n=',sprintf("%04d",n))
  )
}


explore_se <- function(my_se){
  newdf <- NULL
  for (i in c(10,100,500)){
    newdf <- rbind(newdf, popeqn(i,my_se))
  }
  ggplot(newdf)+
    geom_point(aes(x=beauty, y=eval))+
    geom_smooth(aes(x=beauty, y=eval), method='lm')+
    facet_wrap(~model)
}


