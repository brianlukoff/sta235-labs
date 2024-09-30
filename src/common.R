library(tidyverse, quietly=TRUE)

# This is not a default package on Colab.
install.packages("ggfortify")
library(ggfortify, quietly=TRUE)

load(url("https://github.com/brianlukoff/sta235-labs/raw/main/sta235.Rdata"))

check_numeric <- function(user_answer, target_value, tolerance) {
  return(abs(user_answer - target_value) <= tolerance)
}

check_numeric_pair <- function(user_answer_1, target_value_1, user_answer_2, target_value_2, tolerance) {
  return(abs(user_answer_1 - target_value_1) <= tolerance && abs(user_answer_2 - target_value_2) <= tolerance)
}


check_multiple_choice <- function(user_answer, target_value) {
  return(trimws(tolower(user_answer)) == trimws(tolower(target_value)))
}

check_tf <- function(user_answer, target_value) {
  return(user_answer == target_value)
}

check_multiple_answer <- function(user_answer, target_value) {
  return(setequal(sort(str_split_1(gsub("[^A-Za-z]", "", toupper(user_answer)), "")),
                  sort(toupper(target_value))))
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

# Lab 2 - Multiple regression

lab_2_question_1a <- function(user_answer) {
  if (check_tf(user_answer, F)) {
    cat("Correct!")
  } else {
    cat("While this interpretation is mathematically OK, it doesn't really make sense because it talks about a 1-unit increase in AVG, which is unrealistic given that AVG is a proportion between 0 and 1.")
  }
}
lab_2_question_1b <- function(user_answer) {
  if (check_tf(user_answer, T)) {
    cat("Correct!")
  } else {
    cat("Try plugging two different numbers in for AVG that differ by 0.01 (for example, 0.5 and 0.51). How much does R.G change by?")
  }
}
lab_2_question_1c <- function(user_answer) {
  if (check_tf(user_answer, F)) {
    cat("Correct!")
  } else {
    cat("A large coefficient alone is not enough to determine whether it is statistically significant -- remember you would have to look at the confidence intervals for the coefficients too!")
  }
}
lab_2_question_1d <- function(user_answer) {
  if (check_tf(user_answer, T)) {
    cat("Correct!")
  } else {
    cat("Do the same analysis you did above for AVG.")
  }
}
lab_2_question_1e <- function(user_answer) {
  if (check_tf(user_answer, T)) {
    cat("Correct!")
  } else {
    cat("Look at the p-value or confidence interval for AVG.")
  }
}
lab_2_question_1f <- function(user_answer) {
  if (check_tf(user_answer, T)) {
    cat("Correct!")
  } else {
    cat("Compare the sizes of the cofficients!")
  }
}

lab_2_question_2 <- function(user_answer) {
  if (check_numeric(user_answer, 0.92, 0.01)) {
    cat("Correct!")
  } else {
    cat("Look at the 'Multiple R-squared' in the model output.")
  }
}

lab_2_question_3 <- function(user_answer) {
  if (check_numeric(user_answer, 5.43, 0.01)) {
    cat("Correct!")
  } else {
    cat("Run a command like this: predict(model, list(AVG=___, OBP=___, SLG=___))")
  }
}

lab_2_question_4 <- function(user_answer_1, user_answer_2) {
  if (check_numeric_pair(user_answer_1, 5.12, user_answer_2, 5.75, 0.01)) {
    cat("Correct!")
  } else {
    cat("Ask yourself: are we making a prediction for a single new observation, or the average Y of all observations with a particular combination of X values?")
  }
}

# Lab 3

lab_3_question_1 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "D")) {
    cat("You got it! Professors with tenure are predicted to have an average evaluation score that is about 0.17 points less than professors without tenure. The confidence interval for tenureyes does not include 0 so the difference is statistically significant.")
  } else {
    cat("Use confint to create the confidence intervals -- is 0 inside the confidence interval for the tenure variable? Then look at the slope to determine whether professors with or without tenure tend to get higher ratings.")
  }
}
  
lab_3_question_2 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "D")) {
    cat("Right! Even after controlling for beauty, the difference is still about 0.17 points and statistically significant. This means that the difference between tenured- and non-tenured professors can't be \"explained away\" by someone who claims that the reason for the difference is that nontenured professors are more attractive than tenured professors (and as we have previously seen, better-looking professors get better evaluations).")
  } else {
    cat("Use confint to create the confidence intervals -- is 0 inside the confidence interval for the tenure variable? Then look at the slope to determine whether, among professors at the same beauty level, professors with or without tenure tend to get higher ratings.")
  }
}

lab_3_question_3 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "A")) {
    cat("Right! The coefficient of 0.069 for classsmall indicates that, among professors of identical attractiveness and tenure status, those that teach small classes are predicted to have evaluation scores that are 0.069 points higher than those that teach large classes.")
  } else {
    cat("Try plugging in numbers for each variable, two times -- for example, plug in the same beauty and tenure status both times, but do it once for a small class and once for a large class.")
  }
}

lab_3_question_4 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "C")) {
    cat("Nice! Looking at the regression output, all else equal, small classes are expected to be 0.069 points higher than large classes, and medium classes are expected to be about 0.073 points lower than large classes. That means that we'd expect small classes to be about 0.069 + 0.073 = 0.142 points higher than medium classes.")
  } else {
    cat("Try plugging in numbers for each variable, two times -- for example, plug in the same beauty and tenure status both times, but do it once for a small class and once for a *medium* class.")
  }
}

lab_3_question_5 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "A")) {
    cat("Yup!")
  } else {
    cat("Put together what you found in the last two questions. (Drawing the three categories out on a number line might be helpful!)")
  }
}

# Lab 4 - Interactions

lab_4_question_1 <- function(user_answer) {
  if (check_numeric(user_answer, 3.74, 0.01)) {
    cat("Correct!")
  } else {
    cat("Write out the equation and plug in 0's or 1's as appropriate, or run a command like this: predict(model, list(gender=___, minority=___))")
  }
}

lab_4_question_2 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "D")) {
    cat("Yup!")
  } else {
    cat("Look at your table -- which cell matches this quantity?")
  }
}

lab_4_question_3 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "F")) {
    cat("Yup!")
  } else {
    cat("Look at your table -- which cell matches this quantity?")
  }
}

lab_4_question_4 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "H")) {
    cat("Yup!")
  } else {
    cat("Look at your table -- which cell matches this quantity?")
  }
}

lab_4_question_5 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "I")) {
    cat("Yup!")
  } else {
    cat("Look at your table -- which cell matches this quantity?")
  }
}

lab_4_question_6 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "E")) {
    cat("Got it!")
  } else {
    cat("Remember that when there is an interaction term between two variables A and B, the slope of A is the incremental impact on Y of each 1-unit change in A, when holding B constant AT ZERO.")
  }
}

lab_4_question_7 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "A")) {
    cat("That's it!")
  } else {
    cat("Write out the two equations and carefully substitute beauty = 2 or beauty = -2, and then simplify. Compare the slope of students for each!")
  }
}

lab_5_question_2 <- function(user_answer) {
  if (check_multiple_answer(user_answer, c("A"))) {
    cat("That's it!")
  } else {
    cat("Look at the subplot in the top right corner.")
  }
}

lab_5_question_3 <- function(user_answer) {
  if (check_multiple_answer(user_answer, c("A", "D"))) {
    cat("That's it!")
  } else {
    cat("Consider the number of observations in the data set vs how many points appear unusual.")
  }
}

lab_5_question_4 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "C")) {
    cat("That's it!")
  } else {
    cat("Look at the y-axis of the Scale-Location plot or the Residuals vs Leverage plot.")
  }
}

lab_5_question_5 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "A")) {
    cat("That's it!")
  } else {
    cat("Look at the Residuals vs Leverage plot.")
  }
}

lab_5_question_6 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "A")) {
    cat("That's it!")
  } else {
    cat("Look at the Residuals vs Leverage plot. Remember that influential = high residual + high leverage!")
  }
}


lab_5_question_8 <- function(user_answer) {
  if (check_multiple_answer(user_answer, c("D"))) {
    cat("That's it!")
  } else {
    cat("Look at the diagnostic plot and also think about how the data were collected.")
  }
}

lab_5_question_9 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "C")) {
    cat("That's it!")
  } else {
    cat("Look at the Scale-Location plot.")
  }
}

lab_5_question_10 <- function(user_answer) {
  if (check_multiple_answer(user_answer, c("E"))) {
    cat("That's it!")
  } else {
    cat("Look at the Residuals vs Leverage plot.")
  }
}

lab_5_question_11 <- function(user_answer) {
  if (check_multiple_choice(user_answer, c("B"))) {
    cat("That's it!")
  } else {
    cat("Recall the definition of influential observations.")
  }
}

lab_6_question_2 <- function(user_answer) {
  if (check_tf(user_answer, "T")) {
    cat("That's it!")
  } else {
    cat("Recreate the scatterplot to be sure.")
  }
}

lab_6_question_3 <- function(user_answer) {
  if (check_multiple_choice(user_answer, c("ACD"))) {
    cat("That's it!")
  } else {
    cat("Perhaps you are forgetting one or more assumptions?")
  }
}

lab_6_question_4 <- function(user_answer) {
  if (check_multiple_choice(user_answer, c("C"))) {
    cat("Correct!")
  } else {
    cat("What does each diagnostic plot tell you?")
  }
}

lab_6_question_5 <- function(user_answer) {
  if (check_multiple_choice(user_answer, c("B"))) {
    cat("That's it!")
  } else {
    cat("What does each diagnostic plot tell you?")
  }
}

lab_6_question_6 <- function(user_answer) {
  if (check_multiple_choice(user_answer, c("F"))) {
    cat("That's it!")
  } else {
    cat("Remember to convert back to dollars.")
  }
}
