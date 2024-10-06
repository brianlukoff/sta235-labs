source("https://github.com/brianlukoff/sta235-labs/raw/main/src/common.R")
install.packages("ggthemes")
library(gridExtra)
library(ggthemes, quietly=TRUE)

set.seed(1022)
ix = sample(1:nrow(utilities), 12)
uin = utilities[-ix,]
uout = utilities[ix,]

sleep = sleep %>% filter(total_sleep_time<550 & total_sleep_time>195 & study != 1) %>% 
  mutate(total_sleep_time = total_sleep_time/60)
# lm1 <- lm(dailyspend~temp, data=uin)
# lm2 <- lm(dailyspend ~ temp +I(temp^2), data=uin)
# lm7 <- lm(dailyspend ~ temp + I(temp^2)+I(temp^3)+I(temp^4)+I(temp^5)+I(temp^6)+I(temp^7), data=uin)

lab7_plot_utilities_poly <- function(lm1, lm2, lm7, newpts=FALSE){
  xgr = seq(min(utilities$temp), max(utilities$temp), length.out = 1000)
  newdat = data.frame(temp=xgr)
  
  pm = list(lm1, lm2, lm7)
  pm_preds = sapply(pm, function(x) predict(x, newdata=newdat))
  deg = c(1,2,7)
  colnames(pm_preds) = paste0("d", deg)
  
  preds = data.frame(temp = xgr, pm_preds) %>%
    pivot_longer(
      starts_with("d"),
      names_to = c(".value", "degree"),
      names_pattern = "(.)(.)"
    )
  if (!newpts){
  ggplot(aes(x=temp, y=dailyspend), data=uin) + 
    geom_point(color="lightgray") + 
    geom_line(aes(x=temp, y=d, color=degree), data=preds) + 
    scale_color_colorblind()
  }
  else {
    ggplot(aes(x=temp, y=dailyspend), data=uin) + 
      geom_point(color="lightgray") + 
      geom_line(aes(x=temp, y=d, color=degree), data=preds) + 
      geom_point(aes(x=temp, y=dailyspend), color="darkred", data=uout) +
      scale_color_colorblind()
  }
}


lab_7_question_1 <- function(user_answer) {
  if (check_numeric(user_answer, 0.7871704777, 0.01)) {
    cat("You got it!")
  } else {
    cat("That's not it -- Double check the steps of your RMSE calculation")
  }
}


lab_7_question_2 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "B")) {
    cat("You got it! The quadratic model had the lowest OOS RMSE.")
  } else {
    cat("Remember, the lower the RMSE, the smaller the error, the better the performance.")
  }
}

lab_7_question_3 <- function(user_answer) {
  if (check_multiple_choice(user_answer, "B")) {
    cat("You got it! The quadratic model had the lowest OOS RMSE. The 7th order polynomial OVERFITS.")
  } else {
    cat("Remember, the lower the RMSE, the smaller the error, the better the performance.")
  }
}
lab_7_question_4 <- function(user_answer) {
  if (checktf(user_answer, F)) {
    cat("You got it! Race does not seem to have an impact on sleep time")
  } else {
    cat("Look at the boxplot carefully, does race seem to impact sleep time?")
  }
}

lab_7_question_5 <- function(user_answer) {
  if (checktf(user_answer, T)) {
    cat("You got it! Race does not seem to have an impact on cumulative gpa")
  } else {
    cat("Look at the boxplot carefully, does race seem to cumulative gpa?")
  }
}

lab_7_question_6 <- function(user_answer) {
  if (checktf(user_answer, F)) {
    cat("You got it! First-generation does not seem to have an impact on sleep time")
  } else {
    cat("Look at the boxplot carefully, does first-generation seem to impact sleep time?")
  }
}

lab_7_question_7 <- function(user_answer) {
  if (checktf(user_answer, T)) {
    cat("You got it! First-generation does not seem to have an impact on cumulative gpa")
  } else {
    cat("Look at the boxplot carefully, does first-generation seem to cumulative gpa?")
  }
}


lab_7_RMSE <- function(pm, df) {
  preds_oos <- predict(pm, df)
  # You should see 12 numbers below, a prediction for every data point in uout
  square_error <- (df$dailyspend - preds_oos)^2 # square the difference between actual and predicted
  mean_square_error <- mean(square_error) # average the square error
  root_mean_square_error <- sqrt(mean_square_error) # take the square root of the error
  return (root_mean_square_error)
}


# lab7_plot_utilities_poly()
# lab7_plot_utilities_poly(TRUE)
# 
# # Step 1 - predictions for the new data
# preds_oos <- predict(lm1, uout)
# # You should see 12 numbers below, a prediction for every data point in uout
# preds_oos
# actuals <- uout$dailyspend
# square_error <- (actuals - preds_oos)^2 # square the difference between actual and predicted
# mean_square_error <- mean(square_error) # average the square error
# root_mean_square_error <- sqrt(mean_square_error) # take the square root of the error
# paste("The RMSE is: ", round(root_mean_square_error,4))
# lab_7_RMSE(lm1, uout) # this should match what you had above
# lab_7_RMSE(lm2, uout)
# lab_7_RMSE(lm7, uout)
# lab_7_RMSE(lm1, uin) # this should match what you had above
# lab_7_RMSE(lm2, uin)
# lab_7_RMSE(lm7, uin)