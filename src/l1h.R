library(tidyverse, quietly=TRUE)
profs <- read_csv('https://emyucel.com/sta235/profs.csv')

check_pred_1 <- function(user_answer){
  tolerance <- 0.01
  target_value <- 4.06477
  if (abs(user_answer - target_value) <= tolerance) {
    return("Correct")
  } else {
    return("Incorrect")
  }
}

check_conf_1 <- function(user_answer) {
  if (tolower(input) == "c") {
    return("Correct")
  } else {
    return("Incorrect")
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


