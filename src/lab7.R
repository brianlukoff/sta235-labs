set.seed(1022)
ix = sample(1:nrow(utilities), 12)
uin = utilities[-ix,]
uout = utilities[ix,]
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

# lab7_plot_utilities_poly()
# lab7_plot_utilities_poly(TRUE)
