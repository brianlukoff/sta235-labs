# Recreate the Rdata file with all of the data sets.
rm(list=ls())
profs <- read.csv("data/profs.csv")
colleges <- read.csv("data/colleges.csv")
baseball <- read.table("data/RunsPerGame.txt", header=T)
save.image("sta235.Rdata")