set.seed(1022)
ix = sample(1:nrow(utilities), 12)
uin = utilities[-ix,]
uout = utilities[ix,]