library(dplyr)
library(ggplot2)
library(corrplot)
library(LICORS)
library(foreach)
library(mosaic)
social = read.csv('./social_marketing.csv')
names(social)



target_df = social[social$personal_fitness > 0|social$health_nutrition>0,]
target_df = target_df[,c(4,8,10,11,17,20,21,24,33)]

summary(target_df)

X= scale(target_df, center=TRUE, scale=TRUE)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")


clust2 = kmeanspp(X, k=6, nstart=25)

clust2$center[1,]*sigma + mu
clust2$center[2,]*sigma + mu
clust2$center[3,]*sigma + mu
clust2$center[4,]*sigma + mu
clust2$center[5,]*sigma + mu
clust2$center[6,]*sigma + mu
