#2

#a
#LN/
rate.df = read.csv(file = "/Users/mac/Desktop/desktop/108_2課程區/商業決策/HW2/CourseRatings.csv")

library(tidyverse)
rate.df %>% filter(X =="E.N.")
rate.df %>% filter(!is.na(SQL) | !is.na(DM.in.R)|!is.na(R.Prog)|!is.na(Regression))

#b


dim(rate.df)
summary(rate.df)
head(rate.df)

rate.df <- rate.df[,2:ncol(rate.df)]
min(rate.df[][], na.rm = TRUE)
max(rate.df[][], na.rm = TRUE)
hist(as.vector(as.matrix(rate.df)), main = "Distribution of Jester Ratings",col = "yellow", xlab = "Ratings")
summary(rate.df)
library(recommenderlab)
rmat <- as.matrix(rate.df)
rmat <- as(rmat,"realRatingMatrix")
UB.Rec <- Recommender(rmat, "UBCF")
pred <- predict(UB.Rec, rmat, type="ratings")
#4  E.N. python
rmat
similarity(rmat, method="cosine")

#c
#2,3,5
rmat@data

#d
#Spatial

#e
#The formula differs from the correlation formula
# by not subtracting the means. The meaning of mean subtraction is to adjust for users?? overall rating
#: some tend to be nice while others tend to be strict.

#f
#SQL ->  Python.Forecast
#DM.in.R -> NO
#R.Prog -> Regression

#g
#item based
##item-based collaborative filtering
IB.Rec <- Recommender(rmat, "IBCF")
pred2 <- predict(IB.Rec, rmat)
as(pred2, "matrix")
#spatial,python,forecast


