setwd("~/Desktop")
library(apcluster)
library(tidyverse)

#Q1 Cosmetics Purchases
#read.data
cos.df <- read.csv("/Users/mac/Desktop/desktop/108_2課程區/商業決策/HW2/Cosmetics.csv")
cos.mat <- as.matrix(cos.df[,-1])
##  convert the binary incidence matrix into a transactions database
cos.trans <- as(cos.mat, "transactions")
inspect(head(cos.trans))
summary(cos.trans)
# plot data
itemFrequencyPlot(cos.trans)
# run apriori function  (use the default parameters)
rules <- apriori(cos.trans, parameter = list(target = "rules"))
# inspect rules
inspect(sort(rules, by = "lift")[1:12])

# [1]  {Brushes}                       => {Nail.Polish} 0.149   1.0000000  3.571429 149  
# [2]  {Blush,Concealer,Eye.shadow}    => {Mascara}     0.119   0.9596774  2.688172 119  
# [3]  {Blush,Eye.shadow}              => {Mascara}     0.169   0.9285714  2.601040 169 
#2,3 ???ƥu?tconcealer
# [4]  {Nail.Polish,Eye.shadow}        => {Mascara}     0.119   0.9083969  2.544529 119
# [5]  {Concealer,Eye.shadow}          => {Mascara}     0.179   0.8905473  2.494530 179
#2,5???ƥu?tBlush
# [6]  {Bronzer,Eye.shadow}            => {Mascara}     0.124   0.8794326  2.463397 124
#6,12???ƥu?O?????A??
# [7]  {Concealer,Eye.shadow,Eyeliner} => {Mascara}     0.114   0.8769231  2.456367 114  
# [8]  {Blush,Mascara}                 => {Eye.shadow}  0.169   0.9184783  2.410704 169  
# [9]  {Eye.shadow,Lipstick}           => {Mascara}     0.110   0.8527132  2.388552 110  
# [10] {Mascara,Lipstick}              => {Eye.shadow}  0.110   0.9090909  2.386065 110 
#9,10???ƥu?O?????A??
# [11] {Blush,Concealer,Mascara}       => {Eye.shadow}  0.119   0.9083969  2.384244 119
#6,12???ƥu?O?????A??
# [12] {Bronzer,Mascara}               => {Eye.shadow}  0.124   0.9051095  2.375615 124  

###  redundancy
# [1]  {Brushes}                       => {Nail.Polish} 0.149   1.0000000  3.571429 149  
# [2]  {Blush,Concealer,Eye.shadow}    => {Mascara}     0.119   0.9596774  2.688172 119  
# [4]  {Nail.Polish,Eye.shadow}        => {Mascara}     0.119   0.9083969  2.544529 119
# [6]  {Bronzer,Eye.shadow}            => {Mascara}     0.124   0.8794326  2.463397 124
# [7]  {Concealer,Eye.shadow,Eyeliner} => {Mascara}     0.114   0.8769231  2.456367 114  
# [9]  {Eye.shadow,Lipstick}           => {Mascara}     0.110   0.8527132  2.388552 110 

rules.sorted <- sort(rules, by="lift")[1:12]
inspect(subset(rules.sorted))

#Q2. Course ratings
#read.data
rate.df<-  read.csv("/Users/mac/Desktop/desktop/108_2課程區/商業決策/HW2/CourseRatings.csv")
rating <-  rate.df
avg <- 
  rating %>% gather(`SQL` , `Spatial` , `PA.1` , `DM.in.R` , `Python` , `Forecast` , `R.Prog` ,     
                    `Hadoop` , `Regression` , key = "x" , value = "number") %>%
  group_by(X) %>% summarise( avg = mean( number,na.rm = TRUE))

#data clear
rating <- rating[,c(which(is.na(rating[4,]) == FALSE))]
rating <- rating[-6:-14,]


rating_EN <- rating %>% left_join(avg)
#2-a
#EN vs LN
((rating_EN[4,2]-rating_EN[4,6])*(rating_EN[1,2]-rating_EN[1,6])+
 (rating_EN[4,4]-rating_EN[4,6])*(rating_EN[1,4]-rating_EN[1,6])+
 (rating_EN[4,5]-rating_EN[4,6])*(rating_EN[1,5]-rating_EN[1,6]))/
  (sqrt((rating_EN[4,2]-rating_EN[4,6])^2+
        (rating_EN[4,4]-rating_EN[4,6])^2+
        (rating_EN[4,5]-rating_EN[4,6])^2)*
   sqrt((rating_EN[1,2]-rating_EN[1,6])^2+
        (rating_EN[1,4]-rating_EN[1,6])^2+
        (rating_EN[1,5]-rating_EN[1,6])^2))
#EN vs MH
((rating_EN[4,2]-rating_EN[4,6])*(rating_EN[2,2]-rating_EN[2,6]))/
  (sqrt((rating_EN[4,2]-rating_EN[4,6])^2)*sqrt((rating_EN[2,2]-rating_EN[2,6])^2))
#EN vs JH
((rating_EN[4,2]-rating_EN[4,6])*(rating_EN[3,2]-rating_EN[3,6]))/
  (sqrt((rating_EN[4,2]-rating_EN[4,6])^2)*sqrt((rating_EN[3,2]-rating_EN[3,6])^2))
#EN vs DU
((rating_EN[4,2]-rating_EN[4,6])*(rating_EN[5,2]-rating_EN[5,6]))/
  (sqrt((rating_EN[4,2]-rating_EN[4,6])^2)*sqrt((rating_EN[5,2]-rating_EN[5,6])^2))
#EN vs DS
((rating_EN[4,2]-rating_EN[4,6])*(rating_EN[6,2]-rating_EN[6,6])+
 (rating_EN[4,3]-rating_EN[4,6])*(rating_EN[6,3]-rating_EN[6,6])+
 (rating_EN[4,4]-rating_EN[4,6])*(rating_EN[6,4]-rating_EN[6,6]))/
  (sqrt((rating_EN[4,2]-rating_EN[4,6])^2+
        (rating_EN[4,3]-rating_EN[4,6])^2+
        (rating_EN[4,4]-rating_EN[4,6])^2)*
   sqrt((rating_EN[6,2]-rating_EN[6,6])^2+
        (rating_EN[6,3]-rating_EN[6,6])^2+
        (rating_EN[6,4]-rating_EN[6,6])^2))
#2-b
#LN because it is closest to 1 
rating_EN

#2-c
library(recommenderlab)

dim(rate.df)
summary(rate.df)
head(rate.df)

rate.df <- rate.df[,2:ncol(rate.df)]
min(rate.df[][], na.rm = TRUE)
max(rate.df[][], na.rm = TRUE)
hist(as.vector(as.matrix(rate.df)), main = "Distribution of Jester Ratings",col = "yellow", xlab = "Ratings")
summary(rate.df)

rmat <- as.matrix(rate.df)
rmat <- as(rmat,"realRatingMatrix")
UB.Rec <- Recommender(rmat, "UBCF")
pred <- predict(UB.Rec, rmat, type="ratings")

# compute the cosine similarity between users
round(similarity(rmat, method="cosine"),4)

#2-d
# closest students No.2 3 5
round(similarity(rmat, method="cosine"),4)
rmat@data
#Spatial


#e
#The formula differs from the correlation formula
# by not subtracting the means. The meaning of mean subtraction is to adjust for users?? overall rating
#: some tend to be nice while others tend to be strict.

#f
item_base <- as.matrix(rmat@data)
item_base <- ifelse(item_base == 0  , NA , item_base)
mean(item_base[,1],na.rm = TRUE)
#SQL = 3.5
mean(item_base[,7],na.rm = TRUE)
#Rpog = 4
mean(item_base[,9],na.rm = TRUE)
#Regression = 2.5
mean(item_base[,5],na.rm = TRUE)
#python=3.5

#correlation
#SQL vs Spatial = 0.8181
((-0.5)*0.5  +  (-1.5)*(-1.5)  +  (0.5)*(0.5)  )/(  sqrt( (-0.5)^2+(-1.5)^2+(0.5)^2 ) * sqrt( (0.5)^2+(-1.5)^2+(0.5)^2 )   )
#SQL vs Python = -1.00
((0.5)*(-0.5)  +  (-0.5)*(0.5) )/(sqrt(0.5) * sqrt(0.5))

#2-g

#item based
##item-based collaborative filtering
IB.Rec <- Recommender(rmat, "IBCF")
pred2 <- predict(IB.Rec, rmat)
as(pred2, "matrix")
#spatial,python,forecast
#in(f),choose spatial

#3.Clustering
movies <-  read.table("/Users/mac/Desktop/desktop/108_2課程區/商業決策/HW2/movieLens.txt", header=FALSE, sep="|", quote="\"")
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", 
                     "IMDB", "Unknown", "Action", "Adventure", "Animation",
                     "Childrens", "Comedy", "Crime", "Documentary", "Drama", 
                     "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", 
                     "Romance", "SciFi", "Thriller", "War", "Western")
movies <- movies %>% select("Title",names(movies)[6:24])
movies <-unique(movies)

#a
Movie.seg = kmeans(movies[,2:ncol(movies)], centers=6,nstart = 100,iter.max=200 , set.seed(100))
cluster1 = subset(movies, Movie.seg$cluster==1)
head(cluster1,n=5)
cluster2 = subset(movies, Movie.seg$cluster==2)
head(cluster2,n=5)
cluster3 = subset(movies, Movie.seg$cluster==3)
head(cluster3,n=5)
cluster4 = subset(movies, Movie.seg$cluster==4)
head(cluster4,n=5)
cluster5 = subset(movies, Movie.seg$cluster==5)
head(cluster5,n=5)
cluster6 = subset(movies, Movie.seg$cluster==6)
head(cluster6,n=5)
#b
Movie.seg
#c
#k=6
require(factoextra)
fviz_cluster(Movie.seg,           
             data = movies[,2:ncol(movies)],       
             geom = c("point","text"), 
             frame.type = "norm")  


# K value decide method 1 
library(ggplot2)
list <- seq(1:10)
kmean_fun <- function(x) {
  movies_kms <- kmeans(movies[,2:ncol(movies)], centers = x, nstart = 100,iter.max=200 , set.seed(100))
  ratio <- movies_kms$tot.withinss / (movies_kms$tot.withinss + movies_kms$betweenss)
}
ratios <- sapply(list, kmean_fun)

data_kmean <- data.frame(
  klist = list, 
  ratios = ratios)

ggplot(df, aes(x = klist, y = ratios, label = klist, color = ratios)) +
  geom_point(size = 5) + geom_text(vjust = 2)

# K value decide method 2
WGSS=c()
for(i in 1:10){
  WGSS[i]=sum(kmeans(movies[,2:ncol(movies)], centers=i)$withinss, set.seed(100))
}
plot(1:10, WGSS, type="b")

#k=4
Movie.seg = kmeans(movies[,2:ncol(movies)], centers=4,nstart = 100,iter.max=200 , set.seed(100))
fviz_cluster(Movie.seg,           
             data = movies[,2:ncol(movies)],       
             geom = c("point","text"), 
             frame.type = "norm")  

Movie.seg = kmeans(movies[,2:ncol(movies)], centers=10,nstart = 100,iter.max=200 , set.seed(100))
Movie.seg
