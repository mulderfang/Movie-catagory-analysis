#3
setwd("C:/Users/User/Desktop/商業分析/HW")
movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"")
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", 
                     "IMDB", "Unknown", "Action", "Adventure", "Animation",
                     "Childrens", "Comedy", "Crime", "Documentary", "Drama", 
                     "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", 
                     "Romance", "SciFi", "Thriller", "War", "Western")
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)
#a
Movie.seg = kmeans(movies[,2:20], centers=6,nstart = 100,iter.max=200)
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
k=6
WGSS=c()
for(i in 1:k){
  WGSS[i]=sum(kmeans(movies[,2:20], centers=i)$withinss)
}
plot(1:k, WGSS, type="b")


