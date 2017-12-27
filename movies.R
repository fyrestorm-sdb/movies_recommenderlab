library(ggplot2)
library(reshape2)
library(stringr)
library(recommenderlab)	



#https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

ratings=read.csv("ratings_short.csv",sep=",")
users=read.csv("users.csv",sep=",")
movies=read.csv("movies.csv",sep=",")
#ratings=ratings[1:500000,]#eco memoire

colnames(ratings)=c("UserID","MovieID","Rating","Timestamp")
colnames(users)=c("UserID","Gender","Age","Occupation","Zipcode")
colnames(movies)=c("MovieID","Title","Genres")



round(prop.table(table(movies$Genres))*100)  # proportion


round(prop.table(table(users$Gender))*100)  # proportion
#F  M 
# 28 72 
ggplot(users,aes(x=Gender))+geom_bar(fill="#FF9999")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(,hjust = 1,size=10))



round(prop.table(table(ratings$Rating))*100)  # proportion
 #1  2  3  4  5 
 #6 11 26 35 22 
ggplot(ratings,aes(x=Rating))+geom_bar(fill="#FF9999")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(,hjust = 1,size=10))
 
 
 
################preparation 
ratings$Timestamp=NULL #supprimer la colonne Timestamp

affinity=acast(ratings,UserID ~ MovieID ) # creation de la matrice UserID X MovieID
 ### ne pas remplacer NA par 0 !!!!

dim(affinity)
#[1] 3069 3618
rownames(affinity)=c(paste("u", 1:dim(affinity)[1], sep="")) #generer des noms en u****
colnames(affinity)=c(paste("movie", 1:dim(affinity)[2], sep="")) #generer des noms en movie****



affinity.matrix= as(affinity,"realRatingMatrix")
affinity.matrix = normalize(affinity.matrix) ## NORMALISER
getRatingMatrix(affinity.matrix) ## display

Rec.model=Recommender(affinity.matrix, method = "UBCF")
#Recommender based on user-based collaborative filtering

######predictions
user1= predict(Rec.model, affinity.matrix["u1",], n=5) # top5 film recommande pour l'user1(u1)
as(user1, "list")
# $u1
# [1] "movie283"  "movie3352" "movie2339" "movie446"  "movie3515"


user1.affinity = predict(Rec.model, affinity.matrix["u1",],type="ratings" ) #rating de user1(u1) pour TOUS les films meme ceux qu'il n'a pas note
as(user1.affinity,"list")
# $u1
   # movie2    movie3    movie4    movie5    movie6    movie7    movie8    movie9   movie10   movie11 
 # 3.713178  3.938884  4.190476  3.146465  3.930974  4.322581  3.884892  3.712320  4.114713  3.277372 
  # movie12   movie13   movie14   movie15   movie16   movie17   movie18   movie19   movie20   movie21 
 # 3.826087  3.388889  3.320000  3.288838  3.074466  4.075829  3.649180  3.572549  4.083333  2.909091 
  # movie22   movie23   movie24   movie25   movie26   movie27   movie28   movie29   movie30   movie31 
 # 3.067340  3.312999  3.975196  3.741176  2.960000  4.171429  3.757009  3.618762  3.488372  3.731092

 
 
##### fiche complete pour user1
user1_df=as.data.frame(as(user1.affinity,"list"))
user1_df$movie=rownames(user1_df) #les index de lignes correspondent aux films : on les places dans une nouvelles colonnes 'movie'
rownames(user1_df)=NULL # on efface les index de lignes
user1_df$movie=str_split_fixed(user1_df$movie, "movie", 2)[,2] # on decoupe les chaines 'movie283' en 'movie' et '283', on ne garde que les numeros
colnames(user1_df)=c("rating","MovieID") 
 
user1_df=merge(user1_df, movies, by="MovieID")  # on fusionne user1_df avec movies pour obtenir la fiche des films pour user1  
head(user1_df[order(-user1_df$rating),]  ) #visonnage
     # MovieID   rating                             Title         Genres
# 1949     283 4.962963           New Jersey Drive (1995)    Crime|Drama
# 2525    3352 4.962963            Brown's Requiem (1998)          Drama
# 1412    2339 4.956522 I'll Be Home For Christmas (1998) Comedy|Romance
# 2706    3515 4.843137                Me Myself I (2000)         Comedy
# 2914     446 4.843137      Farewell My Concubine (1993)  Drama|Romance
# 2707    3516 4.837838      Bell, Book and Candle (1958) Comedy|Romance  
  
  
  
  
############### evaluation
  
e = evaluationScheme(affinity.matrix, method="split", train=0.7,given=3)
# creation d'un modele de recommendation type ubcf
Rec.ubcf = Recommender(getData(e, "train"), "UBCF")
# creation d'un modele de recommendation type ibcf pour comparer
Rec.ibcf = Recommender(getData(e, "train"), "IBCF")
# predictions sur test (UBCF)
p.ubcf = predict(Rec.ubcf, getData(e, "known"), type="ratings")
# predictions sur test (IBCF)
p.ibcf = predict(Rec.ibcf, getData(e, "known"), type="ratings")
# Calcul des erreurs pour chaque methodes
error.ubcf=calcPredictionAccuracy(p.ubcf, getData(e, "unknown"))
error.ibcf=calcPredictionAccuracy(p.ibcf, getData(e, "unknown"))
error = rbind(error.ubcf,error.ibcf)
rownames(error) = c("UBCF","IBCF")
error
# 		RMSE      MSE       MAE   # lower better
# UBCF 1.160121 1.345882 0.9131636
# IBCF 1.665191 2.772861 1.2743363





