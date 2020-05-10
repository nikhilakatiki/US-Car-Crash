library(plyr)
library(ggplot2)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(class)

#KNN
myvars <- c("Population", "Deaths", "Fatal.crashes","Vehicle.miles.traveled..millions.","State")
df <- Dataset2014[myvars]
df$Deaths <- as.numeric(as.character(gsub(",","",df$Deaths)))
df$Population <- as.numeric(as.character(gsub(",","",df$Population)))
df$Fatal.crashes <- as.numeric(as.character(gsub(",","",df$Fatal.crashes)))
df$Vehicle.miles.traveled..millions. <- as.numeric(as.character(gsub(",","",df$Vehicle.miles.traveled..millions.)))
head(df) ## see the studcture


##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(df), 0.9 * nrow(df)) 

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run normalization on first 4 columns of dataset because they are the predictors
vehicle_norm <- as.data.frame(lapply(df[,c(1,2,3,4)], nor))

summary(vehicle_norm)

##extract training set
vehicle_train <- vehicle_norm[ran,] 
##extract testing set
vehicle_test <- vehicle_norm[-ran,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
vehicle_target_category <- df[ran,5]
##extract 5th column if test dataset to measure the accuracy
vehicle_test_category <- df[-ran,5]

##run knn function
pr <- knn(vehicle_train,vehicle_test,cl=vehicle_target_category,k=13)

##create confusion matrix
tab <- table(pr,vehicle_test_category)
print(tab)
##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

plot.df = data.frame(vehicle_test, predicted = pr)

plot.df1 = data.frame(x = plot.df$Deaths, 
                      y = plot.df$Population, 
                      predicted = plot.df$predicted)


find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

ggplot(plot.df, aes(Deaths, Population, color = predicted, fill = predicted)) + 
  geom_point(size = 5)+labs(title = "Predicting States based on Deaths and Population", # labelling the title
                           x = "Number of Deaths", # x axis
                           y = "Population")+
  theme_update(plot.title = element_text(hjust = 0.9))+ #setting the title at center
  theme(legend.position="right",legend.background = element_rect(fill="gray90", size=0.9,linetype="solid",colour ="blue")) #setting parameters forlegend

################################################################

#Kmeans
myvars <- c("Vehicle.miles.traveled..millions.","Deaths")
df <- Dataset2014Row[myvars]
df$Vehicle.miles.traveled..millions. <- as.numeric(as.character(gsub(",","",df$Vehicle.miles.traveled..millions.)))

#df$Population <- as.numeric(as.character(gsub(",","",df$Population)))
df$Deaths <- as.numeric(as.character(gsub(",","",df$Deaths)))
#df$Fatal.crashes <- as.numeric(as.character(df$Fatal.crashes))
df<- na.omit(df)
df <- scale(df)

head(df)

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df, centers = 3, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = df,main="K-means clustering",xlab="Number of miles vehicles travelled(Millions)",ylab="Number of deaths(Millions)")
