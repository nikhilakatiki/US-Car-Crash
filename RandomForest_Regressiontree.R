
library (tree)
library(ISLR)
library(randomForest)
library(rpart)
library(corrplot)




redesign <- read.csv("Dataset2014 (1).csv",stringsAsFactors=FALSE)
colnames(redesign) <- c("State", "Population", "Travel", "Crashes", "Deaths", "DeathsIn100000", "Death_Travel")

redesign$State <- gsub("\xca", "", redesign$State)
redesign$Population <- gsub(",", "", redesign$Population)
redesign$Travel <- gsub(",", "", redesign$Travel)
redesign$Crashes <- gsub(",", "", redesign$Crashes)
redesign$Deaths <- gsub(",", "", redesign$Deaths)
redesign[ ,c(2:5)] <- lapply(redesign[ ,c(2:5)], as.numeric)


head(redesign)
names(redesign) # same as colnames
dim(redesign)  # rows and columns


# Check for missing data
sum(is.na(redesign))

redesign = na.omit(redesign)
sum(is.na(redesign))

set.seed(997)
train = sample(1:nrow(redesign), nrow(redesign)/2)
test <- redesign[-train,]
##########################################################
# Regression Tree
#Fit the tree to the training data.
tree.redesign=tree(Deaths~ .,redesign,subset=train)
summary(tree.redesign)
plot(tree.redesign)
text(tree.redesign,pretty=0,cex=0.6)
tree.redesign


##############################################################
#Random Forest


random <- randomForest(Deaths ~ . - State, data = redesign)
print(random)
varImpPlot(random, cex = 0.8)
na.omit(redesign)
bag.random=randomForest(Deaths~.-State,data=redesign,subset=train,mtry=4,importance=TRUE)
bag.random
###############################################################
# classification Tree
# grow tree 
fit <- rpart( Deaths ~ . - State,method="class", data=redesign)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
plot(fit, uniform=TRUE, 
     main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


