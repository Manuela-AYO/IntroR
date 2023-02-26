# load iris dataset. by just calling data(), we list
# all the datasets
data("iris")
mydata <- iris[1:4] # take the 4 first columns
class <- as.matrix(iris[5]) # create a matrix from the 5th column
dim(iris) # shape of iris dataset

# ********** description *********

summary(iris) # like describe in python
help(iris) # provide the documentaion of the dataset
cor(mydata) # compute the correlation coefficient

# * We see that the most correlated attributes are petal width 
# * petal length, sepal length and petal length and sepal
# * length and petal width


# *********** visualization ************

# pairwise plot
library(corrplot) # load add-on packages
corcoeff = cor(mydata)
corrplot(corcoeff, order='hclust', addrect=2, method="pie")
# we have 7 methods for visualization of the correlation :
# cf : https://taiyun.github.io/corrplot/
# with ellipse method, the smaller the ellipse, the biggest the 
# the coefficient

mydata[1:5, 1:4]

# pairwise plots
pairs(mydata, pch=23, bg=c("red", "green3", "blue")[unclass(iris$Species)])


# *************** Principal Component Analysis *******

iris.pca <- prcomp(iris[1:4]) #pca
iris.pca

# * the first component is directed by the Petal Length
# * the second component is directed by the Sepal Length
# * and sepal width

summary(iris.pca)
# the 2 first components explain more than 98% of the 
# variation of the data

# pch : integer giving the type of representation(solid circle,
# bullet, filled diamond, etc)
pairs(iris.pca, pch=21)

typeof(iris.pca)

str(iris.pca) # str gives the structure of the list iris.pca
iris.pca$y
iris.pca
dim(iris.pca$x)

# to draw
X11(width=10, height = 10 )
plot(iris.pca$x, main="PCA Plot", font.main=4, pch=21, 
     bg=c("red", "green3", "blue")[unclass(iris$Species)]) # scatter plot
pairs(iris.pca$x, main="PCA Plot", font.main=4, pch=19)

biplot(iris.pca) # biplot


# *********** Clustering ***********

# _______ data preparation __________

# z-score normalisation
iris.zs <- iris

for (i in 1:4) {
  i.mean <- sapply(iris[i], mean) # get the mean of that column
  i.std <- sapply(iris[i], sd) # get the std
  iris.zs[i] <- (iris.zs[i] - i.mean)  / i.std
}

# now all the data should be centered and scaled
summary(iris.zs)
# everything is ok

# _________ k means ______________

# kmeans with 3 clusters
for (i in 1:10){
  clf <- kmeans(iris[1:4], 3)
  print(clf)
  plot(iris[3:4], col=clf$cluster)
}

for (i in 1:10){
  clf2 <- kmeans(iris.zs[1:4], 3)
  print(clf2)
  plot(iris.zs[3:4], col=clf$cluster)
}

# the results are pretty stable

# _______ hierarchical clustering _______
hc <- hclust(dist(iris[1:4]), "ave")
print(hc)
X11(width=10, height = 10 )
plot(hc)

hc <- hclust(dist(iris[1:4]), "ward.D")
print(hc)
plot(hc)

hc <- hclust(dist(iris[1:4]), "ward.D2")
print(hc)
plot(hc)

hc <- hclust(dist(iris[1:4]), "single")
print(hc)
plot(hc)

hc <- hclust(dist(iris[1:4]), "complete")
print(hc)
plot(hc)


# ************** Classification *********

# _________ Decision Tree ________
library(rpart) # load the rpart package

# divide the dataset into training and testing data
set.seed(2568)
n <- nrow(iris) # number of rows
print(n)
train <- sort(sample(1:n, floor(n/2)))
iris.train <- iris[train,]
dim(iris.train)
head(iris.train)
iris.test <- iris[-train, ]
head(iris.test)
dim(iris.test)
head(iris)

# decision tree on the learning set
iris.rp <- rpart(class ~ .,
                 data = iris[1:4],
                 subset = train,
                 method = "class", # class method is used for classification 
                 parms = list(split = "information"),
                 maxsurrogate = 0,
                 cp = 0,
                 minsplit = 5, # minimum number of data to split
                 minbucket = 2) # minimum number of observations in any terminal node

# for regression, it would be "anova"

summary(iris.rp)

X11(width=10, height = 10)
plot(iris.rp,
     uniform = TRUE,
     compress = TRUE,
     margin = .2)
text(iris.rp,
     use.n = TRUE,
     all = TRUE,
     fancy = TRUE)

# * The result is consistent with the predictions of PCA
# * In fact, most of the variance was explained by the first
# * component

# evaluation on the test set
pred.rp <- predict(iris.rp,
                   newdata = iris[-train,],
                   type = "class")
pred.rp

pred.rp <- predict(iris.rp,
                   newdata = iris[-train,],
                   type = "prob")
pred.rp
pred.rp <- predict(iris.rp,
                   newdata = iris[-train,],
                   type = "vector")
pred.rp
pred.rp <- predict(iris.rp,
                   newdata = iris[-train,],
                   type = "matrix")
pred.rp

# cross table
table(class[-train], pred.rp)

# * 69 data are well predicted
# * setosa is the easiest class to predict

# _________ KNN _________

library(class)
data(iris3)
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
head(train)
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
clf <- factor(c(rep("Setosa", 25), rep("Versicolor", 25),
                rep("Virginica", 25)))
clf
pred <- knn(train, test, clf, k=3)
pred
print(iris3)
table(pred, clf)

# * 69 examples are well predicted
# * the easiest class to predict is Setosa
# * Iris dataset is pretty well discriminated

# ************** Overfitting Problem **********

a = c(-0.5, 0.5)
typeof(a)
x <- 1:10
x
y <- x + c(-0.5, 0.5)
plot(x,y)

model1 <- lm(y~x) # simple linear regression
lines(x, predict(model1, data.frame(x)), lty=1, col="blue")

model2 <- lm(y~poly(x,3)) # polynomial regression of order 3
lines(x, predict(model2, data.frame(x)), lty=1, col="green")

model3 <- lm(y~poly(x,9)) # polynomial regression of order 9
lines(x, predict(model3, data.frame(x)), lty=1, col="red")


# ********** Exercise ***************

# read the file
data <- read.csv("~/M1/DM/PS1/breast-cancer-wisconsin.data", header=F,
                 quote="")
# dimension
dim(data)

# type of data
typeof(data)
data <- data.frame(data)
typeof(data) # dataframes are list type

# add the columns names
colnames(data) <- c("ID", "Clump_Thickness", "Uniformity_Cell_Size", 
                    "Uniformity_Cell_Shape",
                    "Marginal_Adhesion",
                    "Single_Epithelial_Cell_Size",
                    "Bare_Nuclei", "Bland_Chromatin",
                    "Normal_Nucleoli", "Mitoses",
                    "Class")
head(data)

summary(data)
unique(data["Clump_Thickness"])
nrow(unique(data["Uniformity_Cell_Size"]))
nrow(unique(data["Uniformity_Cell_Shape"]))

# store in a csv
write.csv(data, "~/M1/DM/PS1/breast_data.csv", row.names = FALSE)

# split between target and predictors
x <- data[1:10]
head(x)
y <- data["Class"]
head(y)
typeof(y)
(unique(y))

# correlation between data
str(x) # gives the structure of the dataset
correl = cor(x[,-c(1, 7)])
correl
x11(width = 10, height = 10)
corrplot(correl, order='hclust', addrect=2, method="number")

# * the most correlated data are Uniformity_Cell_Shape and
# * Uniformity_Cell_Size

# plotting
plot(x[3:4], col=c("red", "yellow"))
pairs(x[2:5], pch=3, col=c("red", "yellow"))

# ** let us normalize our columns
x_pca <- x[, -c(1,7)]
typeof(x_pca)
for (i in 1:8) {
  mean <- sapply(x_pca[i], mean)
  std <- sapply(x_pca[i], sd)
  x_pca[i] <- (x_pca[i] - mean)/std
}

summary(x_pca)

# visualization with PCA

# compute the pca
x_pca.pca <- prcomp(x_pca)
x_pca.pca
summary(x_pca.pca)
pairs(data.pca, pch=21)
# * the different components don't really help to identify which columns is the
# * most used

# * 2 components are not enough to represent those data
# * we just have 75.322% of the variance explained

pairs(x_pca.pca$x, main="PCA Plot", pch=19)
plot(x_pca.pca$x, main="PCA Plot", pch=21, col=c("red", "yellow"))

# let us compute the principal component analysis of the dataset
x_pca2 <- princomp(x_pca)
x_pca2
biplot(x_pca2)
summary(x_pca2)

# Clustering

# * Don't really expect somthing from the clustering
clf <- kmeans(x_pca, 2)
clf
plot(x[3:4], col=clf$cluster)
dim(x_pca)
head(x_pca)
# let us see if the result is stable
for (i in 1:8){
  clf <- kmeans(x_pca[1:4], 3)
  print(clf)
  plot(x_pca[2:3], col=clf$cluster)
}

# * the clusters are not stable

# Decision Tree

# ** split the data
nb_row <- nrow(x)
nb_row
idx_train <-sort(sample(1:nb_row, floor(nb_row/2)))
length(idx_train)
x.train <- x[idx_train,]
head(x.train)
x.test <- x[-idx_train,]
head(x.test)
help(rpart)

# ** build the decision tree
x.tree <- rpart(class ~ . ,
                data = data[1:10],
                subset = idx_train, 
                method = "class", 
                parms = list(split = "information"),
                maxsurrogate = 0,
                cp = 0,
                minsplit = 5,
                minbucket = 2
                )

summary(data)
n <- nrow(data)
nb_at <- ncol(data)
p <- nb_at - 1 # number of predictive attributes
data.ok <- data

for (i in 1:n)
  for (j in 1:p)
    if (data[i,j] == "?")
      data.ok[i, j] <- NA

# function to have all the predictive attributes as numerical one
data.ok[1:p] <- lapply(data.ok[1:p], FUN=as.numeric)
str(data.ok)

# exclude missing values from our dataset
data.ok <- na.exclude(data.ok)
str(data.ok)

# correlation
cor(data.ok[2:10])
summary(data.ok)
class(data.ok)

n.new <- nrow(data.ok)

# change the type of class column
for (i in 1:n.new) {
  if(data.ok[i, nb_at] == 2)
    data.ok[i, nb_at] <- "begnin"
  else
    data.ok[i, nb_at] <- "malignant"
}
str(data.ok)

# let us remove the first column "ID" which won't be used
data.ok[,1] <- NULL
dim(data.ok) # perfect

# let us save the transformed dataset
write.csv(data.ok, file="wisconsin-breast-cancer.csv", row.names = TRUE)

# let us retry the decision tree
set.seed(1)
n <- nrow(data.ok)
n_at <- ncol(data.ok)
p <- n_at - 1
idx_train <- sort(sample(1:n, floor(n/2)))
data.train <- data.ok[idx_train,]
data.test <- data.ok[-idx_train, ]
data.rp <- rpart(formula = data.ok$Class ~ .,
                 data = data.ok[1:p],
                 subset = idx_train, 
                 method = "class",
                 parms = list(split = "information"),
                 maxsurrogate = 0,
                 cp = 0,
                 minsplit = 5, 
                 minbucket = 2)

summary(data.rp)
plot(data.rp,
     uniform = TRUE, 
     compress = TRUE,
     margin = .2)
text(data.rp,
     use.n = TRUE, 
     all = TRUE,
     fancy = TRUE)

# prediction
pred.rp <- predict(data.rp,
                   newdata = data.ok[-idx_train,],
                   type = "class")
pred.rp
table(data.ok$Class[-idx_train], pred.rp)

install.packages("rattle")
library(rattle)
