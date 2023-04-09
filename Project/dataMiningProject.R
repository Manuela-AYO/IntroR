# **** DO WE HAVE A NEW NICHE FOR A FINTECH ? ***
# On this project, we'll explore how do "low" social category
# people manage their finances.
# And maybe, we'll find a business opportunity

# https://statisticsglobe.com/barplot-in-r
# https://www.geeksforgeeks.org/random-forest-approach-for-classification-in-r-programming/
# https://www.rdocumentation.org/packages/randomForest/versions/4.7-1.1/topics/randomForest

# ********* Libraries ******** #
library(magrittr)
library("tidyr")
library("tidyverse")
library(data.table)
library(ggplot2)
library("viridisLite")
library("viridis")
library(corrplot)
library(ggplot2)
library(rpart)
library(fastDummies)
library(randomForest)
library(snow)
library(snowfall)
library(Hmisc)
library(lattice)
library(caret)
library(doParallel)

# ******** DATA EXPLORATION *************** #
my_data <- read.csv("Data/application_record.csv")
credits <- read.csv("Data/credit_record.csv")

# shape
dim(my_data) # (438557, 18)
dim(credits) # (1048575, 3)
colnames(my_data)
colnames(credits)

# comments : we see that the datasets have different shapes

# summary
summary(my_data)
summary(credits)

# unique values
length(unique(my_data$ID)) # 438510 unique values
length(unique(credits$ID)) # 45985 unique values

# let's explore application_record dataset
unique(my_data$FLAG_OWN_CAR) # ["Y" "N"]
unique(my_data$FLAG_OWN_REALTY) # ["Y" "N"]
unique(my_data$NAME_INCOME_TYPE) #  ["Working","Commercial associate","Pensioner","State servant","Student"]
unique(my_data$NAME_EDUCATION_TYPE) # ["Higher education", "Secondary / secondary special", "Incomplete higher", "Lower secondary", "Academic degree"]
unique(my_data$NAME_FAMILY_STATUS) # ["Civil marriage","Married", "Single / not married", "Separated", "Widow" ]
unique(my_data$NAME_HOUSING_TYPE) # ["Rented apartment", "House / apartment", "Municipal apartment", "With parents", "Co-op apartment", "Office apartment"]
unique(my_data$OCCUPATION_TYPE)
# ["", "Security staff", "Sales staff", "Accountants""Laborers" , "Managers", "Drivers", "Core staff", "High skill tech staff", "Cleaning staff", "Private service staff", 
# "Cooking staff", "Low-skill Laborers", "Medicine staff", "Secretaries", "Waiters/barmen staff", "HR staff", "Realty agents", "IT staff"]

unique(my_data$OCCUPATION_TYPE)
unique(my_data$NAME_EDUCATION_TYPE)
unique(my_data$NAME_INCOME_TYPE)
my_data[my_data$NAME_INCOME_TYPE == "Student",]

# missing values ? 
sapply(my_data, function(x) sum(is.na(x))) # we don't have any missing value

# correlation
correl = cor(my_data[c(5,6,12,18)])
x11(width = 10, height = 10)
corrplot(correl, order='hclust', addrect=2, method="number")

# this corrplot shows there is a strong correlation between the number of children and 
# the number of members in the family
# right now, this information isn't really meaningful for our goal

# ********** Clustering ************* #

# Our first goal is to analyze good and bad clients
# The first question is : is there any cluster ? 
# As so, let's perform clustering on application 

# Z-normalization
for (i in c(5, 6, 12, 18)){
  i.mean <- sapply(my_data[i], mean)
  i.std <- sapply(my_data[i], sd)
  my_data[i] <- (my_data[i] - i.mean) / i.std 
}

summary(my_data)

for (i in 1:5){
  clf <- kmeans(my_data[c(5,6,12,18)], 2)
  print(clf)
  plot(my_data[c(5,6)], col=clf$cluster)
}

plot(my_data[c(5,6)], col=clf$cluster)
plot(my_data[c(5,12)], col=clf$cluster)
plot(my_data[c(6,12)], col=clf$cluster)

# we tried to project the clusters on different axes to get the meaning of the clusters
# and it's still confused

# let's try with 3 clusters
for (i in 1:5){
  clf <- kmeans(my_data[c(5,6,12,18)], 3)
  print(clf)
  plot(my_data[c(5,6)], col=clf$cluster)
}

# it's still confused
# we see that we can't reach our goal just by using the application data set.
# now let's make use of credits data set

# **************** "Good" and "bad" clients ************* #

# we consider as good clients, clients who have less than 1 month overdue

# intersection between the two datasets
length(dplyr::intersect(my_data$ID, credits$ID)) # 36457 common values

# exploration
colnames(credits)

# get the intersection 
temp <- dplyr::intersect(my_data$ID, credits$ID)
temp %>% head(10)
intersect <- credits[credits$ID %in% temp,]
intersect %>% head(100)
dim(intersect[intersect$STATUS !=0, ])
dim(intersect[intersect$STATUS !=1, ])

# we're in the shoes of a business person
# and we want to minimize lose/overdues
# As so, bad clients are those who have more than 30 days overdue

# set to 1 cards with >= 30 days past due and 0 else
intersect$STATUS <- ifelse((intersect$STATUS == 1 | intersect$STATUS == 2 | intersect$STATUS == 3 | intersect$STATUS == 4
                            | intersect$STATUS == 5), 1, 0)
unique(intersect$STATUS)

# we'll work on those rows

# group the data by ID
credit_grouped <- intersect %>% dplyr::group_by(ID)
credit_grouped %>% head(10)

# ********* On this part, our goal is to look at the evolution
# ********* of the credit account's balance along the time

# 1. create a wide version of credits dataframe
# this will help us to see the status of the account along the time
pivot <- spread(intersect, key="MONTHS_BALANCE",
                value="STATUS")
head(pivot)
dim(pivot)

# we want to get smallest value of MONTHS_BALANCE which is the month the account was created
granted <- credit_grouped %>% summarise(MONTHS_BALANCE = min(MONTHS_BALANCE))
pivot$opening_months <- granted$MONTHS_BALANCE
pivot$opening_months

# biggest value of MONTHS_BALANCE is how long the account has been created till we get the data
end <- credit_grouped %>% summarise(MONTHS_BALANCE = max(MONTHS_BALANCE))
pivot$end_month <- end$MONTHS_BALANCE
pivot$end_month
pivot %>% head(10)

zero <- credit_grouped %>% summarise(nb_zeros = sum(STATUS == 0))
zero

ones <- credit_grouped %>% summarise(nb_ones = sum(STATUS == 1))
ones

pivot$nb_zeros <- zero$nb_zeros
pivot$nb_ones <- ones$nb_ones

colnames(pivot)

# 2. focus on just the evolution of the account
# create a new table with just ID, open_month and end_month as columns
# data.table are faster than simple data.frame
table_window <- data.table(
  ID = pivot$ID,
  opening_month = pivot$opening_months,
  end_month = pivot$end_month,
  nb_ones = pivot$nb_ones,
  nb_zeros = pivot$nb_zeros
)

dim(table_window)

head(table_window)

# number of closed accounts
dim(table_window[table_window$end_month != 0])

# 3. create the window : it's the number of months since the account has been opened
table_window$window <- table_window$end_month - table_window$opening_month
head(table_window)

intersect <- left_join(intersect, table_window, by="ID")
colnames(intersect)
head(intersect, 30)

# ***** now let's go ahead

# first save the credits data table because we will perform series of operations
intersect_save <- copy(intersect)
head(intersect_save)

# 4. compute month on book : elapsed time between the opening month and the month balance
# with this column, we'll make conclusions like "10 months after the opening of his account,
# his status was X"
intersect$month_on_book <- intersect$MONTHS_BALANCE - intersect$opening_month

# ordering
setorder(as.data.table(intersect), ID, month_on_book)
intersect %>% head(10)

# **************** Analysis : what makes them different ? ******************* #

# make sure not to have 2 values of STATUS for the same ID

dim(intersect[intersect$STATUS !=0, ])
dim(intersect[intersect$STATUS !=1, ])

# create a new data set which contains intersect's info columns and its correspondent rows in application
appli_inter <- left_join(intersect, my_data, by="ID")
appli_inter %>% head(20)
colnames(appli_inter)

# let's delete some useless columns(flag_mobile, flag_phone, flag_*)
appli_inter <- subset(appli_inter, select = -c(2,21,22,23,24))
appli_inter <- subset(appli_inter, select = -c(7,8))

appli_inter %>% head(10)

appli_inter <- appli_inter %>% distinct(ID, .keep_all = TRUE)
appli_inter %>% head(10)

# no debt clients
dim(appli_inter[appli_inter$STATUS == 0,]) # (36290, 2)

# we want to understand what make some clients better on their finance management than others

# first let us see if clustering is better on those data
for (i in 1:5){
  clf <- kmeans(appli_inter[c(10,11,16,17,19)], 2)
  print(clf)
  plot(appli_inter[c(10,11)], col=clf$cluster)
}

# the clusters are still mixed

unique(appli_inter$OCCUPATION_TYPE)

# collect the "low" occupation types : those are the persons we're interested on
occupations = c("Core staff","Security staff", "Waiters/barmen staff", "Cleaning staff", "Laborers", "Cooking staff", "Low-skill Laborers", "Drivers")

# we have 14087 persons belonging to this category
dim(appli_inter[appli_inter$OCCUPATION_TYPE %in% occupations,])

# among those 14087 persons, 14019 are "good" clients; which is really interesting
dim(appli_inter[appli_inter$OCCUPATION_TYPE %in% occupations & appli_inter$nb_zeros > appli_inter$nb_ones,])
appli_inter_w <- appli_inter[appli_inter$OCCUPATION_TYPE %in% occupations,]
colnames(appli_inter_w)
dim(appli_inter_w)

# let us take only our rows of interest
X <- subset(appli_inter_w, select=-c(1,2,3,4,5,6))
dim(X)
y <- as.factor(ifelse(appli_inter_w$nb_ones >= appli_inter_w$nb_zeros, 1, 0))
length(y)
describe(X)

# *********** Random Forest ****************** #

# In order to know which personal information to rely on
# to try to explain the status of each category, we'll use
# random forest to know the important variables

# ** distribution of status
colnames(appli_inter)
no_debt <- length(class[class==0])
debt <- length(class[class==1])
status <- c("No debt", "Debt")
values <- c(no_debt, debt)
sm_df <- data.frame(status, values)
sm_df
ggplot(sm_df, aes(x = status, y = values, fill=status)) + geom_bar(stat = "identity")

# highly imbalanced

# before diving in the forest, we noticed that we have a lot of categorical data
# we then need to make dummy variables

# set ids as rownames
rownames(X) <- appli_inter_w$ID
head(X)
X <- dummy_cols(X, remove_selected_columns = TRUE)

X.sample <- X[sample(nrow(X)),]

# collect covariate and pseudo-covariate matrices
appli_inter.pseudo <- data.frame(cbind(X, X.sample, y))
p <- ncol(X)

# parallelize computation for random forest
VIMs.unb <- function(k) {
  set.seed(k)
  appli_inter2.sample <- X[sample(nrow(X)),]
  appli_inter2.pseudo <- data.frame(cbind(X, appli_inter2.sample,y))
  appli_inter2.rf <- randomForest(y ~ ., data=appli_inter2.pseudo, ntree=500)
  VIMs <- importance(appli_inter2.rf, type=2)
  VIMs[1:p,] - VIMs[(p+1):(2*p),] 
}

sfInit(parallel = TRUE, cpus=3, type="SOCK")
sfLibrary(randomForest)
sfExport("X", "y", "p")
VIMs.list <- sfLapply(x=1:50, VIMs.unb)
sfStop()

VIMs <- t(matrix(unlist(VIMs.list),p))
GINI.unb <- apply(VIMs, 2, mean)

# visualization
idx <- order(GINI.unb, decreasing = T)
idx
Xs <- X[, idx[1:10]]
Xs
colnames(X)
vm <- c(VIMs[,idx])
grp <- c(t(matrix(rep(1:ncol(VIMs), nrow(VIMs)), ncol(VIMs))))
dt <- data.frame(vm, grp=factor(grp))
ggplot(dt, aes(grp, vm)) + geom_boxplot(outlier.size = 0) +
  scale_x_discrete(breaks=c(1,100,200,300,400,p), name="")+
  scale_y_continuous(name="GINI VIM corrected") +
  geom_hline(yintercept = 0, colour="red", lty=2, lwd=1)+
  theme(text = element_text(size=24))

appli_inter_imp <- X[, idx[1:12]]
save(appli_inter_imp, file="important_covariates.RData")

# According to the analysis, the most important variables
# are : Pensioner, Security staff, Low-skill Laborers

# And it turns out that their credit card status is linked to their income,
# the number of days employed, their marital status and also their age

# let us take a look to those datas
appli_inter_imp[appli_inter_imp$AMT_INCOME_TOTAL == 
                  max(appli_inter_imp$AMT_INCOME_TOTAL),]
appli_inter_imp[appli_inter_imp$AMT_INCOME_TOTAL == 
                  min(appli_inter_imp$AMT_INCOME_TOTAL),]
appli_inter_imp[appli_inter_imp$DAYS_BIRTH == 
                  min(appli_inter_imp$DAYS_BIRTH),]
appli_inter_imp[appli_inter_imp$DAYS_BIRTH == 
                  max(appli_inter_imp$DAYS_BIRTH),]
appli_inter[appli_inter$AMT_INCOME_TOTAL == 
                  min(appli_inter$AMT_INCOME_TOTAL),]

# exploring the data shows that those variables have a reverse impact
# as pensioner, security staff, status singe are set to 0
# means that we should be more attracted by people who don't belong
# to those categories
# Moreover, older people have a better money management

# ** Modeling
# Because we mostly have categorical variables, we're going to use random 
# forest for modeling
appli_inter2.ind <- data.frame(appli_inter_imp, y)
set.seed(10)
# training indices
index <- sample(1:nrow(appli_inter2.ind), 4500)
train <- appli_inter2.ind[index,]
test <- appli_inter2.ind[-index,]
clusters <- parallel::makeCluster(spec=3, type="PSOCK")
registerDoParallel(clusters)

# parameters for train function
param_train <- trainControl(method="repeatedcv", number = 5, repeats = 5)

# tune our model
fit_rf <- train(y ~ .,
                data=train, 
                method = "rf",
                metric = "Accuracy",
                tuneGrid = expand.grid(.mtry=1:6),
                trControl = param_train,
                ntree = 500)
stopCluster(clusters)

print(fit_rf)
plot(fit_rf)
colnames(test)
colnames(train)

# prediction
y_rf <- predict(fit_rf$finalModel, newdata = test, type="class")
y_rf

# ________ THIS IS THE END _____________________ #