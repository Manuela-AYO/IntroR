# *** This project is all about classification of credit card 
# *** applicant : is an applicant good or bad ? In other means, 
# *** is it risky or not to provide an applicant a credit card ?
# *** The particularity of this classification task is that 
# *** we don't have the target class because there is not 
# *** a criteria to predict whether an applicant is good or bad
# *** That's the reason why banks use vintage analysis which
# *** aims to observe the behavior of a client on a time

# https://statisticsglobe.com/barplot-in-r

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

# ******** EDA *************** #
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

# missing values ? 
sapply(my_data, function(x) sum(is.na(x))) # we don't have any missing value

# correlation
correl = cor(my_data2[c(5,6,12,18)])
x11(width = 10, height = 10)
corrplot(correl, order='hclust', addrect=2, method="number")

# this corrplot shows there is a strong correlation between the number of children and 
# the number of members in the family
# right now, this information isn't really meaning for our goal

# ********** Clustering ************* 

# Our first goal is to analyze good and bad clients
# The first question is : is there any cluster ? 
# As so, let's perform clustering on application 

my_data2 <- data.frame(my_data)

# Z-normalization
for (i in c(5, 6, 12, 18)){
  i.mean <- sapply(my_data2[i], mean)
  i.std <- sapply(my_data2[i], sd)
  my_data2[i] <- (my_data2[i] - i.mean) / i.std 
}

summary(my_data2)

for (i in 1:5){
  clf <- kmeans(my_data2[c(5,6,12,18)], 2)
  print(clf)
  plot(my_data2[c(5,6)], col=clf$cluster)
}

plot(my_data2[c(5,6)], col=clf$cluster)
plot(my_data2[c(5,12)], col=clf$cluster)
plot(my_data2[c(6,12)], col=clf$cluster)

# we tried to project the clusters on different axes to get the meaning of the clusters
# and it's still confused

# let's try with 3 clusters
for (i in 1:5){
  clf <- kmeans(my_data2[c(5,6,12,18)], 3)
  print(clf)
  plot(my_data2[c(5,6)], col=clf$cluster)
}

# it's still confused
# we see that we can't reach our goal just by using the application data set.
# now let's make use of credits data set

# **************** "Good" and "bad" clients *************

# we consider as good clients, clients who have less than 1 month overdue

# intersection between the two datasets
length(dplyr::intersect(my_data$ID, credits$ID)) # 36457 common values

# exploration
colnames(credits)

# get the intersection 
temp <- dplyr::intersect(my_data$ID, credits$ID)
temp %>% head(10)
intersect <- credits[credits$ID %in% temp,]
intersect %>% head(30)
dim(intersect)
length(unique(intersect$ID))

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

# we want to get 
# smallest value of MONTHS_BALANCE is the month the account was created
granted <- credit_grouped %>% summarise(MONTHS_BALANCE = min(MONTHS_BALANCE))
pivot$opening_months <- granted$MONTHS_BALANCE
pivot$opening_months

# biggest value of MONTHS_BALANCE is how long the account has been created
# till we get the data
end <- credit_grouped %>% summarise(MONTHS_BALANCE = max(MONTHS_BALANCE))
pivot$end_month <- end$MONTHS_BALANCE
pivot$end_month
pivot %>% head(10)

colnames(pivot)

# 2. focus on just the evolution of the account
# create a new table with just ID, open_month and end_month as columns
# data.table are faster than simple data.frame
table_window <- data.table(
  ID = pivot$ID,
  opening_month = pivot$opening_months,
  end_month = pivot$end_month
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

# 4. delete users whose observe window less than 20(new clients)
# we consider we don't have enough info to know if they are great clients or not
# dim(credits)
# credits <- credits[credits$window > 20,]
# dim(credits)
# unique(credits$STATUS)

# 5. we're in the head of a financial institution
# and we want to minimize overdues
# so we're going to analyze more than 30 days(1 month) past due

# set to 1 cards with >= 30 days past due and 0 else
intersect$STATUS <- ifelse((intersect$STATUS == 1 | intersect$STATUS == 2 | intersect$STATUS == 3 | intersect$STATUS == 4
| intersect$STATUS == 5), 1, 0)
unique(intersect$STATUS)

# 6. compute month on book : elapsed time between the opening month and the month balance
# with this column, we'll make conclusions like "10 months after the opening of his account,
# his status was X"
intersect$month_on_book <- intersect$MONTHS_BALANCE - intersect$opening_month

# ordering
setorder(as.data.table(intersect), ID, month_on_book)
intersect %>% head(10)

# **************** Analysis : what make them different ? ******************* 

# create a new data set which contains intersect's info columns and its correspondent rows in application
appli_inter <- left_join(intersect, my_data2, by="ID")
appli_inter %>% head(20)

# let's delete some useless columns(flag_mobile, flag_phone, flag_*)
appli_inter <- subset(appli_inter, select = -c(22, 21, 20, 19))

temp <- dplyr::distinct(appli_inter, ID, STATUS)

# no debt clients
dim(temp[temp$STATUS == 0,]) # (36456, 2)

# debt clients
dim(temp[temp$STATUS == 1,]) # (4291, 2)

# visualization of the amount of each class
status <- c("No Debt", "Debt")
status
values <- c(dim(temp[temp$STATUS == 0,])[1], dim(temp[temp$STATUS == 1,])[1])
values
sm_df <- data.frame(status, values)
sm_df
viz <- ggplot(sm_df, aes(y=values, x=status, fill=status)) + geom_bar(stat="identity")
viz
# ** comments : the data set is imbalanced but we still have more than 4000 data to analyze debt clients

# we want to understand what make some clients better on their finance management than others

# 1. first let us see if clustering is better on those data
ncol(appli_inter)
head(appli_inter)

sapply(appli_inter, function(x) sum(is.na(x)))

for (i in 1:5){
  clf <- kmeans(appli_inter[c(11,12,18,20)], 2)
  print(clf)
  plot(appli_inter[c(3,12)], col=clf$cluster)
}

# the clustering is still mixed

# 2. Decision tree
# Decision Trees are white-box in machine learning
# We'll build a decision tree to have a first intuition of how
# we should go

appli_inter2 <- subset(appli_inter, select = -c(2,4,5,6,7))
colnames(appli_inter2)
appli_inter2 <- dplyr::distinct(appli_inter2)
head(appli_inter2, 50)

# we observe that there are duplicates in the data
# and we'll delete them because they are a huge source of bias
temp3 <- appli_inter2[!duplicated(appli_inter2), ]
nrow(appli_inter2)
nrow(temp3)
set.seed(2568)
n <- nrow(appli_inter) # number of rows
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

# _______________________________________________________________________________________________________________________________________________________________________________

# 7. compute the denominator
# count how many users opened the account on each month
# will help us compute the due rate after
denominator <- aggregate(table_window$opening_month, by=list(table_window$opening_month), FUN=length)
denominator[1:5,]
denominator <- denominator %>% rename_at("Group.1" , ~"opening_month")
denominator <- denominator %>% rename_at("x" , ~"sum")
colnames(denominator)

# 8. compute the vintage table
# now we'll see the due rate per month and at some discrete periods
vintage <- aggregate(credits$month_on_book, 
                     by=list(credits$month_on_book, credits$opening_month), FUN=length)
vintage <- vintage %>% rename_at("Group.2", ~"opening_month")
vintage <- vintage %>% rename_at("Group.1", ~"month_on_book")
vintage[1:5,]
vintage <- left_join(vintage, denominator, by="opening_month")
vintage[1:5,]

# delete sum.x
vintage <- vintage %>% select(-x)
colnames(vintage)
dim(vintage)
dim(vintage)[1]
vintage$due_count <- rep(0, dim(vintage)[1])
vintage[1:5,]

# compute the due over months
# on each month
for(i in -60:0){
  l = list()
  # for each month on each book
  for (j in 0:60){
    # take the cards which are due
    due <- credits[credits$STATUS == 1 & credits$month_on_book == j &
               credits$opening_month  == i,"ID"]
    l <- append(l, list(due))
    vintage[(vintage$month_on_book == j) & (vintage$opening_month) == i, "due_count"] <- length(unique(l))
  }
}

vintage$rate <- vintage$due_count / vintage$sum
vintage[1:5,]

# ********** Visualization
colnames(vintage)

# a. Put the matrix on the wide form
vintage_wide <- vintage %>% select(-one_of('sum', 'due_count'))
vintage_wide <- pivot_wider(vintage_wide,
                            names_from = "month_on_book",
                            values_from = "rate")
print(vintage_wide, n=60)
vintage_wide[1:60, 50:60]

l <- list()
for (col in colnames(vintage_wide[2:62])){
  l <- c(l,vintage_wide[[col]])
}
print(length(l))

l <- rapply(l, function(x) ifelse(is.na(x),0,x))

temp <- rep(0, nrow(vintage_wide))
print(length(temp))
for (col in c(1:60)){
  print(col)
  temp <- c(temp,
            rep(col, nrow(vintage_wide)))
  print(length(temp))
}
print(length(temp))

# b. reshape the matrix for ggplot
df_resh <- data.table(
  month_on_book = c(0:60),
  cumul_rate = l,
  group = temp
)
length(vintage_wide[["0"]])

df_resh[1:5,]

# c. Plottiiiiiiing
X11(width=10, height = 10 )
my_plot <- ggplot(df_resh, aes(month_on_book, cumul_rate)) + geom_line(aes(colour=group)) +
  scale_color_viridis() + theme(legend.position = "top") + 
  labs(title = "Cumulative % of bad customers(>60 days past due)")
my_plot

ratios <- list()
for (i in c(0:60)){
  ratio = length(table_window[table_window$window < i]) / length(unique(table_window$ID))
  ratios[[length(ratios)+1]] <- ratio
}
ratios
plot(c(0:60), ratios)

temp <- dplyr::intersect(my_data$ID, credits$ID)
length(temp)
