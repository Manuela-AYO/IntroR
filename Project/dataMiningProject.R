# *** This project is all about classification of credit card 
# *** applicant : is an applicant good or bad ? In other means, 
# *** is it risky or not to provide an applicant a credit card ?
# *** The particularity of this classification task is that 
# *** we don't have the target class because there is not 
# *** a criteria to predict whether an applicant is good or bad
# *** That's the reason why banks use vintage analysis which
# *** aims to observe the behavior of a client on a time

# ********* Libraries ******** #
library(magrittr)
library("tidyr")
library("tidyverse")
library(data.table)
library(ggplot2)
library("viridisLite")
library("viridis")
library(corrplot)

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

# 1. Z-normalization
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


# intersection between the two datasets
length(dplyr::intersect(my_data$ID, credits$ID)) # 36457 common values

# group the data by credit
credit_grouped <- credits %>% dplyr::group_by(ID)
credit_grouped %>% head(10)

# ********* On this part, our goal is to look at the evolution
# ********* of the credit account's balance along the time

# 1. create a wide version of credits dataframe
# this will help us to see the status of the account along the time
pivot <- spread(credits, key="MONTHS_BALANCE",
                value="STATUS")
head(pivot)

# smallest value of MONTHS_BALANCE is the month the account was created
granted <- credit_grouped %>% summarise(MONTHS_BALANCE = min(MONTHS_BALANCE))
pivot$begin_months <- granted$MONTHS_BALANCE
pivot$begin_months

# biggest value of MONTHS_BALANCE is how long the account has been created
# till we get the data
end <- credit_grouped %>% summarise(MONTHS_BALANCE = max(MONTHS_BALANCE))
pivot$end_month <- end$MONTHS_BALANCE
pivot$end_month
pivot[1:30,]

colnames(pivot)

pivot <- pivot %>% rename_at('begin_months', ~'opening_month')
colnames(pivot)

# 2. focus on just the evolution of the account
# create a new table with just ID, open_month and end_month as columns
# data.table are faster than simple data.frame
table_window <- data.table(
  ID = pivot$ID,
  opening_month = pivot$opening_month,
  end_month = pivot$end_month
)

dim(table_window)

head(table_window)

# 3. create the window : it's the number of months since the account has been opened
table_window$window <- table_window$end_month - table_window$opening_month
head(table_window)

credits <- left_join(credits, table_window, by="ID")
colnames(credits)
head(credits, 30)

# ***** now let's go ahead

# first save the credits data table because we will perform a series of operations
credits_save <- copy(credits)
head(credits_save)

# 4. delete users whose observe window less than 20(new clients)
# we consider we don't have enough info to know if they are great clients or not
dim(credits)
credits <- credits[credits$window > 20,]
dim(credits)
unique(credits$STATUS)

# 5. we're going to analyze more than 60 days past due
# set to 1 cards with > 60 days past due and 0 else
credits$STATUS <- ifelse((credits$STATUS == 2 | credits$STATUS == 3 | credits$STATUS == 4
| credits$STATUS == 5), 1, 0)
unique(credits$STATUS)

# 6. compute month on book : elapsed time between the opening month and the month balance
# with this column, we'll make conclusions like "10 days after the acquisition of his card,
# his status was X"
credits$month_on_book <- credits$MONTHS_BALANCE - credits$opening_month

# ordering
setorder(as.data.table(credits), ID, month_on_book)
credits[200:290,]


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
