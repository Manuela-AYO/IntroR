# ******** Upload the data ****************
lastfm <- read.csv("~/M1/DM/PS2/Data/lastfm.csv")

# ******** EDA ***********************
head(lastfm)
dim(lastfm) # (289955, 4)
col(lastfm)
lastfm[1:20,]
length(unique(lastfm$country)) # artists from 159 different countries
length(lastfm$user)
lastfm$user <- factor(lastfm$user) # each user is now a category
lastfm$user
levels(lastfm$user)
levels(lastfm$artist)
length(unique(lastfm$user)) # 15000 different artists
summary(lastfm) # 4 columns : user, artist, sex and country
length(unique(lastfm$artist)) # 1004 different artists
table(lastfm$sex) # 78132 "f" for 211823 "m" 
# men actually represent more than a half of the dataset

# ********* Association Rule Mining **********
library(Matrix)
library(arules) # package for mining association rules and item sets identification

# let us transform the data into an incidence matrix
# with listeners as rows and artists as columns
# minsup = 0.08 
playlist <- split(x=lastfm[,"artist"], f=lastfm$user) # like a groupby user
playlist[1:2]

# An artist may be mentioned by the same user more than once, so it is important to remove artist duplicates
# before creating the incidence matrix
playlist <- lapply(playlist, unique) 
typeof(playlist)
playlist <- as(playlist, "transactions") # transform list playlist to transactions table
typeof(playlist)
playlist

# item frequency of each artist with a support >= 0.08
itemFrequency(playlist)
X11(width=10, height = 10)
itemFrequencyPlot(playlist, support=.08, cex.names=1.5)
# artists with sup >= 0.08 :
# - coldplay, radiohead, the beatles : > 0.15
# - metallica, muse, red hot chili peppers : ]0.10, 0.15[
# - death cab for cutie, linkin park, nirvana, pink floyd, system of a down, the killers : [0.08, 0.10]

# let us build the association rules(with Apriorii algorithm)
# minsupport = 0.01, conf = 0.50
musicrules <- apriori(playlist, parameter=list(support=0.01,
                                               confidence=.5))
inspect(musicrules)

# let us filter by lift > 5
# lift is the ratio P(B|A)/P(B) which compares how the presence
# of A impacts B. If this ratio is greater than 1, it means 
# A is an upward lift on B
inspect(subset(musicrules, subset=lift>5))

# order by confidence
inspect(sort(subset(musicrules, subset=lift>5), by="confidence"))

# * -- association rules visualization
library(arulesViz)
X11(width = 10, height = 10)
plot(musicrules)
sel <- plot(musicrules, interactive = TRUE)
plot(musicrules, shading = "order", control = list(main="Two-key plot"))
subrules <- musicrules[quality(musicrules)$confidence > .5]
plot(subrules, method="matrix", measure = "lift")
plot(subrules, method="matrix", engine="3d", measure = "lift")
plot(musicrules, method="grouped")
