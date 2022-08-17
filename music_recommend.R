

install.packages('arules')
install.packages('arulesViz')

library(readr)
library(dplyr)
library(arules)
library(arulesViz)

#read
df0=read.csv("music.csv", header = T)

df0$customer=factor(df0$customer)
df0$artist=factor(df0$artist)
df0$sex=factor(df0$sex)
df0$country=factor(df0$country)
head(df0,19)

df=select(df0, customer, artist)
#553 songs from unknown artists are in the dataset.
df %>% 
  count(artist=='[unknown]')
#remove all rows with unknown artists name from df
df=filter(df,artist!='[unknown]')

# create a list of users (each with the songs of their preference)
playlist = split(df$artist,f=df$customer)
# see first user’s songs
playlist[[1]]
# see first two users songs
playlist[1:2]
# remove song duplicates, if any
playlist = lapply(playlist,unique)
# convert the list of users to transactions object
df.trans <- as(playlist,"transactions")


df1 = data.frame(inspect(df.trans[1:2])) # 16for 1st transaction，29 for 3rd transaction
size=size(df.trans[1:2])
size


itemFrequencyPlot(df.trans,support=.08,cex.names=1.5)
print(paste('radiohead is the most popular artist'))

list1 = list(supp = 0.01, conf = 0.5, target = "rules")
rules <- apriori(df.trans, parameter = list1)
inspect(rules)


rules1=subset(rules,lift>=5)
rules1_sorted = sort(rules1, decreasing = TRUE,by = "confidence")
inspect(rules1_sorted)
print(paste('If led zeppelin and the doors are purchased, then 0.60 likely pink floyd will be purchased,too. 
            led zeppelin, the doors, and pink floyd are 1% of transactions,
            led zeppelin, the doors, and pink floyd are 5.7 more likely to be purchased than not.'))


