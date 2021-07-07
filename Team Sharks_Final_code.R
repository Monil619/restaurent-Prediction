library(tidyverse)
library(tidyr)
library(igraph)
library(ggraph)
library(NLP)
library(tm)
library(dplyr)
library(syuzhet)
library(plotly)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(janeaustenr)
library(reshape2)
library(tidytext)
library(dplyr)
library(tm)
library(wordcloud)
library(tm)
library(dplyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(NLP)
library(tm)

#loding dataset
df <- read.csv('C:/sem 2/EM-622/ProjectData.csv/ProjectData.csv')

#.............................................................................Sentiment analysis.......................................................................


filter_data = df %>% select(name, online_order, rate ,reviews_list ) 

#Build corpus

corpus = iconv(filter_data$reviews_list)
corpus = Corpus(VectorSource(corpus))

#Text Cleaning
corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeNumbers)

cleanset = tm_map(corpus, removeWords, stopwords("english"))

cleanset = tm_map(corpus, removeWords, c("rated", "ratedn"))

cleanset = tm_map(cleanset, stripWhitespace)

cle <- data.frame(text = get("content", cleanset))

#Reading
words = iconv(cle$text)

# get the emotions using the NRC dictionary
s <- get_nrc_sentiment(words)

#Bar plot
barplot(100*colSums(s)/sum(s),
        las = 2,
        col = rainbow(10),
        ylab = "percentage",
        main = 'Sentiment Scores for Top 10 Restaurants Reviews')


docs <- Corpus(VectorSource(words))
# take all the phrases
docs1 <-tibble(phrases =docs$content)

# add an id, from 1 to n
docs1$ID <- row.names(docs1)

# split all the words
tidy_docs <- docs1 %>% unnest_tokens(word, phrases)
View(tidy_docs)

#create now the cloud: a pair of warnings, because you do not have negative words and it is joining by word(correct)
tidy_docs %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "black"), scale=c(9,.5),
                   max.words = 200)

count = tidy_docs %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
View(count)



#...............................................................................Word cloud...........................................................
df <- read.csv('C:/sem 2/EM-622/ProjectData.csv/ProjectData.csv')
View(df)

#wordcloud
#Considering dishes liked from the best resto
filter_data = df %>% filter(name %in% c ('Onesta','Faasos','Empire Restaurant','Smallys Resto Cafe','Faasos','Corner House Ice Cream','Burger King','Polar Bear','Smoor','Mani Dum Biryani','KFC')) %>%select(name, dish_liked, cuisines)
str(filter)

#Build corpus
corpus = iconv(filter_data$dish_liked)
corpus = Corpus(VectorSource(corpus))
inspect(corpus[1:2])

#Text Cleaning
corpus = tm_map(corpus, tolower)
inspect(corpus[1:2])

corpus = tm_map(corpus, removePunctuation)
inspect(corpus[1:2])

corpus = tm_map(corpus, removeNumbers)
inspect(corpus[1:2])

corpus = tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus[1:2])

cleanset = tm_map(corpus, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

cleanset = tm_map(cleanset, stripWhitespace)

tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]
tdm
w = rowSums(tdm)
w

#wordcloud plot of dishes liked from best resto
cloud = sort(rowSums(tdm), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 50,
          random.order = F,
          colors = brewer.pal(n = 11,name = 'Spectral'),
          scale = c(4, 0.7),
          rot.per = 0.15, main = 'Dishes Liked ')
#----------------#
Onesta = filter_data%>%select(dish_liked,cuisines,rest_type)

docs = iconv(Onesta$dish_liked)
docs = Corpus(VectorSource(docs))
inspect(docs[1:2])

#Text Cleaning
docs = tm_map(docs, tolower)
inspect(docs[1:2])

docs = tm_map(docs, removePunctuation)
inspect(docs[1:2])

docs = tm_map(docs, removeNumbers)
inspect(docs[1:2])

docs = tm_map(docs, removeWords, stopwords("english"))
inspect(docs[1:2])

c.onesta = tm_map(docs, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

c.onesta = tm_map(docs, stripWhitespace)

tdm1 <- TermDocumentMatrix(c.onesta)
tdm1
tdm1 <- as.matrix(tdm1)
tdm1[1:10, 1:20]
tdm1
v = rowSums(tdm1)
v

#wordcloud plot of dishes liked from best resto
cloud = sort(rowSums(tdm), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v),
          freq = v,
          max.words = 150,
          random.order = F,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(4, 0.7),
          rot.per = 0.15, main = 'Liked Dishes from Onesta')

#-------------------#
Onesta = filter_data%>%select(dish_liked,cuisines,rest_type)

docs = iconv(Onesta$dish_liked)
docs = Corpus(VectorSource(docs))
inspect(docs[1:2])

#Text Cleaning
docs = tm_map(docs, tolower)
inspect(docs[1:2])

docs = tm_map(docs, removePunctuation)
inspect(docs[1:2])

docs = tm_map(docs, removeNumbers)
inspect(docs[1:2])

docs = tm_map(docs, removeWords, stopwords("english"))
inspect(docs[1:2])

c.onesta = tm_map(docs, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

c.onesta = tm_map(docs, stripWhitespace)

tdm1 <- TermDocumentMatrix(c.onesta)
tdm1
tdm1 <- as.matrix(tdm1)
tdm1[1:10, 1:20]
tdm1
v = rowSums(tdm1)
v

#wordcloud plot of dishes liked from best resto
cloud = sort(rowSums(tdm), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v),
          freq = v,
          max.words = 150,
          random.order = F,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(4, 0.7),
          rot.per = 0.15, main = 'Liked Dishes from Onesta')

#----------------#
Onesta = filter_data%>%select(dish_liked,cuisines,rest_type)

docs = iconv(Onesta$dish_liked)
docs = Corpus(VectorSource(docs))
inspect(docs[1:2])

#Text Cleaning
docs = tm_map(docs, tolower)
inspect(docs[1:2])

docs = tm_map(docs, removePunctuation)
inspect(docs[1:2])

docs = tm_map(docs, removeNumbers)
inspect(docs[1:2])

docs = tm_map(docs, removeWords, stopwords("english"))
inspect(docs[1:2])

c.onesta = tm_map(docs, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

c.onesta = tm_map(docs, stripWhitespace)

tdm1 <- TermDocumentMatrix(c.onesta)
tdm1
tdm1 <- as.matrix(tdm1)
tdm1[1:10, 1:20]
tdm1
v = rowSums(tdm1)
v

#wordcloud plot of dishes liked from best resto

cloud = sort(rowSums(tdm), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v),
          freq = v,
          max.words = 150,
          random.order = F,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(4, 0.7),
          rot.per = 0.15, main = 'Liked Dishes from Onesta')

#----------# onesta
Onesta = subset(filter_data,name == 'Onesta')

docs = iconv(Onesta$dish_liked)
docs = Corpus(VectorSource(docs))
inspect(docs[1:2])

#Text Cleaning
docs = tm_map(docs, tolower)
inspect(docs[1:2])

docs = tm_map(docs, removePunctuation)
inspect(docs[1:2])

docs = tm_map(docs, removeNumbers)
inspect(docs[1:2])

docs = tm_map(docs, removeWords, stopwords("english"))
inspect(docs[1:2])

c.onesta = tm_map(docs, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

c.onesta = tm_map(docs, stripWhitespace)

tdm1 <- TermDocumentMatrix(c.onesta)
tdm1
tdm1 <- as.matrix(tdm1)
tdm1[1:10, 1:20]
tdm1
v = rowSums(tdm1)
v

#wordcloud plot of dishes liked from best resto
cloud1 = sort(rowSums(tdm), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v),
          freq = v,
          max.words = 150,
          random.order = F,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(4, 0.7),
          rot.per = 0.15, main = 'Liked Dishes from Onesta')

#------------------#Faasos
Faasos = subset(filter_data,name == 'Faasos')

docs1 = iconv(Faasos$dish_liked)
docs1 = Corpus(VectorSource(docs))
inspect(docs1[1:2])

#Text Cleaning
docs1 = tm_map(docs1, tolower)
inspect(docs1[1:2])

docs1 = tm_map(docs1, removePunctuation)
inspect(docs1[1:2])

docs1 = tm_map(docs1, removeNumbers)
inspect(docs1[1:2])

docs1 = tm_map(docs1, removeWords, stopwords("english"))
inspect(docs1[1:2])

c.faasos = tm_map(docs, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

c.faasos = tm_map(docs, stripWhitespace)

tdm2 <- TermDocumentMatrix(c.faasos)
tdm2
tdm2 <- as.matrix(tdm2)
tdm2[1:10, 1:20]
tdm2
v2 = rowSums(tdm2)
v2

#wordcloud plot of dishes liked from best resto
cloud2 = sort(rowSums(tdm2), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v2),
          freq = v2,
          max.words = 150,
          random.order = F,
          colors = brewer.pal(11, 'RdYlGn'),
          scale = c(4, 0.7),
          rot.per = 0.15, main = 'Liked Dishes from Faasos')
#------------# Empire Restaurant
ER = subset(filter_data,name == 'Empire Restaurant')

docs2 = iconv(ER$dish_liked)
docs2 = Corpus(VectorSource(docs2))
inspect(docs2[1:2])

#Text Cleaning
docs2 = tm_map(docs2, tolower)
inspect(docs2[1:2])

docs2 = tm_map(docs2, removePunctuation)
inspect(docs1[1:2])

docs2 = tm_map(docs2, removeNumbers)
inspect(docs2[1:2])

docs2 = tm_map(docs2, removeWords, stopwords("english"))
inspect(docs2[1:2])

c.er = tm_map(docs2, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

c.er = tm_map(docs2, stripWhitespace)

tdm3 <- TermDocumentMatrix(c.er)
tdm3
tdm3 <- as.matrix(tdm3)
tdm3[1:10, 1:20]
tdm3
v3 = rowSums(tdm3)
v3

#wordcloud plot of dishes liked from best resto
cloud3 = sort(rowSums(tdm3), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v3),
          freq = v3,
          max.words = 150,
          random.order = F,
          colors = brewer.pal(8, 'Accent'),
          scale = c(4, 0.7),
          rot.per = 0.15, main = 'Liked Dishes from Empire Restaurant')

#---------#Corner House Ice Cream
CHIC = subset(filter_data,name == 'Corner House Ice Cream')

docs3 = iconv(CHIC$dish_liked)
docs3 = Corpus(VectorSource(docs3))
inspect(docs1[1:2])

#Text Cleaning
docs3 = tm_map(docs3, tolower)
inspect(docs3[1:2])

docs3 = tm_map(docs3, removePunctuation)
inspect(docs3[1:2])

docs3 = tm_map(docs3, removeNumbers)
inspect(docs3[1:2])

docs3 = tm_map(docs3, removeWords, stopwords("english"))
inspect(docs3[1:2])

chic = tm_map(docs3, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

chic = tm_map(docs3, stripWhitespace)

tdm3 <- TermDocumentMatrix(chic)
tdm3
tdm3 <- as.matrix(tdm3)
tdm3[1:10, 1:20]
tdm3
v3 = rowSums(tdm3)
v3

#wordcloud plot of dishes liked from best resto

cloud3 = sort(rowSums(tdm3), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v3),
          freq = v3,
          max.words = 150,
          random.order = F,
          colors = brewer.pal(11, 'RdYlGn'),
          scale = c(4, 0.7),
          rot.per = 0.05, main = 'Liked Dishes from Corner House Ice Cream')

#--------#Smoor
Smoor = subset(filter_data,name == 'Smoor')

docs4 = iconv(Smoor$dish_liked)
docs4 = Corpus(VectorSource(docs4))
inspect(docs4[1:2])

#Text Cleaning
docs4 = tm_map(docs4, tolower)
inspect(docs4[1:2])

docs4 = tm_map(docs4, removePunctuation)
inspect(docs4[1:2])

docs4 = tm_map(docs4, removeNumbers)
inspect(docs4[1:2])

docs4 = tm_map(docs4, removeWords, stopwords("english"))
inspect(docs4[1:2])

smoor = tm_map(docs4, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

smoor = tm_map(docs4, stripWhitespace)

tdm4 <- TermDocumentMatrix(smoor)
tdm4
tdm4 <- as.matrix(tdm4)
tdm4[1:10, 1:20]
tdm4
v4 = rowSums(tdm4)
v4

#wordcloud plot of dishes liked from best resto
library(wordcloud)
cloud4 = sort(rowSums(tdm4), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v4),
          freq = v4,
          max.words = 200,
          random.order = F,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(4, 0.7),
          rot.per = 0.15, main = 'Liked Dishes from Smoor')

#----------#KFC
KFC = subset(filter_data,name == 'KFC')

docs5 = iconv(KFC$dish_liked)
docs5 = Corpus(VectorSource(docs5))
inspect(docs5[1:2])

#Text Cleaning
docs5 = tm_map(docs5, tolower)
inspect(docs5[1:2])

docs5 = tm_map(docs5, removePunctuation)
inspect(docs5[1:2])

docs5 = tm_map(docs5, removeNumbers)
inspect(docs5[1:2])

docs5 = tm_map(docs5, removeWords, stopwords("english"))
inspect(docs5[1:2])

kfc = tm_map(docs5, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

kfc = tm_map(docs5, stripWhitespace)

tdm5 <- TermDocumentMatrix(kfc)
tdm5
tdm5 <- as.matrix(tdm5)
tdm5[1:10, 1:20]
tdm5
v5 = rowSums(tdm5)
v5

#wordcloud plot of dishes liked from best resto
cloud5 = sort(rowSums(tdm5), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v5),
          freq = v5,
          max.words = 150,
          random.order = F,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(4, 0.7),
          rot.per = 0.15, main = 'Liked Dishes from KFC')

#-----------#Burger King
BK = subset(filter_data,name == 'Burger King')

docs6 = iconv(BK$dish_liked)
docs6 = Corpus(VectorSource(docs6))
inspect(docs6[1:2])

#Text Cleaning
docs6 = tm_map(docs6, tolower)
inspect(docs6[1:2])

docs6 = tm_map(docs6, removePunctuation)
inspect(docs6[1:2])

docs6 = tm_map(docs6, removeNumbers)
inspect(docs6[1:2])

docs6 = tm_map(docs6, removeWords, stopwords("english"))
inspect(docs6[1:2])

king = tm_map(docs6, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

king = tm_map(docs6, stripWhitespace)

tdm6 <- TermDocumentMatrix(king)
tdm6
tdm6 <- as.matrix(tdm6)
tdm6[1:10, 1:20]
tdm6
v6 = rowSums(tdm6)
v6

#wordcloud plot of dishes liked from best resto
cloud6 = sort(rowSums(tdm6), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v6),
          freq = v6,
          max.words = 200,
          random.order = F,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.6),
          rot.per = 0.05, main = 'Liked Dishes from Burger King')

#-----#Polar Bear
PB = subset(filter_data,name == 'Polar Bear')

docs7 = iconv(PB$dish_liked)
docs7 = Corpus(VectorSource(docs7))
inspect(docs7[1:2])

#Text Cleaning
docs7 = tm_map(docs7, tolower)
inspect(docs7[1:2])

docs7 = tm_map(docs7, removePunctuation)
inspect(docs7[1:2])

docs7 = tm_map(docs7, removeNumbers)
inspect(docs7[1:2])

docs7 = tm_map(docs7, removeWords, stopwords("english"))
inspect(docs7[1:2])

polar = tm_map(docs7, removeWords, c('the','and','rated','ratedn','was','for','with', 'had', 'have', 'just', 'can', 'time', 'are', 'been','ordered', 'all','place','food','this'))

polar = tm_map(docs7, stripWhitespace)

tdm7 <- TermDocumentMatrix(polar)
tdm7
tdm7 <- as.matrix(tdm7)
tdm7[1:10, 1:20]
tdm7
v7 = rowSums(tdm7)
v7

#wordcloud plot of dishes liked from best resto
cloud7 = sort(rowSums(tdm7), decreasing = TRUE)

set.seed(222)
wordcloud(words = names(v7),
          freq = v7,
          max.words = 250,
          random.order = F,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.6),
          rot.per = 0.05, main = 'Liked Dishes from Polar Bear')

#-----#Mani Dum Biryani





#.............................................................Code for Extracting location and ploting geomap ............................................................
df <- read.csv('C:/sem 2/EM-622/ProjectData.csv/ProjectData.csv')
table(is.na(df))



df_new <- df%>%
  filter(name %in% c ('Onesta','Faasos','Empire Restaurant','Smallys Resto Cafe','Faasos','Corner House Ice Cream','Burger King','Polar Bear','Smoor','Mani Dum Biryani','KFC'))%>%
  select('address',location,name)


install.packages("ggmap")
library(ggmap)

register_google(key = 'API') 

df_new <- df_new$address[!(df_new$address=="")]
#df_new$address <- as.factor(df_new$address)
#df_new$location <- as.factor(df_new$location)
df_new <- as.data.frame(df_new)


geocodes <- data.frame(stringsAsFactors = FALSE)

for(i in nrow(df_new)){
  
  result <- geocode(location = df_new$address,output = "latlona",source = "google")
  df_new$lon[i] <- as.numeric(result[1])
  df_new$lat[i]    <- as.numeric(result[2])
  df_new$geoaddress <- as.character(result[3])
  
}
Rest_name <- df_new%>%select(name)
Rest_name <- as.data.frame(Rest_name)

loc_data <- c(result,Rest_name)
loc_data <- as.data.frame(loc_data)

table(is.na(loc_data))
loc_data <- na.omit(loc_data)

write.csv(loc_data,file = "C:/sem 2/EM-622/Project/Sharks/locations.csv")

df1 <- read.csv('C:/sem 2/EM-622/Project/Sharks/locations.csv')

df1$address <- as.factor(df1$address)
df1$name <- as.factor(df1$name)

library(leaflet)
library(magrittr) 
library(htmltools)
library(htmlwidgets)

basicmap <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(data = df1,
             lng = ~lon, lat = ~lat, popup = ~location,
             label = ~htmlEscape(name))
basicmap %>% 
  addLayersControl(overlayGroups = c("name"))


#...................................................................................Network analysis................................................................

#Loding main dataset
df <- ProjectData
View(df)

filter_data = df %>% filter(name %in% c ("Onesta","Faasos","Empire Restaurant","Smally's Resto Cafe","Faasos","Corner House Ice Cream","Burger King","Polar Bear","Smoor","Mani Dum Biryani","KFC")) %>%select(name, online_order, rate ,reviews_list ) 
View(filter_data)

#Cleaning for social network analysis.

#Build corpus
corpus = iconv(df$reviews_list, to = "utf-8-mac")
corpus = Corpus(VectorSource(corpus))
inspect(corpus[1:2])

#Text Cleaning
corpus = tm_map(corpus, tolower)
inspect(corpus[1:2])

corpus = tm_map(corpus, removePunctuation)
inspect(corpus[1:2])

corpus = tm_map(corpus, removeNumbers)
inspect(corpus[1:2])

corpus = tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus[1:2])

cleanset = tm_map(corpus, removeWords, c('rated','ratedn'))
inspect(corpus[1:2])

cle <- data.frame(text = get("content", cleanset))
cle <- na.omit(cle)

write.csv(cle, file = "/Users/monilshah/Desktop/R_Finals/text_for_sentiment_analysis.csv", sep = " ",
          row.names = FALSE)

