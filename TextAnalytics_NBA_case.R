#' Title: NBA Fan Engagement Case
#' Purpose: Explore NBA related tweets
#' Date: March 15, 2022
#' Cholpon Temirbekova
#' Link to a video presentation of a project - https://youtu.be/qODKeOdbTPc

### 1. Set working directory to your specific movie
setwd("~/NLP/Git/Text-Mining-NLP/Case/Case I/Data")

### 3. Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

### 4. Loading libraries
library(stringi)
library(stringr)
library(spelling)
library(hunspell)
library(mgsub)
library(pbapply)
library(tm, NLP)
library(ggplot2)
library(lattice)
library(caret)
library(ggthemes)
library(RColorBrewer)
library(wordcloud)


### 2. Loading the data
Oct19 <- read.csv("A_Oct2019.csv")
Nov <- read.csv("B_Nov2019.csv")
Dec <- read.csv("C_Dec2019.csv")
Feb <- read.csv("E_Feb2020.csv")
Mar <- read.csv("F_Mar2020.csv")
Apr <- read.csv("G_Apr2020.csv")
May <- read.csv("H_May2020.csv")
Jun <- read.csv("I_June2020.csv")
Jul <- read.csv("J_July2020.csv")
Aug <- read.csv("K_Aug2020.csv")
Sep <- read.csv("L_Sep2020.csv")
Oct20 <- read.csv("M_Oct2020.csv")

#################
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'smh')

basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  x <- tolower(x)
  return(x)
}

# apply the function to JUST THE TEXT COLUMN to a new object
Oct19[["text"]] <- basicSubs(Oct19$text)
Oct20[["text"]] <- basicSubs(Oct20$text)
Nov[["text"]] <- basicSubs(Nov$text)
Dec[["text"]] <- basicSubs(Dec$text)
Feb[["text"]] <- basicSubs(Feb$text)
Mar[["text"]] <- basicSubs(Mar$text)
Apr[["text"]] <- basicSubs(Apr$text)
May[["text"]] <- basicSubs(May$text)
Jun[["text"]] <- basicSubs(Jun$text)
Jul[["text"]] <- basicSubs(Jul$text)
Aug[["text"]] <- basicSubs(Aug$text)
Sep[["text"]] <- basicSubs(Sep$text)
Oct20[["text"]] <- basicSubs(Oct20$text)

All_year <- rbind(Oct19, Nov, Dec, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct20)

Lebron <- grep("lebron james", All_year$text, ignore.case=TRUE)
LebronLength <- length(Lebron)
Giannis <- grep("giannis antetokounmpo", All_year$text, ignore.case=TRUE)
GiannisLength <- length(Giannis)
Kevin <- grep("kevin durant", All_year$text, ignore.case=TRUE)
KevinLength <- length(Kevin)
Nike <- grep("nike", All_year$text, ignore.case=TRUE) 
NikeLength <- length(Nike)

ratioLebron <- (LebronLength / nrow(Oct19)) * 100
ratioGiannis <- (GiannisLength / nrow(Oct19)) * 100
ratioKevin <- (KevinLength / nrow(Oct19)) * 100
ratioNike<- (NikeLength/ nrow(Oct19)) * 100

Ratio <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(ratioLebron, ratioGiannis, ratioKevin, ratioNike))

ggplot(Ratio, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

### 6. Count the number of mentions
# October 2019
LebronOct19  <- sum(stri_count(Oct19$text, fixed ='lebron james'))
GiannisOct19  <- sum(stri_count(Oct19$text, fixed ='giannis antetokounmpo'))
KevinOct19  <- sum(stri_count(Oct19$text, fixed ='kevin durant'))
NikeOct19  <- sum(stri_count(Oct19$text, fixed ='nike'))

# November 2019
LebronNov  <- sum(stri_count(Nov$text, fixed ='lebron james'))
GiannisNov  <- sum(stri_count(Nov$text, fixed ='giannis antetokounmpo'))
KevinNov  <- sum(stri_count(Nov$text, fixed ='kevin durant'))
NikeNov  <- sum(stri_count(Nov$text, fixed ='nike'))

# December 2019
LebronDec  <- sum(stri_count(Dec$text, fixed ='lebron james'))
GiannisDec  <- sum(stri_count(Dec$text, fixed ='giannis antetokounmpo'))
KevinDec  <- sum(stri_count(Dec$text, fixed ='kevin durant'))
NikeDec  <- sum(stri_count(Dec$text, fixed ='nike'))

# February 2020
LebronFeb  <- sum(stri_count(Feb$text, fixed ='lebron james'))
GiannisFeb  <- sum(stri_count(Feb$text, fixed ='giannis antetokounmpo'))
KevinFeb  <- sum(stri_count(Feb$text, fixed ='kevin durant'))
NikeFeb  <- sum(stri_count(Feb$text, fixed ='nike'))

# March 2020
LebronMar  <- sum(stri_count(Mar$text, fixed ='lebron james'))
GiannisMar  <- sum(stri_count(Mar$text, fixed ='giannis antetokounmpo'))
KevinMar  <- sum(stri_count(Mar$text, fixed ='kevin durant'))
NikeMar  <- sum(stri_count(Mar$text, fixed ='nike'))

# April 2020
LebronApr  <- sum(stri_count(Apr$text, fixed ='lebron james'))
GiannisApr  <- sum(stri_count(Apr$text, fixed ='giannis antetokounmpo'))
KevinApr  <- sum(stri_count(Apr$text, fixed ='kevin durant'))
NikeApr  <- sum(stri_count(Apr$text, fixed ='nike'))

# May 2020
LebronMay  <- sum(stri_count(May$text, fixed ='lebron james'))
GiannisMay  <- sum(stri_count(May$text, fixed ='giannis antetokounmpo'))
KevinMay  <- sum(stri_count(May$text, fixed ='kevin durant'))
NikeMay  <- sum(stri_count(May$text, fixed ='nike'))

# June 2020
LebronJun  <- sum(stri_count(Jun$text, fixed ='lebron james'))
GiannisJun  <- sum(stri_count(Jun$text, fixed ='giannis antetokounmpo'))
KevinJun  <- sum(stri_count(Jun$text, fixed ='kevin durant'))
NikeJun  <- sum(stri_count(Jun$text, fixed ='nike'))

# July 2020
LebronJul  <- sum(stri_count(Jul$text, fixed ='lebron james'))
GiannisJul  <- sum(stri_count(Jul$text, fixed ='giannis antetokounmpo'))
KevinJul  <- sum(stri_count(Jul$text, fixed ='kevin durant'))
NikeJul  <- sum(stri_count(Jul$text, fixed ='nike'))

# Aug 2020
LebronAug  <- sum(stri_count(Aug$text, fixed ='lebron james'))
GiannisAug  <- sum(stri_count(Aug$text, fixed ='giannis antetokounmpo'))
KevinAug  <- sum(stri_count(Aug$text, fixed ='kevin durant'))
NikeAug  <- sum(stri_count(Aug$text, fixed ='nike'))

# Sep 2020
LebronSep  <- sum(stri_count(Sep$text, fixed ='lebron james'))
GiannisSep  <- sum(stri_count(Sep$text, fixed ='giannis antetokounmpo'))
KevinSep  <- sum(stri_count(Sep$text, fixed ='kevin durant'))
NikeSep  <- sum(stri_count(Sep$text, fixed ='nike'))

# Oct 2020
LebronOct20  <- sum(stri_count(Oct20$text, fixed ='lebron james'))
GiannisOct20  <- sum(stri_count(Oct20$text, fixed ='giannis antetokounmpo'))
KevinOct20  <- sum(stri_count(Oct20$text, fixed ='kevin durant'))
NikeOct20  <- sum(stri_count(Oct20$text, fixed ='nike'))

# Organize term objects into a data frame
termFreqNikeOct19 <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                                freq  = c(LebronOct19, GiannisOct19, KevinOct19, NikeOct19))

termFreqNikeNov <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronNov, GiannisNov, KevinNov, NikeNov))

termFreqNikeDec <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronDec, GiannisDec, KevinDec, NikeDec))

termFreqNikeFeb <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronFeb, GiannisFeb, KevinFeb, NikeFeb))              

termFreqNikeMar <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronMar, GiannisMar, KevinMar, NikeMar))

termFreqNikeApr <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronApr, GiannisApr, KevinApr, NikeApr))

termFreqNikeMay <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronMay, GiannisMay, KevinMay, NikeMay))

termFreqNikeJun <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronJun, GiannisJun, KevinJun, NikeJun))

termFreqNikeJul <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronJul, GiannisJul, KevinJul, NikeJul))

termFreqNikeAug <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronAug, GiannisAug, KevinAug, NikeAug))

termFreqNikeSep <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                              freq  = c(LebronSep, GiannisSep, KevinSep, NikeSep))

termFreqNikeOct20 <- data.frame(terms = c('lebron','giannis', 'kevin', 'nike'),
                                freq  = c(LebronOct20, GiannisOct20, KevinOct20, NikeOct20))

# Plot a geom_bar with ggplot2 by filling in the correct data, adding a layers "theme_gdocs() + theme(legend.position = "none")"
# October 2019
ggplot(termFreqNikeOct19, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# November
ggplot(termFreqNikeNov, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# December
ggplot(termFreqNikeDec, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# February
ggplot(termFreqNikeFeb, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# March
ggplot(termFreqNikeMar, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# April
ggplot(termFreqNikeApr, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# May
ggplot(termFreqNikeMay, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# June
ggplot(termFreqNikeJun, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# July
ggplot(termFreqNikeJul, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# August
ggplot(termFreqNikeAug, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# September
ggplot(termFreqNikeSep, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# October 2020
ggplot(termFreqNikeOct20, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# Frequency of terms
# Lebron James
termFreqLebron <- data.frame(terms = c('Oct19','Nov19', 'Dec19', 'Feb20', 'Mar20', 
                                       'Apr20', 'May20', 'Jun20', 'Jul20', 'Aug20', 'Sep20', 'Oct20'),
                             freq  = c(LebronOct19, LebronNov, LebronDec, LebronFeb,
                                       LebronMar, LebronApr, LebronMay, LebronJun, LebronJul,
                                       LebronAug, LebronSep, LebronOct20))

ggplot(termFreqLebron, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# giannis antetokounmpo
termFreqGiannis <- data.frame(terms = c('Oct19','Nov19', 'Dec19', 'Feb20', 'Mar20', 
                                        'Apr20', 'May20', 'Jun20', 'Jul20', 'Aug20', 'Sep20', 'Oct20'),
                              freq  = c(GiannisOct19, GiannisNov, GiannisDec, GiannisFeb,
                                        GiannisMar, GiannisApr, GiannisMay, GiannisJun, GiannisJul,
                                        GiannisAug, GiannisSep, GiannisOct20))

ggplot(termFreqGiannis, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# Kevin Durant
termFreqKevin <- data.frame(terms = c('Oct19','Nov19', 'Dec19', 'Feb20', 'Mar20', 
                                      'Apr20', 'May20', 'Jun20', 'Jul20', 'Aug20', 'Sep20', 'Oct20'),
                            freq  = c(KevinOct19, KevinNov, KevinDec, KevinFeb,
                                      KevinMar, KevinApr, KevinMay, KevinJun, KevinJul,
                                      KevinAug, KevinSep, KevinOct20))
ggplot(termFreqKevin, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# Nike
termFreqNike <- data.frame(terms = c('Oct19','Nov19', 'Dec19', 'Feb20', 'Mar20', 
                                     'Apr20', 'May20', 'Jun20', 'Jul20', 'Aug20', 'Sep20', 'Oct20'),
                           freq  = c(NikeOct19, NikeNov, NikeDec, NikeFeb,
                                     NikeMar, NikeApr, NikeMay, NikeJun, NikeJul,
                                     NikeAug, NikeSep, NikeOct20))
ggplot(termFreqNike, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")
##########################################################

### Spliting the data and using only the sample
## October
index <- sample(1:nrow(Oct19), size = 0.02*nrow(Oct19))
small_Oct19 <- Oct19[index,]

## November
index <- sample(1:nrow(Nov), size = 0.02*nrow(Nov))
small_Nov <- Nov[index,]

## December
index <- sample(1:nrow(Dec), size = 0.02*nrow(Dec))
small_Dec <- Dec[index,]

## February
index <- sample(1:nrow(Feb), size = 0.02*nrow(Feb))
small_Feb <- Feb[index,]

## March
index <- sample(1:nrow(Mar), size = 0.02*nrow(Mar))
small_Mar <- Mar[index,]

## April
index <- sample(1:nrow(Apr), size = 0.02*nrow(Apr))
small_Apr <- Apr[index,]

## May
index <- sample(1:nrow(May), size = 0.02*nrow(May))
small_May <- May[index,]

## June
index <- sample(1:nrow(Jun), size = 0.02*nrow(Jun))
small_Jun <- Jun[index,]

## July
index <- sample(1:nrow(Jul), size = 0.02*nrow(Jul))
small_Jul <- Jul[index,]

## August
index <- sample(1:nrow(Aug), size = 0.02*nrow(Aug))
small_Aug <- Aug[index,]

## September
index <- sample(1:nrow(Sep), size = 0.02*nrow(Sep))
small_Sep <- Sep[index,]

## October 2020
index <- sample(1:nrow(Oct20), size = 0.02*nrow(Oct20))
small_Oct20 <- Oct20[index,]

############################

################################



####################################
### Cleaning

## Applying the function to the data
# October
###
small_Oct19.clean <- VCorpus(VectorSource(small_Oct19$text))
small_Oct19.clean <- cleanCorpus(small_Oct19.clean, stops)
small_Oct19.clean.dtm <- DocumentTermMatrix(small_Oct19.clean)
small_Oct19.clean.matrix <- as.matrix(small_Oct19.clean.dtm)

# November
###
small_Nov.clean <- VCorpus(VectorSource(small_Nov$text))
small_Nov.clean <- cleanCorpus(small_Nov.clean, stops)
small_Nov.clean.dtm <- DocumentTermMatrix(small_Nov.clean)
small_Nov.clean.matrix <- as.matrix(small_Nov.clean.dtm)

# December
###
small_Dec.clean <- VCorpus(VectorSource(small_Dec$text))
small_Dec.clean <- cleanCorpus(small_Dec.clean, stops)
small_Dec.clean.dtm <- DocumentTermMatrix(small_Dec.clean)
small_Dec.clean.matrix <- as.matrix(small_Dec.clean.dtm)

# February
###
small_Feb.clean <- VCorpus(VectorSource(small_Feb$text))
small_Feb.clean <- cleanCorpus(small_Feb.clean, stops)
small_Feb.clean.dtm <- DocumentTermMatrix(small_Feb.clean)
small_Feb.clean.matrix <- as.matrix(small_Feb.clean.dtm)

# March
###
small_Mar.clean <- VCorpus(VectorSource(small_Mar$text))
small_Mar.clean <- cleanCorpus(small_Mar.clean, stops)
small_Mar.clean.dtm <- DocumentTermMatrix(small_Mar.clean)
small_Mar.clean.matrix <- as.matrix(small_Mar.clean.dtm)

# April
###
small_Apr.clean <- VCorpus(VectorSource(small_Apr$text))
small_Apr.clean <- cleanCorpus(small_Apr.clean, stops)
small_Apr.clean.dtm <- DocumentTermMatrix(small_Apr.clean)
small_Apr.clean.matrix <- as.matrix(small_Apr.clean.dtm)

# May
###
small_May.clean <- VCorpus(VectorSource(small_May$text))
small_May.clean <- cleanCorpus(small_May.clean, stops)
small_May.clean.dtm <- DocumentTermMatrix(small_May.clean)
small_May.clean.matrix <- as.matrix(small_May.clean.dtm)

# June
###
small_Jun.clean <- VCorpus(VectorSource(small_Jun$text))
small_Jun.clean <- cleanCorpus(small_Jun.clean, stops)
small_Jun.clean.dtm <- DocumentTermMatrix(small_Jun.clean)
small_Jun.clean.matrix <- as.matrix(small_Jun.clean.dtm)

