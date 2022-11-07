# START SESSION ----
rm(list=ls())
setwd("~/Desktop/M1S1/R/R2022--DATA")

library(data.table)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

abstracts = fread("abstracts.txt")
author_name = fread("author_name.txt")
paper_author_instit = fread("paper_author_instit.txt")
paper_info = fread("paper_info.txt")
references = fread("references.txt")

# Q1 ----

#correct data type (in order to correct a data type bug)
abstracts$PaperId = as.numeric(abstracts$PaperId)
paper_author_instit$PaperId = as.numeric(paper_author_instit$PaperId)

#merging datasets
data = merge(abstracts, paper_info, by='PaperId', all.y = TRUE, all.x = TRUE)
data = merge(data, paper_author_instit, by='PaperId', all.y = TRUE, all.x = TRUE)
data = merge(data, author_name, by='AuthorId', all.y = TRUE, all.x = TRUE)

#last changing
data = data[, -c("DocType")]
data$Date = str_sub(data$Date, start=1, end=4)

#keep only Bdx institution
data$OriginalAffiliation = toupper(data$OriginalAffiliation)
data$present = grepl("BORDEAUX", data$OriginalAffiliation)
data = subset(data, present==TRUE)
data = data[, -c("present")]

#delete NA
data = drop_na(data)

# Q2 ----

#count the number of chr of each abstract
data$count_abst = nchar(data$abstract)

#keep only those >=100
data = subset(data, count_abst>=100)
data = data[, -c("count_abst")]

#count name occurence
frequence = as.data.frame(table(data$NormalizedName))
names(frequence)[names(frequence) == 'Var1'] = 'author_name'

#keep only name occurence >=3 in our dataset
names(data)[names(data) == 'NormalizedName'] = 'author_name'
data = merge(data, frequence, by='author_name')
data = subset(data, Freq>=3)
data = data[, -c("Freq")]

# Q3 ----

## A ----
#keep only alphanumerics in abstracts
data$abstract = gsub("[^[:alpha:][:digit:]]", " ", data$abstract)

#keep only words with 3+ letters
data$abstract = gsub("\\b[[:alpha:]]{1,2}\\b", "", data$abstract)
data$abstract1 = strsplit(data$abstract, split = " ")


## B ----
#create a vector of words
words = gsub("[^[:alpha:]]", " ", data$abstract)
words = unique(unlist(strsplit(words, split = " ")))
#words = data.frame(words)
words = as.factor(words)
words = data.frame(words)

#define a count function
count = function(word, abstracts){
  
  c = 0
  for (abstract in abstracts){
    if (word %in% abstract){
      c = c + 1
    }
    return(c)
  }
}




#words[,1] %in% unlist(strsplit(data$abstract[1], split = " "))




# Q4 ----

jaki = function(abs1, abs2){
  
  abs1 = unlist(strsplit(abs1, split = " "))
  abs2 = unlist(strsplit(abs2, split = " "))

  common_word = length(intersect(abs1, abs2))
  total_distinct_word = length(abs1) + length(abs2) - common_word
  
  return(common_word/total_distinct_word)
}

#data$PaperId2 = data$PaperId
#datest = expand.grid(data$PaperId, data$PaperId2)


# Q5 ----

#merging
names(references)[names(references) == 'paper_citing'] = 'PaperId'
references$PaperId = as.numeric(references$PaperId)
cite = merge(paper_info, references, by="PaperId", all.x = TRUE, all.y = TRUE)

#some changes
cite = cite[,-c("DocType")]
cite = drop_na(cite)
cite$Date = str_sub(cite$Date, start=1, end=4)

#get date from cited paper
cite2 = cite[, -c("paper_cited")]
names(cite2)[names(cite2) == 'Date'] = 'Publication'
names(cite)[names(cite) == 'Date'] = 'Date_cited'
names(cite2)[names(cite2) == 'PaperId'] = 'paper_cited'

#merge
cite$paper_cited = as.numeric(cite$paper_cited)
citation = distinct(merge(cite, cite2, by="paper_cited", allow.cartesian = TRUE))

#compute difference
citation$Date_cited = as.integer(citation$Date_cited)
citation$Publication = as.integer(citation$Publication)
citation$difference = citation$Date_cited - citation$Publication

#remove citations 5 years after publication
citation = subset(citation, difference <= 5)

#compute nb_cites
citation$one = 1
citation = citation %>%
  group_by(paper_cited) %>%
  mutate(nb_cites = sum(one))



# Q6 ----

#merge datasets
names(citation2)[names(citation2) == 'paper_cited'] = 'PaperId'
new_data = merge(data, citation2, by="PaperId", all.x= TRUE)
new_data = new_data[, c("PaperId", "abstract", "nb_cites")]

# Q7 ----

keyword = as.data.frame(words[100:200, 1])
keyword$average = NA

#compute average number of citation per/keyword
for (i in seq(1, nrow(keyword))){
  c = 0
  cites = 0
  
  cat("Start of the", i, "keyword \n")
  
  for (j in seq(1, nrow(new_data))){
    if (keyword[i,1] %in% new_data$abstract[j]){
      c = c + 1
      cites = cites + new_data$nb_cites[j]
    }
  }
  keyword$average[i] = cites/c
}


# display the 20 top keywords
vec = data$Date
vec = as.numeric(vec)
vec = sort(vec, decreasing =TRUE)
top20 = vec[1:20]
