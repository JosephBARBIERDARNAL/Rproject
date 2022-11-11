# START SESSION ----
rm(list=ls())
setwd("~/Desktop/M1S1/R/R2022--DATA")

library(data.table)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(stringi)

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

#keep only alphanumerics in abstracts
data$abstract = gsub("[^[:alpha:]]", " ", data$abstract)

#fix the case
data$abstract = tolower(data$abstract)

#keep only words with 3+ letters
data$abstract = gsub("\\b[[:alpha:]]{1,2}\\b", "", data$abstract)


#create a function that take an abstract and remove dupplicate
my_str_split = function(abstract){
  x = paste(stri_unique(unlist(strsplit(abstract, split = " "))))
  output = paste(x, collapse = " ")
  return(output)
}

#use our last function for each abstract of the "focus" dataset
data$short_abstract = lapply(data$abstract, my_str_split)

#paste all short_abstract into one large abstract
all_short_abs = paste(data$short_abstract, collapse = " ")

#transform all_short_abs into a dataframe: 1 row = 1 word
abs_list = unlist(strsplit(all_short_abs, " "))
freq = as.data.frame(abs_list)

#compute word occurence
freq$one = 1
freq = freq %>%
  group_by(abs_list) %>%
  mutate(occurence = sum(one))

#remove useless feature and duplicate
freq = freq[, -2]
freq = distinct(freq)

#create keywords
percent = round(length(data$abstract)*0.1)
freq = subset(freq, occurence<=percent & occurence>=5)


# Q4 ----

jaccard = function(abs1, abs2){
  
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
citation = citation[,-2]
names(citation)[names(citation) == 'paper_cited'] = 'PaperId'
new_data = merge(data, citation, by="PaperId", allow.cartesian = TRUE)

#create final dataset
new_data = new_data[, c("PaperId", "abstract", "nb_cites")]
new_data = distinct(new_data)

# Q7 ----
x = c("resume ", "recent ", "abstract ", "this", "purpose", "rank")
keyword = as.data.frame(x)
keyword$average = NA

#compute average number of citation per/keyword
for (i in seq(1, nrow(keyword))){
  c = 0
  cites = 0
  
  cat("Start of the", i, "th keyword \n")
  
  for (j in seq(1, nrow(new_data))){
    if (grepl(pattern = keyword[i,1], x = new_data$abstract[j])){
      c = c + 1
      cites = cites + new_data$nb_cites[j]
    }
  }
  keyword$average[i] = cites/c
}

# display the 20 top keywords
keyword$average = sort(keyword$average, decreasing =TRUE)
top20 = keyword$x[1:5]





#useless code ----

#create a vector of all words
words = gsub("[^[:alpha:]]", " ", data$abstract)
words = unique(unlist(strsplit(words, split = " ")))


#count word occurence
sum(stri_count_fixed(str=(all_short_abs), pattern=words[2]))
stri_count_fixed(str = all_short_abs, pattern = words[2])
length(all_short_abs[which(all_short_abs==words[2])])

#remove numeric in all_short_abs
all_short_abs = gsub("[^[:alpha:]]", " ", all_short_abs)


frq1 = as.data.frame(unlist(strsplit(all_short_abs, " ")))
colnames(frq1) = c("var1")
frq1$freq = table(frq1$var1)


data$abstract1 = strsplit(data$abstract, split = " ")
sum(stri_count_fixed(str=unique(data$abstract), pattern=words[2]))


words = as.data.frame(words)
words$freq[i] = sum(grepl(words[i,1], data$abstract))

