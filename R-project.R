# START SESSION ----
rm(list=ls())
setwd("~/Desktop/M1S1/R/R2022--DATA")

library(data.table)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(stringi)
library(combinat)

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
data = merge(abstracts, paper_info, by='PaperId')
data = merge(data, paper_author_instit, by='PaperId')
data = merge(data, author_name, by='AuthorId')

#last changing
data = data[, -c("DocType")]
data$Date = str_sub(data$Date, start=1, end=4)

#keep only Bdx institution
data$OriginalAffiliation = toupper(data$OriginalAffiliation)
data$present = grepl("BORDEAUX", data$OriginalAffiliation, ignore.case = TRUE)
data = subset(data, present==TRUE)
data = data[, -c("present")]

#delete NA
data = drop_na(data)
remove(abstracts, author_name, paper_author_instit)

# Q2 ----

#count the number of chr of each abstract
data$count_abst = nchar(data$abstract)

#keep only those >=100
data = subset(data, count_abst>=100)
data = data[, -c("count_abst")]

#count name occurence and keep only >=3
frequence = data.frame(table(data$AuthorId))
colnames(frequence) = c("AuthorId", "occurence")
frequence = subset(frequence, occurence >= 3)

#assuming that scientists with the same name but different AuthorId are not the same person
data$AuthorId = as.factor(data$AuthorId)
data = merge(data, frequence, by='AuthorId')
focus_sample = data[, -c("occurence")]

remove(frequence)

# Q3 ----

#keep only PaperId and abstract
data = unique(data[, 2:3])

#keep only alphanumerics in abstracts
data$abstract = gsub("[^[:alpha:]]", " ", data$abstract) #[:digit:]

#fix the case
data$abstract = toupper(data$abstract)

#keep only words with 3+ letters
data$abstract = gsub("\\b[[:alpha:]]{1,2}\\b", "", data$abstract)

#create "words": the list of all abstract without dupplicates
all_words = strsplit(data$abstract, " ")
short_abs = lapply(all_words, unique)
freq_kw = table(unlist(short_abs)) # finding in how many rows each word occurs
freq_kw = as.data.table(freq_kw) # setting freqs as a datatable
freq_kw = freq_kw[N>=5 & N<= 0.1*length(data$abstract)]

#
liste = list()
for (i in 1:length(data$abstract)) {
  keywords = strsplit(short_abs[[i]], " ")
  PaperId = rep(data$PaperId[i], length(keywords))
  temp = data.table(PaperId, keywords)
  liste[[i]] = temp
}

data_kw = rbindlist(liste)
data_kw = data_kw[keywords %in% freq_kw$V1]

#create the variable keyword for each PaperId
data = data_kw %>%
  group_by(PaperId) %>%
  mutate(keyword = paste(keywords, collapse = " "))
data = data[, -2]
data = distinct(data)

remove(all_words, keywords, liste, short_abs, temp, i, PaperId)

# Q4 ----

data = merge(data, focus_sample, by = "PaperId")
remove(focus_sample, all_words, freq_kw,
       keywords, liste, temp, short_abs, i,
       PaperId, paper_info, references, data_kw)

jaccard = function(abs1, abs2){
  abs1 = unlist(strsplit(abs1, split = " "))
  abs2 = unlist(strsplit(abs2, split = " "))
  common_word = length(intersect(abs1, abs2))
  total_distinct_word = length(abs1) + length(abs2) - common_word
  return(common_word/total_distinct_word)
}

x = subset %>%
  group_by(AuthorId) %>%
  expand.grid(subset$keyword)


subset = data[, c(2,3)]
subset = distinct(subset)
x = subset %>%
  group_by(AuthorId) %>%
  combn(x=subset$keyword, m=2, fun=jaccard)

  
#x – source vectorielle pour les combinaisons
#m – nombre d’éléments à prendre
#fun – fonction à appliquer à chaque combinaison (peut être nulle)
#simplifier – logique, si FALSE, renvoie une liste, sinon renvoie un vecteur ou un array


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
citation = citation[, c(1,7)]
names(citation)[names(citation) == 'paper_cited'] = 'PaperId'
new_data = merge(data, citation, by="PaperId", allow.cartesian = TRUE)

#create final dataset
new_data = new_data[, c("PaperId", "nb_cites", "keyword")]
new_data = distinct(new_data)

# Q7 ----

keyword = freq_kw[, 1]
keyword$average = NA







#compute average number of citation per/keyword
for (i in seq(1, nrow(keyword))){
  
  #initialisation
  #c = 0
  #cites = 0
  cat("Start of the", i, "th keyword \n")
  
  #compute average of the i-th keyword
  keyword$average[i] = compute_presence(kw=keyword$V1[i])
}

keyword$average = lapply(keyword$V1, compute_average)


# display the 20 top keywords
keyword$average = sort(keyword$average, decreasing =TRUE)
top20 = keyword$x[1:5]

compute_average = function(kw, dataframe=new_data){
  
  #init
  cat('\n', kw, '\n')
  c=0
  cites=0
  
  #loop over abstract
  for (j in seq(1, nrow(new_data))){
    if (str_detect(pattern = kw, string = new_data$keyword[j])){
      c = c + 1
      cites = cites + new_data$nb_cites[j]
    }
  }
  
  #compute average
  average = cites/c
  return(average)
}






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

#KW = paste(freq$abs_list, collapse = " ")
keep_KW2 = function(abstract, Kw = KW){
  x = c(abstract, Kw)
  out = Reduce(intersect, strsplit(x, " "))
  return(out)
}


#create a function that take an abstract and remove dupplicate
my_str_split = function(abstract){
  x = paste(stri_unique(unlist(strsplit(abstract, split = " "))))
  output = paste(x, collapse = " ")
  return(output)
}

#use our last function for each abstract of the "focus" dataset
data$short_abstract = sapply(data$abstract, my_str_split)
data$short_abstract = as.character(data$short_abstract)


#paste all short_abstract into one large abstract
all_short_abs = paste(data$short_abstract, collapse = " ")

#transform all_short_abs into a dataframe: 1 row = 1 word
abs_list = unlist(strsplit(all_short_abs, " "))
freq = data.frame(abs_list)

#transform all_short_abs into a dataframe: 1 row = 1 word
short_abs = unlist(short_abs)
freq = data.table(short_abs)

#compute word occurence
freq$one = 1
freq = freq %>%
  group_by(short_abs) %>%
  mutate(occurence = sum(one))

#remove useless feature and duplicate
freq = freq[, -2]
freq = distinct(freq)

#create keywords
percent = round(length(data$abstract)*0.1)
freq = subset(freq, occurence<=percent & occurence>=5)

#create a function that take an abstract and remove dupplicate
my_str_split = function(abstract){
  x = paste(stri_unique(unlist(strsplit(abstract, split = " "))))
  output = paste(x, collapse = " ")
  return(output)
}

#use our last function for each abstract of the "focus" dataset
data$short_abstract = sapply(data$abstract, my_str_split)
data$short_abstract = as.character(data$short_abstract)


#paste all short_abstract into one large abstract
all_short_abs = paste(data$short_abstract, collapse = " ")

#transform all_short_abs into a dataframe: 1 row = 1 word
abs_list = unlist(strsplit(all_short_abs, " "))
freq = data.frame(abs_list)

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

#create a function that takes an abstract and returns the keywords of the latter
keep_KW = function(abstract, KW = freq$abs_list){
  abstract = unlist(strsplit(abstract, split = " ", fixe = TRUE))
  out = paste(intersect(abstract, KW), collapse = " ")
  #cat(out, "\n")
  return(out)
}


#1 (should work but seems infinite)
data$keyword = sapply(X=data$short_abstract, FUN=keep_KW)

#2 (works but a bit long)
for (i in seq(nrow(data))){
  data$keywords[i] = keep_KW(data$short_abstract[i])
  cat(i, 'th iteration over \n')
}
data_kw = subset(data, select = c("PaperId", "keywords"))

#save keywords
write.csv2(data_kw, file='data_kw')
test = read.csv2('data_kw')

compute_average = function(kw, dataframe=new_data){
  
  cat("\n", kw, "\n")
  
  #compute the number of abstract the kw is present in
  c = sum(kw %in% dataframe$keyword)
  
  
  #c = sum(grepl(pattern = kw, x = dataframe$keyword))
  c = str_count(dataframe$keyword, pattern = kw)
  
  #compute sum of citations
  cites = sum(dataframe$nb_cites[grepl(pattern = kw, x = dataframe$keyword)])
  
  #compute average
  output = cites/c
  
  return(output)
}

keyword$average = lapply(X=keyword$V1, FUN=compute_average)

