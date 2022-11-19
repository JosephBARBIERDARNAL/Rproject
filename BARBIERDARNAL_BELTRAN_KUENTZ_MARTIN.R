# START SESSION ----
rm(list=ls())
setwd("~/Desktop/M1S1/R/R2022--DATA")

library(data.table)
library(ggplot2)

abstracts = fread("abstracts.txt")
author_name = fread("author_name.txt")
paper_author_instit = fread("paper_author_instit.txt")
paper_info = fread("paper_info.txt")
references = fread("references.txt")

# Q1 ----

#correct data type (in order to correct a data type bug)
abstracts$PaperId = as.double(abstracts$PaperId)
paper_author_instit$PaperId = as.double(paper_author_instit$PaperId)

#merging datasets
data = merge(abstracts, paper_info, by='PaperId')
data = merge(data, paper_author_instit, by='PaperId')
data = merge(data, author_name, by='AuthorId')

#last changing
data = data[, -c("DocType")]
data$Date = substr(data$Date, start=1, stop=4)

#keep only Bdx institution
data$OriginalAffiliation = toupper(data$OriginalAffiliation)
data$present = grepl("BORDEAUX", data$OriginalAffiliation, ignore.case = TRUE)
data = subset(data, present==TRUE)
data = data[, -c("present")]

#clean environment
remove(abstracts, author_name, paper_author_instit)

# Q2 ----

#keep only abstracts with at least 100 characters
data = data[nchar(data$abstract) >= 100]

#count name occurence and keep only >=3
frequence = data.frame(table(data$AuthorId))
colnames(frequence) = c("AuthorId", "occurence")
frequence = subset(frequence, occurence >= 3)

#assuming that scientists with the same name but different AuthorId are not the same person
data$AuthorId = as.factor(data$AuthorId)
data = merge(data, frequence, by='AuthorId')
focus_sample = data[, -c("occurence")]

#clean environment
remove(frequence, data)

# Q3 ----

#keep only PaperId and abstract
data = unique(focus_sample[, c("PaperId", "abstract")])

#keep only alphanumerics in abstracts
data$abstract = gsub("[^[:alpha:]]", " ", data$abstract)

#fix the case to upper
data$abstract = toupper(data$abstract)

#keep only words with 3+ letters
data$abstract = gsub("\\b[[:alpha:]]{1,2}\\b", "", data$abstract)

#compute in how many abstract each keyword is present
all_words = strsplit(data$abstract, " ")
short_abs = lapply(all_words, unique)
freq_kw = table(unlist(short_abs))
freq_kw = as.data.table(freq_kw)

#remove non-keyword words from the words list
freq_kw = freq_kw[N>=5 & N<=0.1*length(data$abstract)]

#create a dataset where each row is a word from a PaperId
liste = list()
for (i in seq(1, nrow(data))) {
  keywords = strsplit(short_abs[[i]], " ")
  PaperId = rep(data$PaperId[i], length(keywords))
  temp = data.table(PaperId, keywords)
  liste[[i]] = temp
}
data_kw = rbindlist(liste)

#remove non-keyword words from our last dataframe
data_kw = data_kw[keywords %in% freq_kw$V1]

#put back together all words in order to get keywords-only abstracts
data = data_kw[, .(keyword = paste(keywords, collapse = " ")),
               by = PaperId]

#clean environment
remove(all_words, keywords, liste, short_abs, temp, i, PaperId, freq_kw, data_kw)

# Q4 ----

#function that computes jaccard index between two papers
jaccard = function(abs1, abs2){
  
  #split abstracts
  abs1 = unlist(strsplit(abs1, split = " "))
  abs2 = unlist(strsplit(abs2, split = " "))
  
  #compute jaccard
  common_words = length(intersect(abs1, abs2))
  total_distinct_words = length(abs1) + length(abs2) - common_words
  
  return(common_words/total_distinct_words)
}

x = "I am a rock"
y = "I am the fire"
jaccard(x, y) #should return 0.333

#function that computes the specialization measure 
specialization = function(author, dataframe=data2){
  
  #compute all jaccards for one author
  daf = subset(dataframe, AuthorId==author)
  n = nrow(daf)
  daf = data.frame(combn(x=daf$X2, m=2))
  daf = data.frame(t(daf))
  daf$speciali = apply(X = daf, MARGIN = 1, FUN = jaccard)
  
  #compute specialization
  denominator = n * (n-1)
  daf$X3 = NA
  specialisation = sum(daf$X3)/denominator
  return(specialisation)
}


#clean environment
remove(x, y, jaccard, specialization)

# Q5 ----

#merging
names(references)[names(references) == 'paper_citing'] = 'PaperId'
references$PaperId = as.numeric(references$PaperId)
cite = merge(paper_info, references, by="PaperId", all.x = TRUE, all.y = TRUE)

#some small changes
cite = cite[,-c("DocType")]
cite = cite[complete.cases(cite)]
cite$Date = substr(cite$Date, start=1, stop=4)

#get date from cited and citing paper
cite2 = cite[, -c("paper_cited")]
names(cite2)[names(cite2) == 'Date'] = 'Date_Publication'
names(cite)[names(cite) == 'Date'] = 'Date_Citing'
names(cite2)[names(cite2) == 'PaperId'] = 'paper_cited'

#merge
cite$paper_cited = as.numeric(cite$paper_cited)
citation = merge(cite, cite2, by="paper_cited", allow.cartesian = TRUE)

#compute years difference
citation$Date_Citing = as.integer(citation$Date_Citing)
citation$Date_Publication = as.integer(citation$Date_Publication)
citation$difference = citation$Date_Citing - citation$Date_Publication

#remove citations 5 years after publication
citation_5years = subset(citation, difference <= 5 & difference >= 0)

#compute nb_cites in a 5 years range
citation_5years = citation_5years[, .(nb_cites = .N),
                                  by = paper_cited]

#clean environement
remove(cite, cite2, paper_info, references)

# Q6 ----

#merge datasets
names(citation_5years)[names(citation_5years) == 'paper_cited'] = 'PaperId'
new_data = merge(data, citation_5years, by="PaperId", allow.cartesian = TRUE)

# Q7 ----

#compute average number of citations for each keyword
new_data = as.data.table(new_data)
keywords = (new_data[, .(PaperId, nb_cites, keyword = strsplit(keyword, " "))]
            [, .(keyword = unlist(keyword)), by = .(nb_cites)]
            [, list(mean(nb_cites)), by = .(keyword)])

#display 20 top keywords
keywords = keywords[order(-V1)]
keywords[1:20,]

# Q8 ----

#compute nb_cites for all years
citation = citation[, list(nb_cites = .N),
                    by = paper_cited]

#add number of citation for all years
names(citation)[names(citation) == 'paper_cited'] = 'PaperId'
new_data = merge(data, citation, by="PaperId", allow.cartesian = TRUE)

#compute number of cites for each scientist
scientist = merge(focus_sample[, c(1,2,6)], new_data[,c(1,3)], by="PaperId", all.x = TRUE)
scientists = scientist[, .(mean_cites = mean(nb_cites, na.rm=TRUE),
                           total_cites = sum(nb_cites, na.rm=TRUE)),
                       by = AuthorId]

#deducting the total number of publications
scientists$tot_pub = scientists$total_cites / scientists$mean_cites

scientists_name = unique(focus_sample[, c(1,6)])
scientists = merge(scientists, scientists_name, by="AuthorId")

#clean environment
remove(citation, citation_5years, data, new_data, scientist, scientists_name)

# Q9 ----

#put maj to scientists name
toTitle = function(x) {
  s = strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
scientists$Name = sapply(X = scientists$NormalizedName, FUN = toTitle)

#because we don't have the specialization measure obtained Q4,
#we create a fake one only for the graph
set.seed(1)
n = nrow(scientists)
spec = scientists$mean_cites*2 - rnorm(n=n, sd=600)

#transform into a z-score and add it to the df
spec = scale(spec)
scientists$specialization = spec

#create scatterplot between specialization and average number of citations
#we add the abline obtained with the least squares method
scatter = ggplot(scientists, aes(x=mean_cites, y=specialization)) +
  geom_point() +
  ylab("Specialization measure") +
  xlab("Average number of citations") +
  xlim(0, 3700) +
  ggtitle("Correlation between citations and specialization \nin Bordeaux scientists") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_label( 
    data=scientists[scientists$specialization>5.85 | scientists$mean_cites>2000],
    aes(label=Name), nudge_y = 1, nudge_x = -100)  + 
  theme_bw() + 
  theme(panel.background = element_rect(color = 1, size = 2))

#plot
scatter