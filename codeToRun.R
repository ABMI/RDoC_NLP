##############################################
# Database connection and extract
##############################################
library(dplyr)
library(DatabaseConnector)

connectionDetails <- DatabaseConnector::createConnectionDetails(user = '',
                                                                password = '',
                                                                server = '',
                                                                dbms = '',
                                                                pathToDriver = pathToDriver)

connect <- DatabaseConnector::connect(connectionDetails)
# note <- DatabaseConnector::dbReadTable(conn = connect, name = "JM_Note.dbo.depression_admission_ajou")
# str(note)

notepsy <- read.csv('./DSM5scoring/erpsy.csv', stringsAsFactors = F)
str(notepsy)

# temp <- erpsy %>% select(note_id, person_id, note_date) %>% as.data.frame()
# erpsy <- as.data.frame(erpsy)
# erpsy <- erpsy %>% select(note_id, person_id, note_date, note_text)

# note <- as.data.frame(erpsy)
# insertTable(connection = connect, tableName = "JM_Note.dbo.DSM_ER_person", data = temp, createTable = T, progressBar = T)
hospitalPts <- dbReadTable(conn = connect, name = "JM_Note.dbo.Hospitalized_Psy")

# target cohort (hospitalized patients due to suicidal attempt)
note <- left_join(hospitalPts, notepsy, by=c("note_id" = "note_id"))
str(note)

note$los <- note$visit_end_date-note$visit_start_date
temp <- note %>% select(visit_start_date, visit_end_date, los)
View(temp)

# hospitalPts %>% select(note_id) %>% unique() %>% count()
# nrow(hospitalPts)
# erpsy %>% select(note_id) %>% unique() %>% count()
# nrow(erpsy)

##############################################
# preprocessing and create corpus
##############################################
library(dplyr)
library(NLP)
library(tm)
# note <- note %>% select(NOTE_ID, PERSON_ID, NOTE_DATE, NOTE_TEXT)
str(note)

# corpus <- notepsy$note_text
tokenizer <- function(x){unlist(lapply(NLP::ngrams(NLP::words(x), 1:2), paste, collapse = " "), use.names = FALSE)}

# temp <- note[1:10,]
test <- note$note_text
test <- gsub('[^[:ascii:]]', '', test, perl = T)
test <- tm::VCorpus(tm::VectorSource(test))
test <- tm::tm_map(test, removePunctuation)
test <- tm::tm_map(test, removeNumbers)
test <- tm::tm_map(test, tolower)
test <- tm::tm_map(test, removeWords, STOP_WORDS)
test <- tm::tm_map(test, stripWhitespace)
test <- tm::tm_map(test, PlainTextDocument)

# temp <- NLP::ngrams(NLP::words(test), 1)
# dtm <- tm::DocumentTermMatrix(test, control = list(wordLength=c(2,Inf), tokenizer = tokenizer)) # add intergrated versio of unigram and bigram
tdm <- tm::TermDocumentMatrix(test, control = list(wordLength=c(2,Inf), tokenizer = tokenizer)) # add intergrated versio of unigram and bigram

##wordlength는 minimum word(alphabet like a is 1 word)

# rownames(dtm) <- temp$NOTE_ID
colnames(tdm) <- note$note_id
# dtm <- as.data.frame(as.matrix(dtm))
tdm <- as.data.frame(as.matrix(tdm))
tdm <- ifelse(tdm[,1:length(tdm)]>0, 1, 0)

# positive <- gsub("_", " ", positive)
# negative <- gsub("_", " ", negative)
# cognitive <- gsub("_", " ", cognitive)
# social <- gsub("_", " ", social)
# arousal_regulatory <- gsub("_", " ", arousal_regulatory)

# positive
positive_tdm <- tdm[rownames(tdm) %in% positive,]
positive_score <- colSums(positive_tdm)/nrow(positive_tdm)
# negative
negative_tdm <- tdm[rownames(tdm) %in% negative,]
negative_score <- colSums(negative_tdm)/nrow(negative_tdm)
# cognitive
cognitive_tdm <- tdm[rownames(tdm) %in% cognitive,]
cognitive_score <- colSums(cognitive_tdm)/nrow(cognitive_tdm)
# social
social_tdm <- tdm[rownames(tdm) %in% social,]
social_score <- colSums(social_tdm)/nrow(social_tdm)
# arousal_regulatory
arousal_regulatory_tdm <- tdm[rownames(tdm) %in% arousal_regulatory,]
arousal_regulatory_score <- colSums(arousal_regulatory_tdm)/nrow(arousal_regulatory_tdm)

DSM5_score <- rbind(positive_score, negative_score, cognitive_score, social_score, arousal_regulatory_score)
DSM5_score <- t(DSM5_score) %>% round(3)
DSM5_score

# Korean
# Traget population
# ngram
