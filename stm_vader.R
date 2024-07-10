# Libraries
library(stm)
library(tm)
library(conText)
library(tidytext)
library(quanteda)
library(tidyverse)

# Merge all the posts and comments for 500 posts for Redpillwomen and TheRedpill.

# -------Redpillwomen
Redpillwomen_raw <- rio::import(here::here("data", "combined_Redpillwomen.csv")) 
#names(Redpillwomen_raw)

get_ids <- \(data){
set.seed(123)
  post_id <- data %>% 
    distinct(parent_id) %>%
    sample_n(size = 500) %>% 
    pull(parent_id)
}
post_ids <- get_ids(Redpillwomen_raw)

# This function merged the posts and comments 
merge_postsNcoms <- \(id){
  title <-  Redpillwomen_raw  %>% 
    filter(parent_id == {{id}}) %>% 
    distinct(title)
  
  selftext <- Redpillwomen_raw  %>% 
    filter(parent_id == {{id}}) %>% 
    distinct(selftext)
  
  
  comments <-  Redpillwomen_raw %>% 
    filter(parent_id == {{id}}) %>% 
    dplyr::select(comment) %>% 
    flatten() %>% 
    unlist()
  
  merged_text <- paste(c(title, selftext, comments), collapse = ",")
  tibble(id, merged_text)
  
}

Redpillwomen_merged <- map_df(post_ids, merge_postsNcoms) %>% 
  mutate(source = rep("female",500))

#View(Redpillwomen_merged )

# ----TheRedPill

TheRedPill_raw <- rio::import(here::here("data", "combined_TheRedPill.csv")) 

post_ids <- get_ids(TheRedPill_raw)

# changed the data here
merge_postsNcoms <- \(id){
  title <-  TheRedPill_raw %>% 
    filter(parent_id == {{id}}) %>% 
    distinct(title)
  
  selftext <-  TheRedPill_raw %>% 
    filter(parent_id == {{id}}) %>% 
    distinct(selftext)
  
  
  comments <-  TheRedPill_raw %>% 
    filter(parent_id == {{id}}) %>% 
    dplyr::select(comment) %>% 
    flatten() %>% 
    unlist()
  
  merged_text <- paste(c(title, selftext, comments), collapse = ",")
  tibble(id, merged_text)
  
}

TheRedPill_merged <- map_df(post_ids, merge_postsNcoms) %>% 
  mutate(source = rep("male",500))

full_data <- bind_rows(Redpillwomen_merged, TheRedPill_merged)

#saveRDS(full_data, file = "data/stm_data.rds")

#View(full_data)

# STRUCTURAL TOPIC MODELING

data_unclean <- rio::import("data/stm_data.rds")

View(data)

# Data pre-processing 

data_clean <- data_unclean %>% 
  rename("text" = merged_text) %>% 
  mutate(text = tolower(text))

data_proc <- textProcessor(documents = data_clean$text,
                           metadata = data_clean,
                           lowercase = TRUE,
                           stem = TRUE,
                           removestopwords = TRUE,
                           removenumbers = TRUE,
                           removepunctuation = TRUE)

meta <- data_proc$meta
vocab <-data_proc$vocab
docs <- data_proc$documents

pres_out <- prepDocuments(docs, vocab, meta)
docs <- pres_out$documents
vocab <- pres_out$vocab
meta <- pres_out$meta

#--------Determining the Topic Model---------#
install.packages("geometry")
install.packages(c("Rtsne", "rsvd"))

set.seed(123)

# what is the average topic effect if there is a sig difference between the language used by sources
# if I get rid of the prevalence line, can do logistic regression or any kind of classification
#get rid of one of the topics before doing a classification (k-1) topics; svm, rf
# deep learning cnn

# k = 0 & prevalence
pill_stm0prv <- stm(docs, vocab, K = 0,
                prevalence = ~ as.factor(source), 
                data = meta,
                init.type =  "Spectral")

# Create a text file to look at the topics
sink("pill_stm0prv_terms.txt")
labelTopics(pill_stm0prv, n = 15)
sink()

# k = 0 
pill_stm0 <- stm(docs, vocab, K = 0,
                #prevalence = ~ as.factor(source), 
                data = meta,
                init.type =  "Spectral")



# Create a text file to look at the topics
sink("pill_stm0_terms.txt")
labelTopics(pill_stm0, n = 15)
sink()

# k = 10 & prevalence
pill_stm10prv <- stm(docs, vocab, K = 10,
                 prevalence = ~ as.factor(source), 
                 data = meta,
                 init.type =  "Spectral")

# Create a text file to look at the topics
sink("pill_stm10prv_terms.txt")
labelTopics(pill_stm10prv, n = 15)
sink()

# k = 30 & prevalence
pill_stm30prv <- stm(docs, vocab, K = 30,
                     prevalence = ~ as.factor(source), 
                     data = meta,
                     init.type =  "Spectral")

# Create a text file to look at the topics
sink("pill_stm30prv_terms.txt")
labelTopics(pill_stm30prv, n = 15)
sink()

# k = 50 & prevalence
pill_stm50prv <- stm(docs, vocab, K = 50,
                     prevalence = ~ as.factor(source), 
                     data = meta,
                     init.type =  "Spectral")

# Create a text file to look at the topics
sink("pill_stm50prv_terms.txt")
labelTopics(pill_stm30prv, n = 15)
sink()

#-----#-----#-----#-----#-----#-----#-----#

#Results of the qualitative content analysis: 
#72 of 87 topics were retained
# 9 of the 10 topics were retained.

# effects of the source on the topics
pill_stm0prv_effects <- estimateEffect(1:87 ~ as.factor(source),
                                   stmobj = pill_stm0prv,
                                   metadata = meta,
                                   uncertainty = "None")

sink("pill_stm0prv_effects.txt")
summary(pill_stm0prv_effects)
sink()

stm0prv_effects <- summary(pill_stm0prv_effects)
typeof(stm0prv_effects)


retain_sig_topics <- \(totalTopics){
  
  p_value <- stm0prv_effects %>% 
  pluck("tables", totalTopics, 8)
 tibble(p_value, topic_no = 1:totalTopics)

}


stm0prv_effects 
  pluck(tables)


#Highest probability of observing a work

#FREX - words that occur together and don't co-ccur with other words


# Create matrix of topic scores/values
pill_stm0 <- tidy(pill_stm0, matrix = "gamma")

pill_stm0 <- pill_stm0 %>%
  spread(key = topic, value = gamma)

# source ~ all cols - 1
pres_reg <- cbind(meta, 
                  pill_stm0[,2:95])

View(pres_reg )

# Cosine similarity - in these forums to what extent are males and females speacking similarly on these topics
