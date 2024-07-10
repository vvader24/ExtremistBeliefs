# IMPORT REDDIT DATA FROM ARCTIC SHIFT 
# DOWNLOAD JSONL FILE FROM ARCTIC SHIFT AT 
# https://arctic-shift.photon-reddit.com/download-tool 

library(dplyr)
library(jsonlite)
library(writexl)

data<-stream_in(file(file.choose())) #posts
#data<-tidyr::unnest(data)
data2<-stream_in(file(file.choose())) #comments

#View(posts)

posts<-data.frame("author" = data$author,
                  "author_fullname" = data$author_fullname,
                  "title" = data$title,
                  "selftext" = data$selftext,
                  "created_utc" = data$created_utc,
                  "score" = data$score,
                  "num_comments" = data$num_comments,
                  "parent_id" = data$id,
                  "permalink" = data$permalink,
                  "retrieved_on" = data$retrieved_on,
                  "subreddit" = data$subreddit,
                  "subreddit_id" = data$subreddit_id,
                  "subreddit_subscribers" = data$subreddit_subscribers,
                  "domain" = data$domain,
                  "url" = data$url,
                  "is_created_from_ads_ui" = data$is_created_from_ads_ui)

posts$created_utc<-as.numeric(posts$created_utc)
posts$created_utc<-as.POSIXct(posts$created_utc, origin="1970-01-01")
posts$retrieved_on<-as.numeric(posts$retrieved_on)
posts$retrieved_on<-as.POSIXct(posts$created_utc, origin="1970-01-01")
posts$permalink<-paste0("https://www.reddit.com",posts$permalink)

comments<-data.frame("author" = data2$author,
                     "author_fullname" = data2$author_fullname,
                     "comment" = data2$body,
                     "created_utc" = data2$created_utc,
                     "score" = data2$score,
                     "id" = data2$id,
                     "permalink" = data2$permalink,
                     "link_id" = data2$link_id,
                     "parent_id" = data2$parent_id,
                     "retrieved_on" = data2$retrieved_on,
                     "subreddit" = data2$subreddit,
                     "subreddit_id" = data2$subreddit_id)

comments$created_utc<-as.numeric(comments$created_utc)
comments$created_utc<-as.POSIXct(comments$created_utc, origin="1970-01-01")
comments$retrieved_on<-as.numeric(comments$retrieved_on)
comments$retrieved_on<-as.POSIXct(comments$retrieved_on, origin="1970-01-01")
comments$parent_id<-gsub("t1_|t2_|t3_", "", comments$parent_id)
comments$permalink<-paste0("https://www.reddit.com",comments$permalink)

combined = merge(x=posts, y=comments, by="parent_id", all.y=TRUE)
combined<-combined %>% filter(!is.na(author.x))
combined<-combined %>% filter(!is.na(author_fullname.y))

sheets<-list("posts" = posts, "comments" = comments)
write_xlsx(sheets, paste0("reddit_data_", format(Sys.time(), "%d-%b-%Y_%H.%M.%S"),".xlsx"))
write.csv(combined, paste0("combined_reddit_data_",format(Sys.time(), "%d-%b-%Y_%H.%M.%S"),".csv"))

 #write.csv(posts, paste0("posts_redpillwomen",format(Sys.time(), "%d-%b-%Y_%H.%M.%S"),".csv"))


#create and export edge table for gephi network analysis
edges<-data.frame("target" = posts$author.x, 
                  "source" = posts$author.y, 
                  "date" = posts$created_utc.y, 
 #                 "title" = posts$title, 
 #                 "comment" = posts$comment, 
                  "score" = posts$score.y)
edges<-as.data.frame(na.omit(edges))
edges<-edges %>% filter(source != "[deleted]")
edges<-edges %>% filter(target != "[deleted]")
write.csv(edges, paste0("reddit_edges_", format(Sys.time(), "%d-%b-%Y_%H.%M.%S"),".csv"), row.names = FALSE)


#install.packages("udpipe")
library(udpipe)
ud_model <- udpipe_download_model(language = "english") #download English model
ud_model <- udpipe_load_model(ud_model$file_model) #load model

#txt<-c("I don't know about you, but I believe that this institute is full of brilliant people.") #sample text
x <- udpipe_annotate(ud_model, x = posts$title) #run model on sample text
x <- data.frame(x) #copy results to data frame

#View(x)

library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
#------Load session----#
load("~/Desktop/ExtremistBeliefs/posts&comments_redpillwomen.RData")
barchart(key ~ freq, data = stats, col = "orangered", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Frequency")

## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma) #stats$token
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")


## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$lemma)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring adjectives", xlab = "Freq")

## Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")


## Using Pointwise Mutual Information Collocations
x$word <- tolower(x$token)
stats <- keywords_collocation(x = x, term = "word", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ pmi, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by PMI Collocation", 
         xlab = "PMI (Pointwise Mutual Information)")


## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")


cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")



cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")


x$id <- unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm <- subset(x, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y)


# TOPIC MODELING

library(udpipe)
comments <- subset(brussels_reviews, language %in% "fr")

ud_model <- udpipe_download_model(language = "french")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = comments$feedback, doc_id = comments$id)
x <- as.data.frame(x)

View(x)

x$topic_level_id <- unique_identifier(x, fields = c("doc_id", "paragraph_id", "sentence_id"))
## Get a data.frame with 1 row per id/lemma
dtf <- subset(x, upos %in% c("NOUN"))
dtf <- document_term_frequencies(dtf, document = "topic_level_id", term = "lemma")
head(dtf)

## Create a document/term/matrix for building a topic model
dtm <- document_term_matrix(x = dtf)
## Remove words which do not occur that much
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)
head(dtm_colsums(dtm_clean))


#FIGURE THIS ONE
## Remove nouns which you really do not like (mostly too common nouns)
dtm_clean <- dtm_remove_terms(dtm_clean, terms = c("appartement", "appart", "eter"))
## Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency
dtm_clean <- dtm_remove_tfidf(dtm_clean, top = 50)

#install.packages("topicmodels")
library(topicmodels)
m <- LDA(dtm_clean, k = 4, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))

scores <- predict(m, newdata = dtm, type = "topics", 
                  labels = c("labela", "labelb", "labelc", "labeld"))
str(scores)

predict(m, type = "terms", min_posterior = 0.05, min_terms = 3)



## Build document term matrix on nouns/adjectives only
dtf <- subset(x, upos %in% c("NOUN", "ADJ") & 
                !lemma %in% c("appartement", "appart", "eter", "tres"))
dtf <- document_term_frequencies(dtf, document = "topic_level_id", term = "lemma")
dtm <- document_term_matrix(x = dtf)
dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)
## Build topic model + get topic terminology
m <- LDA(dtm_clean, k = 4, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
topicterminology <- predict(m, type = "terms", min_posterior = 0.025, min_terms = 5)
scores <- predict(m, newdata = dtm, type = "topics")


library(igraph)
library(ggraph)
library(ggplot2)
x_topics <- merge(x, scores, by.x="topic_level_id", by.y="doc_id")
wordnetwork <- subset(x_topics, topic %in% 1 & lemma %in% topicterminology[[1]]$term)
wordnetwork <- cooccurrence(wordnetwork, group = c("topic_level_id"), term = "lemma")
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink")  +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words in topic 1 ", subtitle = "Nouns & Adjective cooccurrence")


topicterminology <- predict(m, type = "terms", min_posterior = 0.05, min_terms = 10)
termcorrs <- subset(x_topics, topic %in% 1 & lemma %in% topicterminology[[1]]$term)
termcorrs <- document_term_frequencies(termcorrs, document = "topic_level_id", term = "lemma")
termcorrs <- document_term_matrix(termcorrs)
termcorrs <- dtm_cor(termcorrs)
termcorrs[lower.tri(termcorrs)] <- NA
diag(termcorrs) <- NA
library(qgraph)
qgraph(termcorrs, layout = "spring", labels = colnames(termcorrs), directed = FALSE,
       borders = FALSE, label.scale = FALSE, label.cex = 1, node.width = 0.5)



## Find keywords with RAKE 
keyw_rake <- keywords_rake(x, 
                           term = "token", group = c("doc_id", "paragraph_id", "sentence_id"), 
                           relevant = x$upos %in% c("NOUN", "ADJ"), 
                           ngram_max = 3, n_min = 5)
## Find simple noun phrases
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
keyw_nounphrases <- keywords_phrases(x$phrase_tag, term = x$token, 
                                     pattern = "(A|N)*N(P+D*(A|N)*N)*", is_regex = TRUE, 
                                     detailed = FALSE)
keyw_nounphrases <- subset(keyw_nounphrases, ngram > 1)

## Recode terms to keywords
x$term <- x$token
x$term <- txt_recode_ngram(x$term, 
                           compound = keyw_rake$keyword, ngram = keyw_rake$ngram)
x$term <- txt_recode_ngram(x$term, 
                           compound = keyw_nounphrases$keyword, ngram = keyw_nounphrases$ngram)
## Keep keyword or just plain nouns
x$term <- ifelse(x$upos %in% "NOUN", x$term,
                 ifelse(x$term %in% c(keyw_rake$keyword, keyw_nounphrases$keyword), x$term, NA))

## Build document/term/matrix
dtm <- document_term_frequencies(x, document = "topic_level_id", term = "term")
dtm <- document_term_matrix(x = dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)


m <- LDA(dtm, k = 3, method = "Gibbs", 
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
topicterminology <- predict(m, type = "terms", min_posterior = 0.10, min_terms = 3)
topicterminology