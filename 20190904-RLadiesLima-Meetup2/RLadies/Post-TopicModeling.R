# libraries
# library(tidyverse)
library(dplyr)
library(tidyr) # spread
library(ggplot2)
# library(tm)
library(tidytext)
library(topicmodels)
library(ggwordcloud)
library(cowplot)
library(viridis)
library(RColorBrewer)
# library(gridExtra)
# library(grid)

# main dataset

data_decade <- read.csv(file = "cleaned_papers_all_years_simple_1000.csv",stringsAsFactors = FALSE)

data_decade_summ <- data_decade %>% select(doi,pubyear)

############ ANALYSIS FROM THE TOPICS #################

# LOAD LDA data

modk <- readRDS(file = "Topics.rds")

############# WORDCLOUDS ######################

# 1. Topic as a mixture of words
# beta: the probability of that term being generated from that topic.
papers_beta <- tidytext::tidy(modk, matrix = "beta")
# papers_beta
head(papers_beta)

# topic_1 <- papers_beta %>% filter(topic == 1 & beta > 0.001) %>%  select(term,beta)
topic_sample <- papers_beta %>% filter(beta > 0.005) #%>%  select(term,beta)

table_topic <- topic_sample %>%
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 2, 4, 2, 1)))

option_par <- "D"

plot_w <- ggplot(table_topic, aes(label = term, size = beta, angle = angle, 
                                  color = beta)) +
  geom_text_wordcloud_area(rm_outside = TRUE, eccentricity = 1) + #area_corr_power = 1,
  scale_size_area(max_size = 30) +
  theme_minimal()+ facet_wrap(~topic) + 
  scale_color_viridis(direction = -1, option = option_par) 

height_par <- 26
width_par <- 26

  ggsave(plot=plot_w,filename="wordcloud_topics.pdf", height=height_par, width = width_par, units = "in")


############ NUMBER OF PAPERS RELATED TO EACH TOPIC #################

papers_gamma <- tidytext::tidy(modk, matrix = "gamma")
head(papers_gamma)

# Each of these values is an estimated proportion of words from that document that are generated from that topic.
# For example, the model estimates that only about 12.3% of the words in document 2 were generated from topic 1.


# Long format to short format
papers_gamma_short <- spread(papers_gamma, key = topic, value = gamma)
names(papers_gamma_short) <- c("document", paste("Topic", names(papers_gamma_short)[-1])) 

papers_summ <- cbind.data.frame(doi = as.character(papers_gamma_short$document),
                                topic_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,which.max),
                                gamma_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,max))


# now we have to count and make a data frame
papers_summ$topic_max <- as.factor(papers_summ$topic_max)
topics_count <- papers_summ %>% select(doi, topic_max) %>% 
  group_by(topic_max) %>% 
  summarise(n())
colnames(topics_count) <- c("Topic","num_papers")


topics_count <- topics_count %>% 
  # arrange(desc(num_papers))
  arrange(num_papers)
topics_count$Topic <- factor(topics_count$Topic,levels=topics_count$Topic)



ggplot(topics_count, aes(x=Topic,y=num_papers)) +
  geom_bar(stat="identity",position="identity") +
  coord_flip() +
  xlab("Topic") + ylab("Number of articles") +
  theme_cowplot() + theme(text = element_text(size=20),axis.text.y = element_text(size=16))
