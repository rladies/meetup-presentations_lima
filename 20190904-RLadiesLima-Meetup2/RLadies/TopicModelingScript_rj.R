# libraries
library(tidyverse)
library(tm)
library(tidytext)
library(topicmodels)
library(gridExtra)
library(grid)
library(parallel)

# READ DATA ---------------------------------------------------------------
datosFinales <- read.csv("cleaned_TidyData_TopicModeling.csv", stringsAsFactors = FALSE)

# glimpse(datosFinales)


# DOCUMENT TERM MATRIX ----------------------------------------------------
datosFreq <- datosFinales %>%
  # Obtain word frequencies
  count(paper, word) 

# Document Term Matrix
datos_dtm <- datosFreq %>%
  # Specify the token
  cast_dtm(document = paper, term = word, value = n)


# TOPIC MODELLING ---------------------------------------------------------

### Many groups!

modk <- LDA(x = datos_dtm, k = 10, method = "Gibbs", control = list(alpha=1, delta= 0.1, seed = 10005))

saveRDS(modk,file = "Topics.rds")

