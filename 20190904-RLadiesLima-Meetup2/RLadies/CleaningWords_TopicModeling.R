# LIBRARIES
library(tidyr)
library(dplyr)
library(tidytext)
library(stringr)
library(tm) # removing words in Spanish
library(textstem) # lemmatizing # to install textstem, error when installing statnet.common because my R version is 3.4, so had to work with 4.1.4 statnet.common

############

datosOriginales <- read.csv("cleaned_papers_all_years_simple_1000.csv", stringsAsFactors = FALSE)

# DATA OF INTEREST --------------------------------------------------------

# 1.Filter columns of interest
test <- datosOriginales %>% 
  mutate(paper = doi) %>% 
  select(paper, abstract) 

# Separating the abstracts by word
df_words <- test %>% 
  unnest_tokens(output = word, # name of the new column
                input = abstract, # column that is going to be splitted
                token = "words", # unit of splitting
                format = "text") 

# Remove English stopwords 
df_words <- df_words %>%
  anti_join(stop_words)

# Remove other unuseful words
bad.words <- c("use","using","used","however","based","high",
               "show","shown","higher","term","can","non","first","two","three",
               "one","also","may","well","among","elsevier")
bad.words <- data.frame(word = bad.words, stringsAsFactors = FALSE)

df_words <- df_words %>% 
  anti_join(bad.words)


# Homogenizing the American and British words
Am2Br <- read.csv("Am2Br.csv", stringsAsFactors = FALSE)
names(Am2Br) <- c("Am","Br")

# Function created by RJ
Americanizing <- function(words,Am2Br){
  Am <- gsub("[[:punct:]]", "", Am2Br$Am)
  Br <- gsub("[[:punct:]]", "", Am2Br$Br)
  word.word <- gsub("[[:punct:]]", "",tolower(unlist(strsplit(words, " "))))
  unlist(lapply(word.word,function(x){
    ind.match <- match(x,Br)
    # print(ind.match)
    if (is.na(ind.match)==FALSE){
      Am.word <- Am[ind.match]
    }else{
      Am.word <- x
    }
    # print(Am.word)
    return(Am.word)
  }))
}


system.time(
Rep.Word <- Americanizing(df_words$word, Am2Br)
)

#  user  system elapsed 
# 78.853   0.197  79.069 


# Adding the new words
df_words$NewWords <- Rep.Word


# Removing Numbers
# http://stla.github.io/stlapblog/posts/Numextract.html
indexRemoveNumbers <- str_detect(df_words$NewWords, "\\-*\\d+\\.*\\d*")
df_words2 <- df_words[!indexRemoveNumbers,]

df_words2 <- df_words2 %>%
  select(paper, NewWords)

datos <- df_words2 

# Find the abbreviation 'km' and replace it with 'kilometer'
indexkm <- which(datos$NewWords == 'km')
datos$NewWords[indexkm] <- 'kilometer'

# Find the abbreviation 'tdrs' and replace it with 'tdr'
indextdrs <- which(datos$NewWords == 'tdrs')
datos$NewWords[indextdrs] <- 'tdr'


# Remove the abbreviations 'eg' and 'ie'
abbr <- data.frame(NewWords = c("eg","ie"), stringsAsFactors = FALSE)

datos2 <- datos %>% 
  anti_join(abbr)

# Removing spanish stopwords

spanishSW <- data.frame(NewWords = stopwords(kind = "sp"), stringsAsFactors = FALSE)
datos2 <- datos2 %>% 
  anti_join(spanishSW)


# LEMMATIZING
system.time(prueba <- lemmatize_words(datos2$NewWords))

# user  system elapsed 
# 0.418   0.024   0.444 

# Adding the root to the dataframe datos2
datos2$word <- prueba 

# Editing 'gp' (Column word) to 'gps'
prueba1 <- which(datos2$NewWords=='gps')
datos2$word[prueba1] <- 'gps'

# Remove the rest of the abbreviations 'gp' meaning 'grazed pixels'
prueba2 <- which(datos2$word=='gp')

datos3 <- datos2[datos2$word!='gp',]

# FINALLY
# TIDY DATA
datosFinales <- datos3 %>% 
  select(paper, word)

# Save data in the project
write.csv(x = datosFinales, file = "cleaned_TidyData_TopicModeling.csv", row.names = FALSE)


