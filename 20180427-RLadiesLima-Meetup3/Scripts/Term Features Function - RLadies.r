###############################################################
## Developed by Leda Basombrio April 2017
## Function for creating term features from text vector


term_features = function(text_var, # Text Vector
                         minfreq=150,  # Min freq of term in documents for extraction
                         tag=""  # Suffix tag for term columns
                         ) {
  
  #require(tm,quietly=T)
  #require(SnowballC,quietly=T)
  
  # Creating corpus
  corpus <- VCorpus(VectorSource(text_var))
  
  # Converting to lower case
  corpus2 <- tm_map(corpus, content_transformer(tolower))
  
  # Function to replace old word with new word
  replaceWord = function(corpus,oldword,newword) {
    tm_map(corpus,content_transformer(gsub), pattern=oldword,replacement = newword)
  }
  
  # Removing URLs
  corpus2 = replaceWord(corpus2, "http[^[:space:]]*","")
  corpus2 = replaceWord(corpus2, "www[^[:space:]]*","")
  
  # Changing names of bank´s user ids 
  corpus2 = replaceWord(corpus2, "@bcpcomunica","idbcpcomunica")
  corpus2 = replaceWord(corpus2, "@scotiabankpe","idscotiabankpe")
  corpus2 = replaceWord(corpus2, "@bbvacontinental","idbbvacontinental")
  corpus2 = replaceWord(corpus2, "@prensabbva_pe","idprensabbva_pe")
  corpus2 = replaceWord(corpus2, "@interbank","idinterbank")
  
  # Arranging some words
  corpus2 = replaceWord(corpus2, "cta","cuenta")
  corpus2 = replaceWord(corpus2, "via bcp","viabcp")
  corpus2 = replaceWord(corpus2, " S/"," soles")
  corpus2 = replaceWord(corpus2, "jajajaja","jaja")
  corpus2 = replaceWord(corpus2, "jajaja","jaja")
  corpus2 = replaceWord(corpus2, "tuit","tweet")
  corpus2 = replaceWord(corpus2, "san isidro","sanisidro")
  corpus2 = replaceWord(corpus2, "san miguel","sanmiguel")
  corpus2 = replaceWord(corpus2, "san borja","sanborja")
  corpus2 = replaceWord(corpus2, "sres","señores")
  
  # Remove tweeter users
  corpus2 = replaceWord(corpus2, "(^|[^@\\w])@(\\w{1,15})\\b","")
  
  # Removing numbers
  corpus2 <- tm_map(corpus2, removeNumbers)
  
  # Removing punctuation
  corpus2 <- tm_map(corpus2, removePunctuation)
  
  # Stopwords
  stopwords_list = c('a','aca','actualmente','adelante','ademas','afirmo','agrego','ahi','ahora','ajena','ajenas','ajeno','ajenos','al','algo','algun','alguna','algunas','alguno','algunos','alla','alli','alrededor','ambos','ampleamos','ante','anterior','antes','añadio','apenas','aproximadamente','aquel','aquella','aquellas','aquello','aquellos','aqui','arriba','aseguro','asi','atras','aun','aunque','ayer','bajo','bastante','bien','buen','buena','buenas','bueno','buenos','cabe','cada','casi','cerca','cierta','ciertas','cierto','ciertos','cinco','comento','como','con','conmigo','conocer','conseguimos','conseguir','considera','considero','consigo','consigue','consiguen','consigues','contigo','contra','cosas','creo','cual','cuales','cualquier','cualquiera','cualquieras','cuan','cuando','cuanta','cuantas','cuanto','cuantos','cuatro','cuenta','da','dado','dan','dar','de','debe','deben','debido','decir','dejar','dejo','del','demas','demasiada','demasiadas','demasiado','demasiados','dentro',
                     'desde','despues','dice','dicen','dicho','dieron','diferente','diferentes','dijeron','dijo','dio','donde','dos','durante','e','ejemplo','el','ella','ellas','ello','ellos','embargo','empleais','emplean','emplear','empleas','empleo','en','encima','encuentra','entonces','entre','era','erais','eramos','eran','eras','eres','es','esa','esas','ese','eso','esos','esta','estaba','estabais','estabamos','estaban','estabas','estad','estada','estadas','estado','estados','estais','estamos','estan','estando','estar','estara','estaran','estaras','estare','estareis','estaremos','estaria','estariais','estariamos','estarian','estarias','estas','este','esteis','estemos','esten','estes','esto','estos','estoy','estuve','estuviera','estuvierais','estuvieramos','estuvieran','estuvieras','estuvieron','estuviese','estuvieseis','estuviesemos','estuviesen','estuvieses','estuvimos','estuviste','estuvisteis','estuvo','etc','ex','existe','existen','explico','expreso','fin','fue','fuera','fuerais','fueramos','fueran','fueras','fueron','fuese','fueseis','fuesemos','fuesen','fueses','fui','fuimos','fuiste','fuisteis','gran','grandes','gueno','ha','habeis','haber','habia','habiais','habiamos','habian','habias','habida','habidas','habido','habidos',
                     'habiendo','habra','habran','habras','habre','habreis','habremos','habria','habriais','habriamos','habrian','habrias','hace','haceis','hacemos','hacen','hacer','hacerlo','haces','hacia','haciendo','hago','han','has','hasta','hay','haya','hayais','hayamos','hayan','hayas','he','hecho','hemos','hicieron','hizo','hoy','hube','hubiera','hubierais','hubieramos','hubieran','hubieras','hubieron','hubiese','hubieseis','hubiesemos','hubiesen','hubieses','hubimos','hubiste','hubisteis','hubo','igual','incluso','indico','informo','intenta','intentais','intentamos','intentan','intentar','intentas','intento','ir','jamas','junto','juntos','la','lado','largo','las','le','les','llego','lleva','llevar','lo','los','luego','lugar','manera','manifesto','mas','mayor','me','mediante','mejor','menciono','menos','mi','mia','mias','mientras','mio','mios','mis','misma','mismas','mismo','mismos','modo','momento','mucha','muchas','muchisima','muchisimas','muchisimo','muchisimos','mucho','muchos','muy','nada','nadie','ni','ningun','ninguna','ningunas','ninguno','ningunos','no','nos','nosotras','nosotros','nuestra','nuestras','nuestro','nuestros','nueva','nuevas','nuevo','nuevos','nunca','o','ocho','os','otra','otras','otro','otros','para','parece',
                     'parecer','parte','partir','pasada','pasado','pero','pesar','poca','pocas','poco','pocos','podeis','podemos','poder','podra','podran','podria','podriais','podriamos','podrian','podrias','poner','por','por que','porque','posible','primer','primera','primero','primero desde','primeros','principalmente','propia','propias','propio','propios','proximo','proximos','pudo','pueda','puede','pueden','puedo','pues','que','quedo','queremos','querer','quien','quienes','quienesquiera','quienquiera','quiere','quiza','quizas','realizado','realizar','realizo','respecto','sabe','sabeis','sabemos','saben','saber','sabes','se','sea','seais','seamos','sean','seas','segun','segunda','segundo','seis','señalo','ser','sera','seran','seras','sere','sereis','seremos','seria','seriais','seriamos','serian','serias','si','sido','siempre','siendo','siete','sigue','siguiente','sin','sino','so','sobre','sois','sola','solamente','solas','solo','solos','somos','son','soy','sr','sra','sres','sta','su','sus','suya','suyas','suyo','suyos','tal','tales','tambien','tampoco','tan','tanta','tantas','tanto','tantos','te','tendra','tendran','tendras','tendre','tendreis','tendremos','tendria','tendriais','tendriamos','tendrian','tendrias','tened','teneis','tenemos',
                     'tener','tenga','tengais','tengamos','tengan','tengas','tengo','tenia','teniais','teniamos','tenian','tenias','tenida','tenidas','tenido','tenidos','teniendo','tercera','ti','tiempo','tiene','tienen','tienes','toda','todas','todavia','todo','todos','tomar','total','trabaja','trabajais','trabajamos','trabajan','trabajar','trabajas','trabajo','tras','trata','traves','tres','tu','tus','tuve','tuviera','tuvierais','tuvieramos','tuvieran','tuvieras','tuvieron','tuviese','tuvieseis','tuviesemos','tuviesen','tuvieses','tuvimos','tuviste','tuvisteis','tuvo','tuya','tuyas','tuyo','tuyos','ultima','ultimas','ultimo','ultimos','un','una','unas','uno','unos','usa','usais','usamos','usan','usar','usas','uso','usted','ustedes','va','vais','valor','vamos','van','varias','varios','vaya','veces','ver','verdad','verdadera','vez','vosotras','vosotros','voy','vuestra','vuestras','vuestro','vuestros','y','ya','yo','mañana','san','d','in','q','xq','yes','ah','oh','rt')
  
  # Removing spanish stop words
  corpus2 <- tm_map(corpus2, removeWords, stopwords_list)
  
  # Removing extra whitespaces
  corpus2 <- tm_map(corpus2, stripWhitespace)
  
  # Stemming 
  corpus2_stem <- tm_map(corpus2, stemDocument, "spanish") 
  
  ## MISSING STEM COMPLETION!!!!!
  
  # Term Frequency Uni and Bigram
  #require(RWeka,quietly=T)
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
  
  termmatrix = DocumentTermMatrix(corpus2_stem,
                                  control=list(
                                    tokenize=BigramTokenizer,
                                    wordLengths = c(1, Inf) ))
  
  word_min = findFreqTerms(termmatrix, lowfreq=minfreq)
  
  termmatrix = DocumentTermMatrix(corpus2_stem,
                                  control=list(
                                    tokenize=BigramTokenizer,
                                    wordLengths = c(1, Inf),
                                    dictionary=word_min ))
  
  var_termfreq <- as.data.frame(inspect(termmatrix ))
  names(var_termfreq) = gsub("ñ","nh",gsub(" ",".",paste0(names(var_termfreq),tag)))
  
  # Term Frequency Uni and Bigram, Weighted
  termmatrix_w = DocumentTermMatrix(corpus2_stem,
                                    control=list(
                                      tokenize=BigramTokenizer,
                                      wordLengths = c(1, Inf),
                                      weighting = function(x) weightTfIdf(x, normalize = FALSE),
                                      dictionary=word_min ))
  
  var_termfreq_w <- as.data.frame(inspect(termmatrix_w ))
  names(var_termfreq_w) = gsub("ñ","nh",gsub(" ",".",paste0(names(var_termfreq_w),"_w",tag)))
  
  var_total = cbind(var_termfreq,var_termfreq_w )
  
  return(var_total)
  
}

###############################################################
### Function for creating table for Word Cloud


term_wordcloud = function(text_var, # Text Vector
                         minfreq=500,  # Min freq of term in documents for extraction
                         tag=""  # Suffix tag for term columns
) {
  
  #require(tm,quietly=T)
  #require(SnowballC,quietly=T)
  
  # Creating corpus
  corpus <- VCorpus(VectorSource(text_var))
  
  # Converting to lower case
  corpus2 <- tm_map(corpus, content_transformer(tolower))
  
  # Function to replace old word with new word
  replaceWord = function(corpus,oldword,newword) {
    tm_map(corpus,content_transformer(gsub), pattern=oldword,replacement = newword)
  }
  
  # Removing URLs
  corpus2 = replaceWord(corpus2, "http[^[:space:]]*","")
  corpus2 = replaceWord(corpus2, "www[^[:space:]]*","")
  
  # Changing names of bank´s user ids 
  corpus2 = replaceWord(corpus2, "@bcpcomunica","idbcpcomunica")
  corpus2 = replaceWord(corpus2, "@scotiabankpe","idscotiabankpe")
  corpus2 = replaceWord(corpus2, "@bbvacontinental","idbbvacontinental")
  corpus2 = replaceWord(corpus2, "@prensabbva_pe","idprensabbva_pe")
  corpus2 = replaceWord(corpus2, "@interbank","idinterbank")
  
  # Arranging some words
  corpus2 = replaceWord(corpus2, "cta","cuenta")
  corpus2 = replaceWord(corpus2, "via bcp","viabcp")
  corpus2 = replaceWord(corpus2, " S/"," soles")
  corpus2 = replaceWord(corpus2, "jajajaja","jaja")
  corpus2 = replaceWord(corpus2, "jajaja","jaja")
  corpus2 = replaceWord(corpus2, "tuit","tweet")
  corpus2 = replaceWord(corpus2, "san isidro","sanisidro")
  corpus2 = replaceWord(corpus2, "san miguel","sanmiguel")
  corpus2 = replaceWord(corpus2, "san borja","sanborja")
  corpus2 = replaceWord(corpus2, "sres","señores")
  
  # Remove tweeter users
  corpus2 = replaceWord(corpus2, "(^|[^@\\w])@(\\w{1,15})\\b","")
  
  # Removing numbers
  corpus2 <- tm_map(corpus2, removeNumbers)
  
  # Removing punctuation
  corpus2 <- tm_map(corpus2, removePunctuation)
  
  # Stopwords
  stopwords_list = c('a','aca','actualmente','adelante','ademas','afirmo','agrego','ahi','ahora','ajena','ajenas','ajeno','ajenos','al','algo','algun','alguna','algunas','alguno','algunos','alla','alli','alrededor','ambos','ampleamos','ante','anterior','antes','añadio','apenas','aproximadamente','aquel','aquella','aquellas','aquello','aquellos','aqui','arriba','aseguro','asi','atras','aun','aunque','ayer','bajo','bastante','bien','buen','buena','buenas','bueno','buenos','cabe','cada','casi','cerca','cierta','ciertas','cierto','ciertos','cinco','comento','como','con','conmigo','conocer','conseguimos','conseguir','considera','considero','consigo','consigue','consiguen','consigues','contigo','contra','cosas','creo','cual','cuales','cualquier','cualquiera','cualquieras','cuan','cuando','cuanta','cuantas','cuanto','cuantos','cuatro','cuenta','da','dado','dan','dar','de','debe','deben','debido','decir','dejar','dejo','del','demas','demasiada','demasiadas','demasiado','demasiados','dentro',
                     'desde','despues','dice','dicen','dicho','dieron','diferente','diferentes','dijeron','dijo','dio','donde','dos','durante','e','ejemplo','el','ella','ellas','ello','ellos','embargo','empleais','emplean','emplear','empleas','empleo','en','encima','encuentra','entonces','entre','era','erais','eramos','eran','eras','eres','es','esa','esas','ese','eso','esos','esta','estaba','estabais','estabamos','estaban','estabas','estad','estada','estadas','estado','estados','estais','estamos','estan','estando','estar','estara','estaran','estaras','estare','estareis','estaremos','estaria','estariais','estariamos','estarian','estarias','estas','este','esteis','estemos','esten','estes','esto','estos','estoy','estuve','estuviera','estuvierais','estuvieramos','estuvieran','estuvieras','estuvieron','estuviese','estuvieseis','estuviesemos','estuviesen','estuvieses','estuvimos','estuviste','estuvisteis','estuvo','etc','ex','existe','existen','explico','expreso','fin','fue','fuera','fuerais','fueramos','fueran','fueras','fueron','fuese','fueseis','fuesemos','fuesen','fueses','fui','fuimos','fuiste','fuisteis','gran','grandes','gueno','ha','habeis','haber','habia','habiais','habiamos','habian','habias','habida','habidas','habido','habidos',
                     'habiendo','habra','habran','habras','habre','habreis','habremos','habria','habriais','habriamos','habrian','habrias','hace','haceis','hacemos','hacen','hacer','hacerlo','haces','hacia','haciendo','hago','han','has','hasta','hay','haya','hayais','hayamos','hayan','hayas','he','hecho','hemos','hicieron','hizo','hoy','hube','hubiera','hubierais','hubieramos','hubieran','hubieras','hubieron','hubiese','hubieseis','hubiesemos','hubiesen','hubieses','hubimos','hubiste','hubisteis','hubo','igual','incluso','indico','informo','intenta','intentais','intentamos','intentan','intentar','intentas','intento','ir','jamas','junto','juntos','la','lado','largo','las','le','les','llego','lleva','llevar','lo','los','luego','lugar','manera','manifesto','mas','mayor','me','mediante','mejor','menciono','menos','mi','mia','mias','mientras','mio','mios','mis','misma','mismas','mismo','mismos','modo','momento','mucha','muchas','muchisima','muchisimas','muchisimo','muchisimos','mucho','muchos','muy','nada','nadie','ni','ningun','ninguna','ningunas','ninguno','ningunos','no','nos','nosotras','nosotros','nuestra','nuestras','nuestro','nuestros','nueva','nuevas','nuevo','nuevos','nunca','o','ocho','os','otra','otras','otro','otros','para','parece',
                     'parecer','parte','partir','pasada','pasado','pero','pesar','poca','pocas','poco','pocos','podeis','podemos','poder','podra','podran','podria','podriais','podriamos','podrian','podrias','poner','por','por que','porque','posible','primer','primera','primero','primero desde','primeros','principalmente','propia','propias','propio','propios','proximo','proximos','pudo','pueda','puede','pueden','puedo','pues','que','quedo','queremos','querer','quien','quienes','quienesquiera','quienquiera','quiere','quiza','quizas','realizado','realizar','realizo','respecto','sabe','sabeis','sabemos','saben','saber','sabes','se','sea','seais','seamos','sean','seas','segun','segunda','segundo','seis','señalo','ser','sera','seran','seras','sere','sereis','seremos','seria','seriais','seriamos','serian','serias','si','sido','siempre','siendo','siete','sigue','siguiente','sin','sino','so','sobre','sois','sola','solamente','solas','solo','solos','somos','son','soy','sr','sra','sres','sta','su','sus','suya','suyas','suyo','suyos','tal','tales','tambien','tampoco','tan','tanta','tantas','tanto','tantos','te','tendra','tendran','tendras','tendre','tendreis','tendremos','tendria','tendriais','tendriamos','tendrian','tendrias','tened','teneis','tenemos',
                     'tener','tenga','tengais','tengamos','tengan','tengas','tengo','tenia','teniais','teniamos','tenian','tenias','tenida','tenidas','tenido','tenidos','teniendo','tercera','ti','tiempo','tiene','tienen','tienes','toda','todas','todavia','todo','todos','tomar','total','trabaja','trabajais','trabajamos','trabajan','trabajar','trabajas','trabajo','tras','trata','traves','tres','tu','tus','tuve','tuviera','tuvierais','tuvieramos','tuvieran','tuvieras','tuvieron','tuviese','tuvieseis','tuviesemos','tuviesen','tuvieses','tuvimos','tuviste','tuvisteis','tuvo','tuya','tuyas','tuyo','tuyos','ultima','ultimas','ultimo','ultimos','un','una','unas','uno','unos','usa','usais','usamos','usan','usar','usas','uso','usted','ustedes','va','vais','valor','vamos','van','varias','varios','vaya','veces','ver','verdad','verdadera','vez','vosotras','vosotros','voy','vuestra','vuestras','vuestro','vuestros','y','ya','yo','mañana','san','d','in','q','xq','yes','ah','oh','rt')
  
  # Removing spanish stop words
  corpus2 <- tm_map(corpus2, removeWords, stopwords_list)
  
  # Removing extra whitespaces
  corpus2 <- tm_map(corpus2, stripWhitespace)
  
  # Stemming 
  corpus2_stem <- tm_map(corpus2, stemDocument, "spanish") 
  
  ## MISSING STEM COMPLETION!!!!!
  
  # Term Frequency Uni and Bigram
  #require(RWeka,quietly=T)
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
  
  termmatrix = DocumentTermMatrix(corpus2_stem,
                                  control=list(
                                    tokenize=BigramTokenizer,
                                    wordLengths = c(1, Inf) ))
  
  word_min = findFreqTerms(termmatrix, lowfreq=minfreq)
  
  termmatrix = DocumentTermMatrix(corpus2_stem,
                                  control=list(
                                    tokenize=BigramTokenizer,
                                    wordLengths = c(1, Inf),
                                    dictionary=word_min ))
  
  termmatrix2 <- sparseMatrix(termmatrix$i, termmatrix$j, x = termmatrix$v, dim=dim(termmatrix), dimnames=dimnames(termmatrix))
  
  freq <- sort(colSums(termmatrix2), decreasing=TRUE)
  
  return(freq)
  
}



