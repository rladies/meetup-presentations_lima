# Minería de textos científicos: ¿Cómo analizar publicaciones sin leerlas?


## Descripción
En esta charla se presentará un procedimiento para realizar text mining de artículos científicos en base a un ejemplo sobre un campo de la ciencia, la ecología del movimiento. Se hablará de la identificación de los artículos en la web, descarga de artículos y análisis de datos. Los participantes podrán también realizar una parte del análisis, identificación de temas (topic modeling) para un pequeño conjunto de datos, y realización de nubes de palabras.

## Paquetes a utilizar
Para poder replicar los análisis mostrados durante la charla, es necesario tener los siguentes paquetes instalados.

### Procesamiento de datos

```r
install.packages("tidyverse")
```

### Análisis de textos

```r
install.packages("SnowballC")
install.packages("tidytext")
install.packages("tm")
install.packages("qdap")
install.packages("topicmodels")

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/textstem")
```

### Nube de palabras

```r
library("wordcloud")
library(ggwordcloud)
```

### Paquetes adicionales
```r
install.packages("RColorBrewer")
install.packages("gridExtra")
install.packages("grid")
install.packages("parallel")

install.packages("knitr")
install.packages("kableExtra")
install.packages("xtable")

install.packages("cowplot")

install.packages("viridisLite")
install.packages("viridis")
```





