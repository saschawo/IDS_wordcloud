# IDS-Jahrestagung 2019

library(tm)
#devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)

files <- dir("C:\\Users\\Sascha\\Documents\\Abstracts JT19",
             full.names = T, pattern = "txt$")

file.list <- lapply(1:length(files), FUN = function (x) {
  cont <- scan(files[x], what = "char")
  data.frame(text = x, word = cont)
})

data <- do.call("rbind", file.list)

data$word <- as.character(data$word)
data$low.word <- tolower(data$word)
data$cl.low.word <- removePunctuation(data$low.word, preserve_intra_word_dashes = T)
data$cl.low.word <- gsub("[‚‘“”„]", "", data$cl.low.word)
data$cl.word <- removePunctuation(data$word, preserve_intra_word_dashes = T)

cont.low.words <- data$cl.low.word
cont.low.words <- cont.low.words[!(cont.low.words %in% c(stopwords("de"),
                                                         "dass", "–", "",
                                                         "deren", "dabei",
                                                         "sowie", "14-74", "zb",
                                                         "ua", "vgl", "et",
                                                         "va", "ii", "dar"))]

cont.low.words <- cont.low.words[!grepl("^[[:digit:]]+$", cont.low.words)]
cont.low.words <- cont.low.words[!grepl("^[[:space:]]+$", cont.low.words)]
cont.low.words <- cont.low.words[nchar(cont.low.words) > 2]

freq.df <- as.data.frame(table(cont.low.words))
names(freq.df) <- c("word", "freq")

# manually exclude words/names
freq.df <- freq.df[!(freq.df$word %in% c("androutsopoulos", "al", "iv", "lotze", "warnke")),]

freq.df2 <- freq.df[freq.df$freq > 1,]

letterCloud(freq.df2, word = "IDS", wordSize = 1)
