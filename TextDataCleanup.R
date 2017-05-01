# loading libraries
library(dplyr)
library(textmineR)
library(hunspell)
library(xlsx)

# What to do?

# removal of non-alpha-numeric characters
remove.nonalphanumerics<- function(tokens) gsub('[^a-zA-Z0-9]+', ' ', tokens)

# removal of digit-only token
remove.digitonly.tokens<- function(strvec) strvec[!grepl('^\\d+$', strvec)]

# removal of consecutive spaces
remove.consecutive.spaces<- function(tokens) gsub('\\s+', ' ', tokens)

# removal of Roman numerals
romannum<- c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')
remove.roman.numerals<- function(strvec) strvec[ !(strvec %in% romannum)]

# spell corrections
correct.spellings<- function(strvec) unname(mapply(function(s, wordexists) ifelse(wordexists, s, hunspell_suggest(s)[[1]][1]),
                                                   strvec, hunspell_check(strvec)))

# removal of empty tokens
remove.empty.tokens<- function(strvec) strvec[ nchar(strvec)>0]

# abbreviation handling
abbrtbl<- read.xlsx('jobtitles_abbreviation.xls', sheetIndex = 1, stringsAsFactors=FALSE, header = FALSE) %>%
  rename(Abbr=X1, Full=X2)
# types of abbreviation:
# - RN
# - R. N.
# - R.N.
# - R. N
# - R.N


# reversing job title containing ('of the')
reverse.titles.with.of<- function(strvec) gsub('(\\w+) of (the )?(\\w+\\s)?(\\w+)$', '\\3 \\4 \\1', strvec) %>%
  remove.consecutive.spaces() %>% trimws()

# reversing job title containing (',')
reverse.titles.with.comma<- function(strvec) gsub('(\\w+\\s)?(\\w+)\\, (\\w+\\s)?(\\w+)', '\\3 \\4 \\1 \\2', strvec) %>%
  remove.consecutive.spaces() %>% trimws()

# Optional:
# 1. Lemmatization

cleanup.text<-function(vec, steps) {
  # routines:
  # tokenize
  tokenized.vec<- stringi::stri_split_boundaries(vec)
  
  # run the steps
  for (func in steps) {
    tokenized.vec<- mapply(func, tokenized.vec)
  }
  
  # concatenate
  new.vec<- mapply(function(strvec) paste(strvec, collapse=' '), tokenized.vec)
  
  # return
  new.vec
}

steps.20170421<- c(remove.nonalphanumerics,
                   remove.consecutive.spaces,
                   trimws,
                   remove.digitonly.tokens,
                   tolower,
                   remove.roman.numerals,
                   correct.spellings,
                   remove.empty.tokens)

cleanup.text.20170421<-function(vec) cleanup.text(vec, steps.20170421)

cleanup.text.nospecialchar<- function(vec) cleanup.text(vec, c(remove.nonalphanumerics,
                                                               remove.consecutive.spaces,
                                                               trimws))
