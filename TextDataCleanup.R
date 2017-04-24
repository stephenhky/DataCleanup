# loading libraries
library(dplyr)
library(textmineR)
library(hunspell)

# What to do?
# 1. removal of digit-only token
# 2. lower case
# 3. removal of Roman numerals
# 4. removal of non-alpha-numerics
# 5. Spell correction

# tokenization: stringi::stri_split_boundaries

# Optional:
# 1. Lemmatization

# predefined knowledge
romannum<- c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')

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

steps.20170421<- c(function(s) gsub('[^a-zA-Z0-9]+', ' ', s),
                   function(s) gsub('\\s+', ' ', s),
                   trimws,
                   function(strvec) strvec[!grepl('^\\d+$', strvec)],
                   tolower,
                   function(strvec) strvec[ !(strvec %in% romannum)],
                   function(strvec) mapply(function(s, wordexists) ifelse(wordexists, s, hunspell_suggest(s)[[1]][1]),
                                           strvec, hunspell_check(strvec)),
                   unname,
                   function(strvec) strvec[ nchar(strvec)>0])

cleanup.text.20170421<-function(vec) cleanup.text(vec, steps.20170421)


