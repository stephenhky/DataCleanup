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

vec<- c('R is useful.', 'totally', '33 soldiers', 'engineer II')

# routines:
# tokenize
tokenized.vec<- stringi::stri_split_boundaries(vec)
# removenon-alpha-numerics
tokenized.vec<- mapply(function(s) gsub('[^a-zA-Z0-9]+', ' ', s), tokenized.vec)
# contract spaces
tokenized.vec<- mapply(function(s) gsub('\\s+', ' ', s), tokenized.vec)
# trim words
tokenized.vec<- mapply(trimws, tokenized.vec)
# remove digit-only tokens
tokenized.vec<- mapply(function(strvec) strvec[!grepl('^\\d+$', strvec)], tokenized.vec)
# lower case
tokenized.vec<- mapply(tolower, tokenized.vec)
# remove roman numerals
tokenized.vec<- mapply(function(strvec) strvec[ !(strvec %in% romannum)], tokenized.vec)
# spell check and correction
tokenized.vec<- mapply(function(strvec) mapply(function(s, wordexists) ifelse(wordexists, s, hunspell_suggest(s)[[1]][1]),
                                               strvec, hunspell_check(strvec)), 
                       tokenized.vec)
tokenized.vec<- mapply(unname, tokenized.vec)
# remove empty tokens
tokenized.vec<- mapply(function(strvec) strvec[ nchar(strvec)>0], tokenized.vec)

# routines (optional):


# concatenate
new.vec<- mapply(function(strvec) paste(strvec, collapse=' '), tokenized.vec)



