# loading libraries
library(dplyr)
library(textmineR)

# What to do?
# 1. removal of digit-only token
# 2. lower case
# 3. removal of Roman numerals
# 4. removal of non-alpha-numerics

# tokenization: stringi::stri_split_boundaries

# Optional:
# 1. Stemming
# 2. Lemmatization

# predefined knowledge
romannum<- c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')

vec<- c('R is useful.', 'totally', '33 soldiers', 'engineer II')

# routines:
tokenized.vec<- stringi::stri_split_boundaries(vec)
tokenized.vec<- mapply(function(s) gsub('[^a-zA-Z0-9]+', ' ', s), tokenized.vec)
tokenized.vec<- mapply(function(s) gsub('\\s+', ' ', s), tokenized.vec)
tokenized.vec<- mapply(trimws, tokenized.vec)
tokenized.vec<- mapply(function(strvec) strvec[!grepl('^\\d+$', strvec)], tokenized.vec)
tokenized.vec<- mapply(tolower, tokenized.vec)



