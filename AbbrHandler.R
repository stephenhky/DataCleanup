# abbreviation handling
abbrtbl<- read.xlsx('jobtitles_abbreviation.xls', sheetIndex = 1, stringsAsFactors=FALSE, header = FALSE) %>%
  rename(Abbr=X1, Full=X2)
# types of abbreviation:
# - RN (normalized)
# - R. N
abbrstyle1<- function(tokens) paste(tokens, collapse = '. ')
# - R.N
abbrstyle2<- function(tokens) paste(tokens, collapse = '.')
# - R. N.
abbrstyle3<- function(tokens) paste(abbrstyle1(tokens), '.', sep='')
# - R.N.
abbrstyle4<- function(tokens) paste(abbrstyle2(tokens), '.', sep='')
# Abbr tokenizer
abbr.tokenize<- function(normabbrs)
  mapply(function(normabbr, posdatum) {
    mapply(function(startpos, len) substr(normabbr, startpos, startpos+len-1), posdatum, attr(posdatum, 'match.length'))
  }, normabbrs, gregexpr('[A-Z]([a-z]+)?', normabbrs))
# combining
generate.abbr.morphologies<- function(normabbrs, 
                                      abbrstylefcns=c(abbrstyle1, abbrstyle2, abbrstyle3, abbrstyle4)) {
  tokenss<- abbr.tokenize(normabbrs)
  mapply(function(fcn) mapply(fcn, tokenss), abbrstylefcns)
}