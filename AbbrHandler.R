# abbreviation handling
#abbrtbl<- read.xlsx('jobtitles_abbreviation.xls', sheetIndex = 1, stringsAsFactors=FALSE, header = FALSE) %>%
#  rename(Abbr=X1, Full=X2)
abbrtbl<- readRDS('AbbreviationTable.rds')

# dealing with normal abbreviations
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
  }, normabbrs, gregexpr('[A-Z]([a-z]+)?', normabbrs), SIMPLIFY = FALSE)
# combining
generate.normabbr.morphologies<- function(normabbrs, 
                                          abbrstylefcns=c(abbrstyle1, abbrstyle2, abbrstyle3, abbrstyle4)) {
  tokenss<- abbr.tokenize(normabbrs)
  mapply(function(tokens) unique(mapply(function(fcn) fcn(tokens), abbrstylefcns)), tokenss, SIMPLIFY = FALSE)
}

# abbreviations with special characters (needed to be run one by one, batch running may cause errors)
# very dirty function
generate.unnormalized.abbr.variants<- function(abbr) {
  if (grepl('\\-', abbr)) {
    abbr
  } else if (grepl('\\&|\\/', abbr)) {
    sepposdata<- gregexpr('\\&|\\/', abbr)[[1]]
    parts<- strsplit(abbr, '\\&|\\/')[[1]]
    parts.variants<- generate.normabbr.morphologies(parts, 
                                                    abbrstylefcns = c(abbrstyle3, abbrstyle4,
                                                                      function(tokens) paste(tokens, collapse='')))
    numtokens<- length(parts.variants)
    for (i in 1:length(sepposdata)) {
      parts.variants[[length(parts.variants)+1]]<- substr(abbr, sepposdata[i], sepposdata[i]+attr(sepposdata, 'match.length')[i]-1)
    }
    parts.variants<- parts.variants[Reduce(function(l1, l2) append(l1, l2), mapply(function(i) c(i, i+numtokens), 1:numtokens))[1:(2*numtokens-1)]]
    possible.combinations<- expand.grid(parts.variants, stringsAsFactors = FALSE)
    variants<- append(mapply(function(i) paste(possible.combinations[i,], collapse=' '), 1:nrow(possible.combinations)),
                      mapply(function(i) paste(possible.combinations[i,], collapse=''), 1:nrow(possible.combinations)))
    variants
  }
}

# More general morphologies generator
generate.abbr.morphologies<- function(abbrs,
                                      abbrstylefcns=c(abbrstyle1, abbrstyle2, abbrstyle3, abbrstyle4)) {
  normalizeds<- !grepl('[^A-Za-z]', abbrs)
  variants<- generate.normabbr.morphologies(abbrs, abbrstylefcns = abbrstylefcns)
  variants[!normalizeds]<- mapply(generate.unnormalized.abbr.variants, abbrs[!normalizeds])
  variants
}

