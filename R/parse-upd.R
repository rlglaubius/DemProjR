## Read in a upd file
## Returns a list with the following fields
## location: country name
## base.pop: base-year population by year, sex (1:male, 2:female) and age
## life.table: life table indicators by year, sex and age
## tfr: total fertility rate by year
## srb: sex ratio at birth by year
## pasfrs: proportional age-specific fertility rates by year and age
## migr: net number of migrants by year, sex and age
read_upd = function(filename) {
  parse.block = function(raw, tag) {
    tag.bgn = sprintf("<%s>", tag)
    tag.end = sprintf("</%s>", tag)
    i.bgn = which(raw[,1] == tag.bgn) + 1
    i.end = which(raw[,1] == tag.end) - 1
    if (length(i.bgn) < 0) {stop(sprintf("Tag %s not found", tag.bgn))}
    if (length(i.end) < 0) {stop(sprintf("Tag %s not found", tag.end))}
    if (length(i.bgn) > 1) {stop(sprintf("Malformed .upd: duplicate %s found", tag.bgn))}
    if (length(i.end) > 1) {stop(sprintf("Malformed .upd: duplicate %s found", tag.end))}
    cnames = raw[i.bgn,which(nchar(raw[i.bgn,]) > 0)]
    ncols = length(cnames)
    values = apply(raw[(i.bgn+1):i.end, 1:ncols], 2, as.numeric)
    colnames(values) = cnames
    return(as.data.frame(values))
  }
  
  raw = read.csv(filename, stringsAsFactors=FALSE, header=FALSE, sep=',')
  
  upd = list(
    location = gsub("Country=", "", raw[which(startsWith(raw[,1], "Country")),1]),
    base.pop = parse.block(raw, "basepop"),
    life.table = parse.block(raw, "lfts"),
    tfr = parse.block(raw, "tfr"),
    srb = parse.block(raw, "srb"),
    pasfrs = parse.block(raw, "pasfrs"),
    migr = parse.block(raw, "migration")
  )
  
  return(upd)
}