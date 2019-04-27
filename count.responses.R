#--- A function to get the frequencies of a list response -->
count.responses<-function(x) {
  vals<-unlist(strsplit(as.character(x), ",\\s+"))
  length(vals)
}