#--- A function to get the frequencies of a list response --#
list.frequencies<-function(x) {
  vals<-toString(x)
  list<-(unlist(strsplit(vals, ",\\s+")))
  sort(table(list), decreasing=TRUE)
}
