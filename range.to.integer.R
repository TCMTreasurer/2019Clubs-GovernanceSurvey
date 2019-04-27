#--- A function to convert string ranges to a random integer in the range ---#
range.to.integer<-function(x) {
  vals<-as.integer(unlist(strsplit(as.character(x), "\\s+to\\s+")))
  sample(vals[1]:vals[2], 1, replace=TRUE)
}