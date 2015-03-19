# ARG1-ARGN are the tables to merge.
# merged table sent to stdout

args <- as.vector(commandArgs(trailingOnly = TRUE))

write.table(
  Reduce(function(x,y)
    { rbind(x, read.delim(y , header = T)) }
    , args
    , init=data.frame())
  , ""
  , sep = "\t"
  )
