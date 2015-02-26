# ARG1 is output file
# ARG2-ARGN are the tables to merge.
# Easy to use on the command line.

args <- as.vector(commandArgs(trailingOnly = TRUE))

write.table(
  Reduce(function(x,y)
    { rbind(x, read.delim(y , header = T)) }
    , args[2:length(args)]
    , init=data.frame())
  , args[1]
  , sep = "\t"
  )
