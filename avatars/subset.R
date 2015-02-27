args <- commandArgs(trailingOnly = TRUE)

table_name = args[1]
n = as.numeric(args[2])
times = as.numeric(args[3])

table = read.delim(table_name, header=T)

for (i in 1:times) {

  output_table_name         = paste(table_name, ".", i, ".keep",    sep="")
  output_table_name_discard = paste(table_name, ".", i, ".discard", sep="")


  samples = sample(1:nrow(table), n, replace = FALSE)

  out = table[samples,]
  write.table(out, output_table_name, sep="\t") 

  out = table[samples*-1,]
  write.table(out, output_table_name_discard, sep="\t") 
}
