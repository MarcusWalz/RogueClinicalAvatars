# send table in via stdin
# arg1 output file prefix
# arg2 number of tables to make
# arg3 number of random samples to take from table

args <- commandArgs(trailingOnly = TRUE)

prefix = args[1]
times = as.numeric(args[2])
n = as.numeric(args[3])

table = read.delim(file('stdin'), header=T)

for (i in 1:times) {

  output_table_name         = paste(prefix, ".", i, ".keep",    sep="")
  output_table_name_discard = paste(prefix, ".", i, ".discard", sep="")

  samples = sample(1:nrow(table), n, replace = FALSE)

  out = table[samples,]
  write.table(out, output_table_name, sep="\t") 

  out = table[samples*-1,]
  write.table(out, output_table_name_discard, sep="\t") 
}
