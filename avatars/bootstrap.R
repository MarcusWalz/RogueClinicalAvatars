args <- commandArgs(trailingOnly = TRUE)

table_name = args[1]

output_table_name = paste(table_name, ".bootstrapped", sep="")

table = read.delim(table_name, header=T)

n = nrow(table)

out = table[sample(1:nrow(table), n, replace = TRUE),]

write.table(out, output_table_name, sep="\t") 
