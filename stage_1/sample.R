# send table in via stdin
# arg1 output file prefix

library('getopt')

spec = matrix(
  c('percent', 'p', 1, 'double'
   ,'rows',    'r', 1, 'integer'
   ,'inverse', 'i', 0, 'logical'
   ,'help',    'h', 0, 'logical'
  ), byrow = TRUE, ncol = 4)

opt = getopt(spec)

if ( !is.null(opt$help) ) {
  cat(getopt(spec, usage=TRUE));
  q(status=1);
}

# read in table
table = read.delim(file('stdin'), header=T)


if(is.null(opt$rows)) { 
  if(!is.null(opt$percent)) {
     opt$rows = nrow(table) * opt$percent
  } else {
    cat("Either percent or rows must be set")
    q(status=0)
  }
}

args <- commandArgs(trailingOnly = TRUE)

samples = sample(1:nrow(table), opt$rows, replace = FALSE)

out = table[samples,]
write.table(out, "", sep="\t") 

if ( !is.null(opt$inverse) ) {
  out = table[samples*-1,]
  write.table(out, stderr(), sep="\t") 
}

