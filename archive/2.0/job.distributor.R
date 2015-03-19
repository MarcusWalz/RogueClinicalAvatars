distribute <- function(avatars, av_per, av_index, numReplicates){

av_per = av_per  # Number of avatars per file (per run)
block = av_per - 1

av_index = av_index

to = av_index*av_per
from = to - block

av_sub = avatars[from:to,]

#Create random seeds
# need to create a global set of random seeds so when the jobs are distributed the values are random and
# don't repeat with every block
set.seed(4321)
numReplicates = numReplicates
randomValues = array(round(abs(rnorm(nrow(avatars)*numReplicates)*nrow(avatars)*numReplicates)), dim=c(nrow(avatars), numReplicates))
rand_sub = randomValues[from:to,]

distribute.out <-list(av_sub,rand_sub)
return(distribute.out)
}