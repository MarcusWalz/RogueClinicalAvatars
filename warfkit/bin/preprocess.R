#!/usr/bin/env Rscript
library(warfkit)

args = commandArgs(T)
input = args[1]
output = args[2]
days = as.numeric(args[3])
avatars_in=read.delim(input)

sim =
 list ( days = days
      , max_dose = 100
      , max_time = 24
      , replicates = 100
      , protocol = "fixed_dose"
      , initial_dose  = "pginitial_IWPC"
      )

saveRDS(preprocess_avatars(avatars_in, sim) , file=output)
