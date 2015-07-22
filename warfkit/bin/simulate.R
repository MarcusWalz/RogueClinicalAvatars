#!/usr/bin/env Rscript
library(warfkit)

args = commandArgs(T)
input = args[1]
output = args[2]

saveRDS(Map(process_avatar, readRDS(input)) , file=output)
