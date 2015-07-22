#!/usr/bin/env Rscript

args = commandArgs(T)

input = args[1]
output = args[2]

this_split = as.numeric(args[3])
num_splits = as.numeric(args[4])

all_avatars = readRDS(input)
num_avatars = length(all_avatars)

per_split = num_avatars / num_splits

split_start = (this_split-1)*per_split +1 # R starts at 1
split_end = split_start + per_split - 1

# case 100 avatars 10 splits
# per_split = 10
# split_start = 1
# split_end = 11 - 1 = 10

saveRDS(all_avatars[split_start:split_end], file=output)
