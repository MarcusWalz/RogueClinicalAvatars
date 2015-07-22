#!/usr/bin/env Rscript

args = commandArgs(T)
output = args[1]
inputs = args[-1]

saveRDS(Reduce(append, Map(readRDS,inputs)), file=output)
