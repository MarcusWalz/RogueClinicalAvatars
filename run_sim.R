#!/usr/bin/Rscript

# library('getopt')

args <- commandArgs(trailingOnly = TRUE)

input_file  = paste(getwd(), args[2], sep='/') 
output_file = paste(getwd(), args[3], sep='/') 

source(args[1])

opts = sim_settings


setwd("~/ClinicalAvatars") 

avatars = as.data.frame(read.delim(input_file))

# spec = matrix(c(
# 'per'     , 'p', 1 , "integer"   ,
#	'block'   , 'b', 1 , "integer"   ,
#	'avatars' , 'a', 1 , "character" ,
#	'initial' , 'i', 1 , "character" ,
#	'protocol', 'n', 1 , "character"
#	), byrow=TRUE, ncol=4)

#opts = getopt(spec)

source("simulator.R")

set.seed(sim_settings$seed)

#av_per=opts$per
#block=av_per-1
#av_index<-opts$block
#to<-av_index*av_per
#from<-to-block
#av_sub<-avatars[from:to,]
numReplicates = sim_settings$numReplicates
randomValues = array(round(abs(rnorm(nrow(avatars)*numReplicates)*nrow(avatars)
                                  *numReplicates)), dim=c(nrow(avatars), numReplicates))
# rand_sub = randomValues[from:to,]
av_out=processAvatar(avatars=avatars, protocol=opts$protocol, initialDose=opts$initial, 
                     numDaysToSimulate=90, maxDose=15, numReplicates=1,
                     maxTime=24, rseed=randomValues)

write.table(av_out, output_file, sep="\t")
