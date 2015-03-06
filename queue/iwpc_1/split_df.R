source("settings.R")

table <- read.delim("avatars.txt")

number_subsets = ceiling (nrow(table) / sim_settings$per_job)

for(i in 1:number_subsets) {
  a= table[((i-1)*sim_settings$per_job):(i*sim_settings$per_job-1),]
  write.table(a, paste("parts/", i, ".txt", sep=""), sep="\t")
}
