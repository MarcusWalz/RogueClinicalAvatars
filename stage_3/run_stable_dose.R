library(warfkit)
job_num = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) 

input_file  = paste("out", job_num, "avatars", "RData", sep=".")
output_file = paste("sd", job_num, "avatars", "RData", sep=".")


avs = readRDS(input_file)

compute_stable_dose_iwpc = function(avs) {
  # Reduce Stable Dose s.t. there is only one replicate
  avs = Map(function(x) {
    x$sim_out = as.data.frame(x$sim_out[[1]]); x
  }, avs)

  # Map Stable Dose Defs for project site 5, 1, 2!

  sapply(1:22, function(ps) { 
         def = get_stable_def(ps)
         as.vector(Map(def, avs))
  })
}

# write the table
# write.table(compute_stable_dose_iwpc(avs), "sd", sep="\t")

saveRDS(compute_stable_dose_iwpc(avs), file=output_file)
