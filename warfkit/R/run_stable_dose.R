library(warfkit)
source("stable_dose.R")

avs = readRDS("first22k.RData")[1:5000]

compute_stable_dose_iwpc = function(avs) {
  # Reduce Stable Dose s.t. there is only one replicate
  avs = Map(function(x) {
    x$sim_out = as.data.frame(x$sim_out[[1]]); x
  }, avs)

  # Map Stable Dose Defs for project site 5, 1, 2!

  sapply(1:21, function(ps) { 
         def = get_stable_def(ps)
         as.vector(Map(def, avs))
  })
}

# write the table
# write.table(compute_stable_dose_iwpc(avs), "sd", sep="\t")

saveRDS(compute_stable_dose_iwpc(avs), file="sd.RDS")
