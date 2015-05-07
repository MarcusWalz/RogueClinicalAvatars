# Include the warfkit library.
library(warfkit)

my_avatars = read.delim("iwpc_train.valid.txt")
my_avatars = my_avatars[my_avatars$VKORC1G != "Unknown",]
my_avatars = my_avatars[my_avatars$CYP2C9 != "Missing",]
my_avatars$DIABETES = F
my_avatars$CYP2C9 = as.factor(as.character(my_avatars$CYP2C9))
my_avatars = my_avatars[sample(nrow(my_avatars),100),]

# Specify the simulation paramaters
my_sim =
 list ( days = 90
      , max_dose = 100
      , max_time = 24
      , replicates = 1
      , protocol = "ahc_clinical"
      , initial_dose  = "AHC"
      )

# Preprocess avatars. This function outputs an array where each element
# contains all the data necessary to run the simulation on a single
# avatar.

avs = preprocess_avatars(my_avatars, my_sim, replicates=3)

# Map over the output of preprocess_avatars. Since the function 
# process_avatar's is  completely "stateless" (i.e. each individual
# simulation is independent of each other) this is safe.

# simulation_out = Map(process_avatar, avs)

saveRDS(avs, file="offset_tune.RDS")

# EC_50_offset = 5
#simulation_out = Map(process_avatar, avs)
# saveRDS(simulation_out, file="offset_-2.RDS")

# Save the output as an .Rdata file
# save(simulation_out, file = "sim_out.Rdata")
