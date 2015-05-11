# Include the warfkit library.
library(warfkit)

my_avatars = read.delim("iwpc.tsv.2")
my_avatars$DIABETES = F
my_avatars$SMOKER = F

# Specify the simulation paramaters
my_sim =
 list ( days = 180
      , max_dose = 100
      , max_time = 24
      , replicates = 1
      , protocol = "ahc_clinical"
      , initial_dose  = "AHC"
      )

# Preprocess avatars. This function outputs an array where each element
# contains all the data necessary to run the simulation on a single
# avatar.

a_a_avatars = my_avatars[my_avatars$VKORC1G == "A/A",]
print(nrow(a_a_avatars))
a_a_avatars = a_a_avatars[sample(1:nrow(a_a_avatars), 100),]
a_a_avatars = preprocess_avatars(a_a_avatars, my_sim)
saveRDS(a_a_avatars, file="a_a.RDS")
a_g_avatars = my_avatars[my_avatars$VKORC1G == "A/G",]
print(nrow(a_g_avatars))
a_g_avatars = a_g_avatars[sample(1:nrow(a_g_avatars), 100),]
a_g_avatars = preprocess_avatars(a_g_avatars, my_sim)
saveRDS(a_g_avatars, file="a_g.RDS")
g_g_avatars = my_avatars[my_avatars$VKORC1G == "G/G",]
print(nrow(g_g_avatars))
g_g_avatars = g_g_avatars[sample(1:nrow(g_g_avatars), 100),]
g_g_avatars = preprocess_avatars(g_g_avatars, my_sim)
saveRDS(g_g_avatars, file="g_g.RDS")


# Map over the output of preprocess_avatars. Since the function 
# process_avatar's is  completely "stateless" (i.e. each individual
# simulation is independent of each other) this is safe.

# simulation_out = Map(process_avatar, avs)


# EC_50_offset = 5
#simulation_out = Map(process_avatar, avs)
# saveRDS(simulation_out, file="offset_-2.RDS")

# Save the output as an .Rdata file
# save(simulation_out, file = "sim_out.Rdata")
