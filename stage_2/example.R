# Include the warfkit library.
library(warfkit)

# Read in the avatars. This file contains 1000 avatars.
my_avatars = read.delim("sample_avatars.txt")
# Let's use the first 25 avatars:
my_avatars = my_avatars[1,]

# Specify the simulation paramaters
my_sim =
 list ( days = 100
      , max_dose = 100
      , max_time = 24
      , replicates = 100
      , protocol = "fixed_dose"
      , initial_dose  = "pginitial_IWPC"
      )

# Preprocess avatars. This function outputs an array where each element
# contains all the data necessary to run the simulation on a single
# avatar.

avs = preprocess_avatars(my_avatars, my_sim, replicates=3)

# Map over the output of preprocess_avatars. Since the function 
# process_avatar's is  completely "stateless" (i.e. each individual
# simulation is independent of each other) this is safe.

simulation_out = Map(process_avatar, avs)

# Save the output as an .Rdata file
save(simulation_out, file = "sim_out.Rdata")
