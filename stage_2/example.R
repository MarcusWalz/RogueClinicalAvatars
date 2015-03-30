# Include the warfkit library.
library(warfkit)

# Read in the avatars. This file contains 1000 avatars.
my_avatars = read.delim("sample_avatars.txt")

# Let's use the first 25 avatars:
my_avatars= my_avatars[1:25,]

# Specify the simulation paramaters
my_sim =
 list ( days = 90
      , max_dose = 100
      , max_time = 24
      , protocol = "ahc_clinical"
      , initial_dose  = "pginitial_IWPC"
      )

# Preprocess avatars. This function outputs an array = number of avatars
# The individual elements of the array contain all the data we need
# to run the simulation on a singe avatar.

avs = preprocess_avatars(my_avatars, my_sim)

# Map the output of preprocess_avatars. Since the function  process_avatar's
# is  completely "stateless" e.g. unaffected by the order of avatars,
# this is safe. 

simulation_out = Map(process_avatar, avs)

# Save the output
save(simulation_out, file = "sim_out.Rdata")
