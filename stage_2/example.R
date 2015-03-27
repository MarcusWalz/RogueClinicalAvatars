library(warfkit)
my_avatars = read.delim("sample_avatars.txt")

my_sim =
 list ( days = 90
      , max_dose = 100
      , max_time = 24
      , protocol = "ahc_clinical"
      , initial_dose  = "pginitial_IWPC"
      )

avs = preprocess_avatars(my_avatars, my_sim)

simulation_out = Map(process_avatar, avs)

save(simulation_out, file = "sim_out.Rdata")
