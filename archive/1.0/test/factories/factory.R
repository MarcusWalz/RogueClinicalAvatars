source("../../protocols.R")
source("avatar.R")
source("simulation.R")
source("inr.R")
source("dose.R")
source("check.R")


run_simulation = 
  function ( avatar = avatars_random(n=1)
           , simulation = simulation_random(n=1)
           , inr   = inr_bad
           , dose  = dose_constant(10)
           , check = check_every_n_days(7)
           ) 
{
  list( "avatar"      = avatar
      , "sim"         = simulation
      , "sim_result"  =
        as.data.frame(
          list ( "INR"    = inr   (avatar, simulation)
               , "Dose"   = dose  (avatar, simulation)
               , "Check"  = check (avatar, simulation)
               )
        ) 
      )
}

run_simulation()
