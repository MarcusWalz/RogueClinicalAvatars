source("../../protocols.R")
source("../../stable_dose.R")

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
  list( "avatar"  = avatar
      , "sim"     = simulation
      , "sim_out" =
        as.data.frame(
          list ( "INR"    = inr   (avatar, simulation)
               , "Dose"   = dose  (avatar, simulation)
               , "Check"  = check (avatar, simulation)
               )
        ) 
      )
}

x = run_simulation()
# combine_f(c(group_by_dose(), group_by_inr_stability(2,3)))(x$sim_out)

# this works
combine_functions2(combine_functions2(group_by_dose() , group_by_inr_stability(2,3)), group_by_dose())(x$sim_out)
# this doesn't



# exe( c( group_by_inr_stability(2,3)
#      , remove_unstable_inr(2,3)
#      , filter_by_days_elapsed(2)
#      )
#   , x$sim_out
#   )
