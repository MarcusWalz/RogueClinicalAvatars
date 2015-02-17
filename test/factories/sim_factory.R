
# legacy output style
sim_out_df = function(avatar, simulation) {
  function( inr   = inr_target()
          , dose  = dose_constant
          , check = check_every_n_days(3)
          ) {
        as.data.frame(
          list ( "INR"    = inr   (avatar, simulation)
               , "Dose"   = dose  (avatar, simulation)
               , "Check"  = check (avatar, simulation)
               )
          )
  }
}

run_simulation = 
  function ( avatar = avatars_random()
           , simulation = simulation_random()
           , inr   = inr_target()
           , dose  = dose_constant(10)
           , check = check_every_n_days(3)
           ) 
  {
  list( "avatar"  = avatar
      , "sim"     = simulation
      , "sim_out" = sim_out_df(avatar, simulation)(inr, dose, check)
      )
}

x = run_simulation()
# cfv(c("id()", "group_by_dose()", "group_by_inr_stability(2,3)", "group_by_dose()", "filter_unstable_inr(2,3)"))(x$sim_out)

# cfv(c(group_by_dose(), group_by_inr_stability(2,3), group_by_dose(), filter_unstable_inr(2,3)))(x$sim_out)
print(x)

for(i in 1:21) {
  cat(paste(i,get_stable_def(i)(x), "\n"))
}
# this works
#combine_functions2(combine_functions2(group_by_dose() , group_by_inr_stability(2,3)), group_by_dose())(x$sim_out)
# this doesn't



# exe( c( group_by_inr_stability(2,3)
#      , remove_unstable_inr(2,3)
#      , filter_by_days_elapsed(2)
#      )
#   , x$sim_out
#   )
