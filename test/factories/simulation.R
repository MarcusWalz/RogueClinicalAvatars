# Simulation in singular now
simulation_random = function ( seed = sample(1:1000, 1)
                             , protocol = sample(protocols)
                             , initial_dose = "test" 
                             , max_dose = 10
                             , days = 100 
                             ) {
  as.data.frame(
    list( seed         = seed 
        , protocol     = sample(protocol, 1, replace=TRUE)
        , initial_dose = "test"
        , max_dose     = 10
        , days         = 100
        )
  )
}
