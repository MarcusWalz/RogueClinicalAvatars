
process_avatar = function(simulation_in) {
  # Conducts a simulation on an avatar. 
  # 
  # Args:
  #   simulation_in: A named list with:
  #     avatar:     named list with avatar data
  #     simulation: named list with simulation paramatars. 
  # Returns:
  #  A named list list with:
  #     avatar:     same as input
  #     simulation: same as input
  #     sim_out:    array of data frames (length equal to number of replicates)
  #      with columns:
  #       Check: non-zero if an INR check occured on this day
  #       INR:   predicted INR
  #       Dose:  dose given to patient on day
  
  # print(simulation_in)
  attach(simulation_in, warn.conflicts = F)
  print(simulation)


  # find the initial dose
  avatar$InitialDose = initial_dose( as.data.frame(avatar)
                                   , simulation$initial_dose)$InitialDose
  # run sim for each replicate
  sim_out = lapply(1:simulation$replicates, function(rep) {
    set.seed(simulation$seed[rep])
    inr       = array(NA, simulation$days)
    inr_check = array(0, simulation$days)
    dose      = array(NA, simulation$days)
    # keep track of the dose superpositioning for each avatar
    # Cs_super_out = array(0, dim=c(simulation$max_time*simulation$days+1
    #                    , num_replicates)) 
    dose[1] = avatar$InitialDose
    Cs_rows = simulation$max_time * simulation$days+1
    Cs = matrix(0, nrow = Cs_rows, ncol=simulation$days)
    Cs_super = 0

    inr_errors = rnorm(simulation$days, 0, 0.1)

    for(day in 1:simulation$days) {
      pill = dose_cat(dose[day], simulation$max_dose)
      test_patient = hamberg_2007( pill
                                 , Cs_super
                                 , avatar$AGE
                                 , as.character(avatar$CYP2C9)
                                 , as.character(avatar$VKORC1G)
                                 , 1
                                 , simulation$max_time * simulation$days
                                 , avatar$seed )

      inr_error = inr_errors[day] # rnorm(1,0,0.1)
      measured_inr = round(test_patient$INRv[day*24+1] + inr_error, digits=1)

      if(is.na(measured_inr)) {
        cat(c("aww snap"))
        print(test_patient$paramaters)
        dose[day] = NA
        inr[day]  = NA
      } else {
        Cs[((day-1)*24 + 1):Cs_rows, day] = test_patient$Cs[1:length(((day-1)*24 + 1 ):Cs_rows)]
        Cs_super = apply(Cs, 1, sum)[day*24+2] 
        inr[day] = measured_inr

        if (is.na(dose[min(day+1, simulation$days)])) {
            inr_check[day] = day  # checked INR on this day (due to looping we are off by one day when we check inr so 2,4,7 days are clinically 3,5,8 but don't worry it all works out)
            dose = get_protocol(avatar, simulation)(inr, dose, day, inr_check)
            print(dose)
        }
      }
    }
  list("INR" = inr, "Dose" = dose, "Check" = inr_check)
})

list( avatar     = avatar
      , simulation = simulation
      , sim_out    = sim_out
      )
}

process_avatars = function(preprocessed_avatars) {
# Maps process_avatars over an Array of avatars.
  Map(preprocessed_avatars, process_avatar)
}
