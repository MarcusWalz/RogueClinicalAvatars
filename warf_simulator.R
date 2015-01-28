require(deSolve)
source("hamberg_2007.R")
source("protocols.R")
source("initial_dosing.R")


processAvatars = function(avatars, protocol,
  initial_dose, num_days_to_simulate, max_dose, num_replicates, max_time, initial_seed) {

  # set init dose
  avatars<-initial_dose(avatars,initial_dose)

  num_avatars = nrow(avatars)
  # set inital seed
  set.seed(initial_seed);
  rseed = replicate(num_replicates, rnorm(nrow(avatars)))

#  inr_error_dist = array(
#      rnorm(num_days_to_simulate*num_avatars*num_replicates,0,0.1)
#    , dim=c(num_days_to_simulate, num_replicates, num_avatars)
#  )  


  apply(rseed, 2, function(seed_vect) {
    avatars[, "seed"] <- seed_vect
    cnames = colnames(avatars)
    apply(avatars, 1, function(avatar) {
      set.seed( avatar["seed"] )
      
      random_nums= 
        round(abs(rnorm(num_days_to_simulate)*100*num_days_to_simulate)) 

      inr          = array(NA, num_days_to_simulate)
      inr_check    = array(0, num_days_to_simulate)
      dose         = array(NA, num_days_to_simulate)
      # keep track of the dose superpositioning for each avatar
      # Cs_super_out = array(0, dim=c(max_time*num_days_to_simulate+1
      #                    , num_replicates)) 

      dose[1] = as.numeric(avatar["InitialDose"])
      Cs_rows = max_time * num_days_to_simulate+1
      Cs = matrix(0, nrow = Cs_rows, ncol=num_days_to_simulate)
      Cs_super = 0

      for(day in 1:num_days_to_simulate) {
        print(day)
        pill = dose_cat(dose[day], max_dose)
        test_patient = hamberg_2007( pill
                                   , Cs_super
                                   , as.numeric(avatar["AGE"])
                                   , as.character(avatar["CYP2C9"])
                                   , as.character(avatar["VKORC1G"])
                                   , 1
                                   , max_time * num_days_to_simulate
                                   , avatar["seed"]
                                   )
        inr_error = rnorm(1,0,0.1)
        measured_inr = round(test_patient$INRv[day*24+1] + inr_error, digits=1)

        if(is.na(measured_inr)) {
          cat(c("aww snap"))
          print(test_patient$paramaters)
          dose[day] = NA
          inr[day]  = NA
        } else {
          Cs[((day-1)*24 + 1):Cs_rows, day] =
              test_patient$Cs[1:length(((day-1)*24 + 1 ):Cs_rows)]
          Cs_super = apply(Cs, 1, sum)[day*24+2]

          inr[day] = measured_inr

          if(is.na(dose[min(day+1, num_days_to_simulate)])) {
            inr_check[day] = day
            dose = protocol(measured_inr, dose, day, max_dose)
          }
        }
      }

      list("INR" = inr, "Dose" = dose, "Check" = inr_check)
    })
  })
}


avatars <- read.table("data_semifinal.txt", sep=" ", header=T)

processAvatars(avatars[1:2,], coumagen_pharm, "pginitial_couma1", 50, 15, 2, 24, 4321)
