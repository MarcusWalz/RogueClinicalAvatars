
#This function takes the following arguments and simulates a course of anticoagulation therapy.
#avatars: A file includes avatars with assigned initial warfarin dose.
#protocols: A desired dosing protocol for warfarin maintenance dosing.
#initialDose: A relevant code for the dosing algorithm used for initial dosing.
#numDaysToSimulate: A desired number of days for simulation.
#simulation$max_dose: The upper limit of a range of warfarin dose.
#numReplicates: Number of replication of a simulation.
#max_time: The time interval for getting INR and Dose calculations outputs.
#rseed: random seeds.

##################################
###    simulation function     ###
##################################

# avatars = df of avatars
# simulation = list of simulations (length equal to number of rows in avatar) 
#   or a single base simulation 

library(plyr)
library(doMC)
# registerDoMC(10)
source("hamberg_2007.R")
source("maintenance_protocols.R")
source("initial_dose.R")
source("job.distributor.R")

job_num = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

how_many_jobs = 30

my_av = read.table("iwpc_avatars_final.txt.head", sep="\t", header=T)

num_avatars = nrow(my_av)

avs_per_job = ceiling(num_avatars / how_many_jobs)

start = (job_num - 1)  * avs_per_job + 1
end   = (job_num    )  * avs_per_job

my_av = my_av[start:end,]


my_av = my_av[my_av$CYP2C9 != "Missing",]

my_sim = list ( days = 90
       , max_dose = 100
       , replicates = 1 
       , max_time = 24
       , protocol = "ahc_clinical"
       , initial_dose  = "pginitial_IWPC"
       )

process_avatars = function( avatars
                          , simulations
                          , build = T
                          , initial_seed = 4321
                          , replicates = 1
                          , threads = 1 
                          ) {

  if(build) {
    
    # set simulations$replicates if undefined
    if(!("replicates" %in% names(simulations)) || is.null(simulations$replicates)) {
      simulations$replicates = replicates
    }
    # pick up the random seed, otherwie fall back to default `initial_seed`
    if(!is.null(simulations$seed)) {
      initial_seed = simulations$seed
    }

    # delete random seed
    simulations[which(names(simulations) %in% c("seed"))] <- NULL

    # create a numbered list of simulations  
    simulations = rep(list(simulations), nrow(avatars))
  }
  
  if(length(simulations) != nrow(avatars)) {
    stop("simulation param of incorrect length")
  }

  # assign random seeds to simulations

  # set using the default seed
  set.seed(initial_seed)

  for(simulation in simulations) {

    # set the seeds if undefined 
    if("seed" %in% names(simulation) || is.null(simulation$seed)) {
      simulation$seed = sample(10^12, simulation$replicates, replace=T)
    }
  }

  # bind simulation params to avatar
  simulation_in = sapply(1:nrow(avatars), function(n) {
    list(list( avatar     = as.list(avatars[n,])
        , simulation = simulations[[n]]   
        ))
  })

  # in parallel conduct the simulation!
  llply(simulation_in, process_avatar, .parallel = threads > 1)
}



process_avatar = function(simulation_in) {
  # simulation_in contains
  # (1) avatar as a list
  # (2) simulation as a list
  
  # see if it works?
  system("hostname")
 # print(simulation_in)
  attach(simulation_in, warn.conflicts = F)


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
            if (simulation$protocol == "coumagen_pharm") {
                dose = coumagen_pharm(measured_inr, dose, day, simulation$max_dose)
            } else if (simulation$protocol == "coumagen_standard") {
                dose = coumagen_standard(measured_inr, dose, day, simulation$max_dose)
            } else if (simulation$protocol == "wilson") {
                dose = wilson(measured_inr, dose, day, simulation$max_dose)		                    
            } else if (simulation$protocol == "fixed_dose") {
                dose = fixed_dose(dose, day) 
            } else if (simulation$protocol == "wilson_coumagen_pharm") {
                dose = wilson_coumagen_pharm(measured_inr, dose, day, simulation$max_dose)
            } else if (simulation$protocol == "wilson_coumagen_standard") {
                dose = wilson_coumagen_standard(measured_inr, dose, day, simulation$max_dose)
            } else if (simulation$protocol == "cooper_intermt") {
                dose = cooper_intermt(measured_inr, dose, day, simulation$max_dose)
            } else if (simulation$protocol == "fen_intermt") {
              dose = fen_intermt(measured_inr, dose, day, simulation$max_dose)
            } else if (simulation$protocol == "rob_intermt") {
              dose = rob_intermt(measured_inr, dose, day, simulation$max_dose)
            } else if (simulation$protocol == "gedge_intermt") {
              dose = gedge_intermt(measured_inr, dose, day, simulation$max_dose)
            } else if (simulation$protocol == "ahc_clinical") {
            dose = ahc_clinical(as.numeric(as.character(measured_inr)), as.numeric(as.character(inr[1:day])), as.numeric(as.character(inr_check[1:day])), as.numeric(as.character(dose)), day, simulation$max_dose, as.character(avatar$TINR))
          } else if (simulation$protocol == "eu_pact_intermt") {#initial and alteration: eu_pact || maintenance: intermountain
            dose = eu_pact_intermt(measured_inr, dose, day, simulation$max_dose, avatars$AGE, as.character(avatar$VKORC1G), avatar$BSA, avatar$TINR, as.character(avatar$AMI), as.character(avatar$CYP2C9), avatar$HEIGHT, avatar$WEIGHT)
         } else if (simulation$protocol == "eu_pact_ahc") {#initial and alteration: eu_pact || maintenance: ahc
            dose = eu_pact_ahc(measured_inr, dose, day, simulation$max_dose, avatar$AGE, as.character(avatar$VKORC1G), avatar$BSA, avatar$TINR, as.character(avatar$AMI), as.character(avatar$CYP2C9), avatar$HEIGHT, avatar$WEIGHT, as.numeric(as.character(inr[1:day])), as.numeric(as.character(inr_check[1:day])))
         } else {
                stop("Hey buddy, you forgot to add the protocol to the simulator - duh...")
        }
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

out = process_avatars(my_av, my_sim, threads=1)
save( out , file=paste("sim_output", job_num, "Rdata", sep = "."))

###### Note: max_time arguement is defined based on the adopted PK/PD model. So, since Hamberg PK/PD
###### model is based on a 24-hr time period, we decided to make argument "max_time" constant "24".
###### So, the regular user of the simulator does not need to change it when calling function "processAvatar".
