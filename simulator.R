
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
source("hamberg_2007.R")
source("maintenance_protocols.R")
source("initial_dose.R")
source("process_avatar.R")

my_av = read.table("sample_avatars.txt", sep="\t", header=T)[1:100,]
my_av$ENZYME <- "Y"

my_sim =
  list ( days = 90
       , max_dose = 100
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
  if(threads > 1) {
    library(doMC)
    registerDoMC(threads)
  }

  if(build) {
    
    # set simulations$replicates if undefined
    if("replicates" %in% names(simulations) || is.null(simulations$replicates)) {
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
process_avatars(my_av, my_sim)
