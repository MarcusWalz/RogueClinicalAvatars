
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
source("process_avatar.R")

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

out = process_avatars(my_av, my_sim, threads=1)
save( out , file=paste("sim_output", job_num, "Rdata", sep = "."))

###### Note: max_time arguement is defined based on the adopted PK/PD model. So, since Hamberg PK/PD
###### model is based on a 24-hr time period, we decided to make argument "max_time" constant "24".
###### So, the regular user of the simulator does not need to change it when calling function "processAvatar".
