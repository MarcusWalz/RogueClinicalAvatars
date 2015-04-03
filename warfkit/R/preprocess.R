library(plyr)

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

# source("hamberg_2007.R")
# source("maintenance_protocols.R")
# source("initial_dose.R")
# source("process_avatar.R")

# my_av = read.table("sample_avatars.txt", sep="\t", header=T)[1:100,]

# hamburg needs a CYP2C9, let's filter these avatars out for now
# my_av = my_av[my_av$CYP2C9 != "Missing",]



# my_sim =
#  list ( days = 90
#       , max_dose = 100
#       , max_time = 24
#       , protocol = "ahc_clinical"
#       , initial_dose  = "pginitial_IWPC"
#       )

preprocess_avatars = function( avatars
                          , simulations
                          , build = T
                          , initial_seed = 4321
                          , replicates = 1
                          ) {

  # Will exit program with a nice error message if avatars or sim params are invalid.
  validate_avatars(avatars)
  validate_simulation_params(simulations)

  if(build) {
  # build is always true for now. However, in the future we
  # may want to assign non-homogenious paramatars to avatars.
    
    # set simulations$replicates if undefined
    if( is.null(simulations$replicates) ) {
      simulations$replicates = replicates
    }
    # pick up the random seed, otherwie fall back to default `initial_seed`
    if(!is.null(simulations$seed)) {
      initial_seed = simulations$seed
    }
    # set using the default seed
    set.seed(initial_seed)

    # delete random seed
    simulations[which(names(simulations) %in% c("seed"))] <- NULL

    # create a numbered list of simulations  
    simulations = rep(list(simulations), nrow(avatars))
  }
  
  if(length(simulations) != nrow(avatars)) {
    stop("simulation param of incorrect length")
  }

  # assign random seeds to simulations



  # bind simulation params to avatar
  simulation_in = sapply(1:nrow(avatars), function(n) {
    simulations[[n]]$seed = sample(1:10000, simulations[[n]]$replicates, replace=T)
    list(list( avatar     = as.list(avatars[n,])
        , simulation = simulations[[n]]   
        ))
  })

  simulation_in
}

# split avatars into x equal sized chunks
split_avatars=function(simulation_in, prefix, chunk_size=500) {
  # split into chunks
  x = split(simulation_in, ceiling(seq_along(simulation_in)/chunk_size))

  # save chunks
  l_ply(1:length(x), function(i) {
    avatars = x[[i]]
    save(avatars, file=paste(prefix, i, "avatars", "Rdata", sep="."))
  }, .progress= "text")
}

# combine split avatars based on a file prefix
combine_avatars = function(prefix) {
  files = list.files(pattern=paste(
    prefix,"[1-9][0-9]*", "avatars","Rdata", sep=".")
  )
  all_avs = list()
  for(file in files) {
    load(file)
    all_avs = append(all_avs, avatars)
  }

  all_avs
}

# split_avatars(preprocess_avatars(my_av, my_sim), prefix="test", chunk_size=10)
