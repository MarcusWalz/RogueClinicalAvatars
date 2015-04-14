library(digest)

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
    # simulations = rep(list(simulations), nrow(avatars))
  }
  
#  if(length(simulations) != nrow(avatars)) {
#    stop("simulation param of incorrect length")
#  }

  # assign random seeds to simulations



  library(plyr)
  # bind simulation params to avatar
  print("preprocessing")
  llply(1:nrow(avatars), function(n) {
    simulation = simulations
    simulation$seed = sample(1:10000, simulation$replicates, replace=F)

    av = as.list(avatars[n,])
    # hash together an id.
    # av$ID = digest(Map(as.character, av))
    list( avatar     = av
        , simulation = simulation )
  }, .progress="text")
}

# split avatars into sets of n equal sized chunks s.t. they can be recombined
# later, s.t.: 
# 
#   split_avatars(simulation_in, "prefix")
#   simulation_in == combine_avatars, "prefix")
#
# These functions work on simulation output too.
split_avatars=function(simulation_in, prefix, chunk_size=500) {
  library(plyr)
  # split into chunks
  x = split(simulation_in, ceiling(seq_along(simulation_in)/chunk_size))

  # save chunks
  print("splitting")
  l_ply(1:length(x), function(i) {
    saveRDS(x[[i]], file=paste(prefix, i-1, "avatars", "RData", sep="."))
  }, .progress= "text")
}

# combine split avatars based on a file prefix. 
combine_avatars = function(prefix) {

  # get the files in sequential order, outerwise "pre.10.avatars.RData" would
  # appears before "pre.1.avatars.RData".
  nums = sort(
    as.numeric(
      gsub("[^0-9]*", "",
        list.files(
          pattern=paste(prefix, "[0-9][0-9]*", "avatars", "RData", sep=".")
        )
      )
    )
  )

  files = sapply(nums, function(num) {paste(prefix,num, "avatars", "RData", sep=".")} )


  Reduce(append, Map(readRDS, files))
}
