
# validates simulation params prior to preprocess step.

validate_simulation_params = function(simulation) {
  # this function won't work outside the libarry
  if(!paste(simulation$protocol, "_protocol", sep="") %in% list_protocols()) {
    stop(paste("Unknown protocol", simulation$protocol
              , ". Installed protocols:", list_protocols()))
  }
  
  #TODO clean up initial_dose.R and make a test like above for
  # initial dose

  if(!is.numeric(simulation$days)) {
    stop(paste("simulation$days must be an integer"))
  }

  if(!is.numeric(simulation$max_dose)) {
    stop(paste("simulation$max_dose must be an integer"))
  }

}
