
# validates simulation params prior to preprocess step.

validate_simulation_params = function(simulation) {
  # this function won't work outside the libarry
  if(!simulation$protocol %in% list_protocols()) {
    exit(paste("Unknown protocol", simulation$protocol
              , ". Installed protocols:", list_protocols()))
  }

  if(!is.numeric(simulation$days)) {
    exit(paste("simulation$days must be an integer"))
  }

}
