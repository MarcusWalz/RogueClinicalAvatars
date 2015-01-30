# Get an INR check every day
check_everday = function() {
  function(avatar, simulation) { 
    1:simulation$days
  }
}

# Go for a check up on this day
check_on = function(day = NA) {
  function(avatar, simulation) { 
    if(is.na(day)) {
      day = sample(1:simulation$days, 1)
    }
    array(0,simulation$days)[day] = day
  }
}

# Go for a check evey n days
check_every_n_days = function(n = 7) {
  function(avatar, simulation) {
    sapply(1:simulation$days, function(n){ if(n %% 7 == 0) { n } else { 0 }})
  }
}

check_merge = function(dose_functions) {
  function(avatar, simulation) {
    rowMax(
      sapply(dose_functions, function(dose) dose(avatar, simulation))
    )
  }
}
