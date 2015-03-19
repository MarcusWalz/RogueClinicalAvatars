dose_constant = function(dose = 3.5) {
  function(avatar, simulation) { 
    array(dose, simulation$days)
  }
}

dose_linear = function(dose_start = 5, dose_end = 3.5) {
  function(avatar, simulation) {
    slope = (dose_end - dose_start) / simulation$days
    sapply(1:simulation$days, function(day) day * slope + dose_start)
  }
}

dose_spike = function(dose_diff = 1.5, start = NA, length = NA) {
  function(avatar, simulation) {
    if(is.na(start)) {
      start = sample(1:simulation$days, 1)
    }
    if(is.na(length)) {
      length = sample(1:(simulation$days*0.5))
    }

    sapply(1:simulation$days, function(day) { 
      if(day >= start && day <= start+length) { dose_diff } else { 0 }
    })
  }
}

dose_add = function(dose_functions) {
  function(avatar, simulation) {
    rowSums(
      sapply(dose_functions, function(dose) dose(avatar, simulation))
    )
  }
}
