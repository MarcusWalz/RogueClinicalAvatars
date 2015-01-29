inr_constant = function(inr = 3.5) {
  function(avatar, simulation) { 
    array(inr, simulation$days)
  }
}

inr_linear = function(inr_start = 5, inr_end = 3.5) {
  function(avatar, simulation) {
    slope = (inr_end - inr_start) / simulation$days
    sapply(1:simulation$days, function(day) day * slope + inr_start)
  }
}

inr_spike = function(inr_diff = 1.5, start = NA, length = NA) {
  function(avatar, simulation) {
    if(is.na(start)) {
      start = sample(1:simulation$days, 1)
    }
    if(is.na(length)) {
      length = sample(1:(simulation$days*0.5))
    }

    sapply(1:simulation$days, function(day) { 
      if(day >= start && day <= start+length) { inr_diff } else { 0 }
    })
  }
}

inr_noise = function(sd = 0.1) {
  function(avatar, simulation) {
    rnorm(simulation$days, 0, sd)
  }
}

inr_smooth = function(inr_functions) { 
  function(avatar, simulation) {
    smooth(inr_add(inr_functions)(avatar, simulation))
  }
}

inr_add = function(inr_functions) {
  function(avatar, simulation) {
    if(!is.vector(inr_functions)) {
      inr_functions <- c(inr_functions)
    }
    rowSums(
      sapply(inr_functions, function(inr) inr(avatar, simulation))
    )
  }
}

inr_mean = function(inr_functions) {
  function(avatar, simulation) { 
    inr_add(inr_functions)(avatar, simulation) / length(inr_functions)
  }
}

test_simulation = list(days = 100) 
inr_bad = inr_smooth(c(inr_linear(5,2), inr_spike(), inr_noise()))
