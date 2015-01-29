ttr_constant = function(ttr = 3.5) {
  function(avatar, simulation) { 
    array(ttr, simulation$days)
  }
}

ttr_linear = function(ttr_start = 5, ttr_end = 3.5) {
  function(avatar, simulation) {
    slope = (ttr_end - ttr_start) / simulation$days
    sapply(1:simulation$days, function(day) day * slope + ttr_start)
  }
}

ttr_spike = function(ttr_diff = 1.5, start = NA, length = NA) {
  function(avatar, simulation) {
    if(is.na(start)) {
      start = sample(1:simulation$days, 1)
    }
    if(is.na(length)) {
      length = sample(1:(simulation$days*0.5))
    }

    sapply(1:simulation$days, function(day) { 
      if(day >= start && day <= start+length) { ttr_diff } else { 0 }
    })
  }
}

ttr_noise = function(sd = 0.1) {
  function(avatar, simulation) {
    rnorm(simulation$days, 0, sd)
  }
}

ttr_smooth = function(ttr_functions) { 
  function(avatar, simulation) {
    smooth(ttr_add(ttr_functions)(avatar, simulation))
  }
}

ttr_add = function(ttr_functions) {
  function(avatar, simulation) {
    if(!is.vector(ttr_functions)) {
      ttr_functions <- c(ttr_functions)
    }
    rowSums(
      sapply(ttr_functions, function(ttr) ttr(avatar, simulation))
    )
  }
}

ttr_mean = function(ttr_functions) {
  function(avatar, simulation) { 
    ttr_add(ttr_functions)(avatar, simulation) / length(ttr_functions)
  }
}

test_simulation = list(days = 100) 
ttr_bad = ttr_smooth(ttr_add(c(ttr_linear(5,2), ttr_spike(), ttr_noise())))
