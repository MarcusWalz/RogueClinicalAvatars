
##################### AHC Protocol ########################
###########################################################

timi_protocol = function(avatar,simulation) {
  dasy = simulation$days
  function(INR, dose, day, check) {
    print(dose)
    print(day)
    # last non-zero non-na dose 
    last_dose = tail(dose[!(is.na(dose) | dose == 0)], n=1)
    zero_case = dose[day-1] == 0
    print(last_dose)

    INR = INR[day]

    if(zero_case) {
      if(INR >= 3.0) {
        dose[day:day+1] = 0
      } else {
        dose[day:day+3] = last_dose * (1-.125)
      }

      return(dose[1:days])
    }
    if(INR < 1.5) {
      dose[day:(day+5)] <- last_dose*1.15
    } else if(INR >= 1.5 && INR < 2) {
      dose[day:(day+9)] <- last_dose*1.075
    } else if(INR >=2 && INR <= 3.0) {
      dose[day:(day+30)] <- last_dose
    } else if(INR > 3 && INR <= 3.5) {
      dose[day:(day+14)] = last_dose * .9
    } else if(INR > 3.5 && INR <= 4) {
      dose[day:(day+1)] = 0
      dose[(day+3):(day+9)] = last_dose * .9
    } else if(INR > 4.0 && INR <= 5) {
      dose[day:(day+1)] = 0
      dose[(day+3):(day+4)] = last_dose * .9
    } else { # INR > 5
      dose[day:(day+1)] = 0 
    }

    return(dose[1:days])
  } 
}
