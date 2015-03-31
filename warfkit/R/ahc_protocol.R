
##################### AHC Protocol ########################
###########################################################

 
 ahc.adjustment.2.5 <- function(dose, INR, day){
  previous_dose <- dose[(day-1)]
  maxDays <- length(dose)
  if(INR < 1.5){
    dose[day:(day + 6)] = previous_dose * 1.15 
    return(dose[1:maxDays])
  }else if (INR >= 1.5 & INR < 1.8){
    dose[day:(day + 6)] = previous_dose * 1.1
    return(dose[1:maxDays])
  }else if (INR >= 1.8 & INR < 2){
    dose[day:(day + 6)] = previous_dose * 1.075
    return(dose[1:maxDays])
  }else if (INR >= 2 & INR <= 3){
    dose[day:(day + 6)] = previous_dose * 1
    return(dose[1:maxDays])
  }else if (INR > 3 & INR < 3.4){
    dose[day:(day + 6)] = previous_dose * 0.925
    return(dose[1:maxDays])
  }else if (INR >= 3.4 & INR < 4){
    dose[day:(day + 6)] = previous_dose * 0.9
    return(dose[1:maxDays])
  }else if (INR >= 4 & INR <= 5){
    dose[day:(day + 1)]= 0
    dose[(day + 2):(day + 6)] = previous_dose * 0.875
    return(dose[1:maxDays])
  }else if (INR > 5){
    dose[day:(day + 2)]= previous_dose * 0
    return(dose[1:maxDays])
  }
	  # dose[day:(day + 6)] = previous_dose * 0.85
  #return(as.numeric(as.character(dose)))
  #return(dose[1:maxDays])
}
 
 ahc.adjustment.3 <- function(dose, INR, day){
  previous_dose <- dose[(day-1)]
  maxDays <- length(dose)
  if(INR < 2.0){
    dose[day:(day + 6)] = previous_dose * 1.15 
    return(dose[1:maxDays])
  }else if (INR >= 2.0 & INR < 2.2){
    dose[day:(day + 6)] = previous_dose * 1.1
    return(dose[1:maxDays])
  }else if (INR >= 2.2 & INR < 2.5){
    dose[day:(day + 6)] = previous_dose * 1.075
    return(dose[1:maxDays])
  }else if (INR >= 2.5 & INR <= 3.5){
    dose[day:(day + 6)] = previous_dose * 1
    return(dose[1:maxDays])
  }else if (INR > 3.5 & INR < 3.8){
    dose[day:(day + 6)] = previous_dose * 0.925
    return(dose[1:maxDays])
  }else if (INR >= 3.8 & INR < 4.3){
    dose[day:(day + 6)] = previous_dose * 0.9
    return(dose[1:maxDays])
  }else if (INR >= 4.3 & INR <= 5){
    dose[day:(day + 1)]= 0
    dose[(day + 2):(day + 6)] = previous_dose * 0.875
    return(dose[1:maxDays])
  }else if (INR > 5){
    dose[day:(day + 2)]= previous_dose * 0
    return(dose[1:maxDays])
  }
  # }else if (INRs.3days <- tail(INRS, 3) > 5){
		  # dose[day:(day + 3)]= 0
	  # }
	  # dose[day:(day + 6)] = previous_dose * 0.85
  #return(as.numeric(as.character(dose)))
  #return(dose[1:maxDays])
}

 
 ahc_clinical_protocol = function(avatar,simulation) {
   maxDose = simulation$max_dose
   TINR = as.numeric(as.character(avatar$TINR))


  function(INR, dose, day, check) {
     INRs = INR[1:day]
     INR = INRs[day]
     INR.check = check[1:day]

 if(TINR <= 3.0) {
  TINR = "low"  
 } else {
  TINR = "high"
 }
 
 if(TINR=="low"){

 #source("ahc.adjustment.R")
 maxDays <- length(dose)
 if(day == 1){  
   dose[2] = dose[1]
   #return(dose)
   return(dose[1:maxDays])
 }else if(day == 2){#"Two days post warfarin initiation"
   if(INR >= 2){
    dose[3:4] = 5 #Everybody
   }else if(INR < 2){ #Assumption
   print(paste("here I am day: ", day))
    dose <- ahc.adjustment.2.5(dose, INR, day+1)
  }
  #return(dose)
  return(dose[1:maxDays])
 }else{
     INR.check.4wks <- tail(INR.check, 28)#The elements of these INR.check vectors are either a number higher than zero, or zero or "na".
     INR.check.2wks <- tail(INR.check, 14)
     INR.check.1wk <- tail(INR.check, 7)
     INR.check.2days <- tail(INR.check, 2)
     INR.check.1day <- tail(INR.check, 1)
 
     INRs.4wks <- tail(INRs, 28)[INR.check.4wks!=0]#This vector includes the INR values on check days.
     INRs.2wks <- tail(INRs, 14)[INR.check.2wks!=0]
     INRs.1wk <- tail(INRs, 7)[INR.check.1wk!=0]
     INRs.2days <- tail(INRs, 2)[INR.check.2days!=0]
     INRs.1day <- tail(INRs, 1)[INR.check.1day!=0]
 
     previous_dose <- dose[day-1]
     print(paste("previous dose: ", previous_dose))
     print(paste("day: ", day))

    if(previous_dose == 0){
      if(INR > 3){
        dose[(day):(day+3)] = 0
      }else{
        previous_dose = tail(Filter(function(a) { a != 0 && !is.na(a) }, dose), n=1)
        dose[(day+1):(day + 5)] = previous_dose * 0.85
      }    
      return(dose[1:maxDays])
    }
     
    if(day >= 28){#day_i
     #previous_dose <- dose[(day-1)]
     #INR is tested
     if(length(INRs.4wks[INRs.4wks < 2 | INRs.4wks > 3]) == 0){
      dose[(day+1):(day + 27)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.2wks[INRs.2wks < 2 | INRs.2wks > 3]) == 0){
      dose[(day+1):(day + 13)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1wk[INRs.1wk < 2 | INRs.1wk > 3]) == 0){
      dose[(day+1):(day + 6)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.2days[INRs.2days < 2 | INRs.2days > 3]) == 0){
      dose[(day+1):(day + 4)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1day[INRs.1day < 2 | INRs.1day > 3]) == 0){
      dose[(day+1):(day + 1)] = previous_dose
      return(dose[1:maxDays])      
     } else {
      return(ahc.adjustment.2.5(dose, INR, day+1))
     }
     #return(dose)
     #return(dose[1:maxDays])
   }else if (day >= 14){
     #previous_dose <- dose[(day-1)]
     if(length(INRs.2wks[INRs.2wks < 2 | INRs.2wks > 3]) == 0){
      dose[(day+1):(day + 13)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1wk[INRs.1wk < 2 | INRs.1wk > 3]) == 0){
      dose[(day+1):(day + 6)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.2days[INRs.2days < 2 | INRs.2days > 3]) == 0){
      dose[(day+1):(day + 4)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1day[INRs.1day < 2 | INRs.1day > 3]) == 0){
      dose[(day+1):(day + 1)] = previous_dose
      return(dose[1:maxDays])
     } else {
      return(ahc.adjustment.2.5(dose, INR, day+1))
     }
     #return(dose)
     #return(dose[1:maxDays])
   }else if (day >= 7){
     #previous_dose <- dose[(day-1)]
     print("day >=7")
     print(INRs.1wk)
     print(INRs.2days)
     print(INRs.1day)
     if (length(INRs.1wk[INRs.1wk < 2 | INRs.1wk > 3]) == 0){
      dose[(day+1):(day + 6)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.2days[INRs.2days < 2 | INRs.2days > 3]) == 0){
      dose[(day+1):(day + 4)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1day[INRs.1day < 2 | INRs.1day > 3]) == 0){
      dose[(day+1):(day + 2)] = previous_dose
      return(dose[1:maxDays])
     } else {
      return(ahc.adjustment.2.5(dose, INR, day+1))
     }
     #return(dose)
     #return(dose)
   }else if (day >= 3){
     #previous_dose <- dose[(day-1)]
     if (length(INRs.2days[INRs.2days < 2 | INRs.2days > 3]) == 0){
      dose[(day+1):(day + 4)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1day[INRs.1day < 2 | INRs.1day > 3]) == 0){
      dose[(day+1):(day + 1)] = previous_dose
      return(dose[1:maxDays])
     } else {
      return(ahc.adjustment.2.5(dose, INR, day+1))
     }
     #return(dose)
     #return(dose[1:maxDays])
   }
   #return(dose)
  }
  #return(as.numeric(as.character(dose)))
  # return(dose)
}else if(TINR=="high"){

 #source("ahc.adjustment.R")
 maxDays <- length(dose)
 if(day == 1){  
   dose[2] = dose[1]
   #return(dose)
   return(dose[1:maxDays])
 }else if(day == 2){#"Two days post warfarin initiation"
   if(INR >= 2.5){
    dose[3:4] = 5 #Everybody
   }else if(INR < 2.5){ #Assumption
   print(paste("here I am day: ", day))
    dose <- ahc.adjustment.3(dose, INR, day+1)
  }
  #return(dose)
  return(dose[1:maxDays])
 }else{
     INR.check.4wks <- tail(INR.check, 28)#The elements of these INR.check vectors are either a number higher than zero, or zero or "na".
     INR.check.2wks <- tail(INR.check, 14)
     INR.check.1wk <- tail(INR.check, 7)
     INR.check.2days <- tail(INR.check, 2)
     INR.check.1day <- tail(INR.check, 1)
 
     INRs.4wks <- tail(INRs, 28)[INR.check.4wks!=0]#This vector includes the INR values on check days.
     INRs.2wks <- tail(INRs, 14)[INR.check.2wks!=0]
     INRs.1wk <- tail(INRs, 7)[INR.check.1wk!=0]
     INRs.2days <- tail(INRs, 2)[INR.check.2days!=0]
     INRs.1day <- tail(INRs, 1)[INR.check.1day!=0]
 
     previous_dose <- dose[day-1]
     print(paste("previous dose: ", previous_dose))
     print(paste("day: ", day))
     
    if(previous_dose == 0){
      if(INR > 3.5){
        dose[(day):(day+3)] = 0
      }else{
        previous_dose = tail(Filter(function(a) { a != 0 && !is.na(a) }, dose), n=1)
        dose[(day+1):(day + 5)] = previous_dose * 0.85
      }    
      return(dose[1:maxDays])
    }
    if(day >= 28){#day_i
     #previous_dose <- dose[(day-1)]
     #INR is tested
     if(length(INRs.4wks[INRs.4wks < 2.5 | INRs.4wks > 3.5]) == 0){
      dose[(day+1):(day + 27)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.2wks[INRs.2wks < 2.5 | INRs.2wks > 3.5]) == 0){
      dose[(day+1):(day + 13)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1wk[INRs.1wk < 2.5 | INRs.1wk > 3.5]) == 0){
      dose[(day+1):(day + 6)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.2days[INRs.2days < 2.5 | INRs.2days > 3.5]) == 0){
      dose[(day+1):(day + 4)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1day[INRs.1day < 2.5 | INRs.1day > 3.5]) == 0){
      dose[(day+1):(day + 1)] = previous_dose
      return(dose[1:maxDays])      
     } else {
      return(ahc.adjustment.3(dose, INR, day+1))
     }
     #return(dose)
     #return(dose[1:maxDays])
   }else if (day >= 14){
     #previous_dose <- dose[(day-1)]
     if(length(INRs.2wks[INRs.2wks < 2.5 | INRs.2wks > 3.5]) == 0){
      dose[(day+1):(day + 13)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1wk[INRs.1wk < 2.5 | INRs.1wk > 3.5]) == 0){
      dose[(day+1):(day + 6)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.2days[INRs.2days < 2.5 | INRs.2days > 3.5]) == 0){
      dose[(day+1):(day + 4)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1day[INRs.1day < 2.5 | INRs.1day > 3.5]) == 0){
      dose[(day+1):(day + 1)] = previous_dose
      return(dose[1:maxDays])
     } else {
      return(ahc.adjustment.3(dose, INR, day+1))
     }
     #return(dose)
     #return(dose[1:maxDays])
   }else if (day >= 7){
     #previous_dose <- dose[(day-1)]
     print("day >=7")
     print(INRs.1wk)
     print(INRs.2days)
     print(INRs.1day)
     if (length(INRs.1wk[INRs.1wk < 2.5 | INRs.1wk > 3.5]) == 0){
      dose[(day+1):(day + 6)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.2days[INRs.2days < 2.5 | INRs.2days > 3.5]) == 0){
      dose[(day+1):(day + 4)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1day[INRs.1day < 2.5 | INRs.1day > 3.5]) == 0){
      dose[(day+1):(day + 2)] = previous_dose
      return(dose[1:maxDays])
     } else {
      return(ahc.adjustment.3(dose, INR, day+1))
     }
     #return(dose)
     #return(dose)
   }else if (day >= 3){
     #previous_dose <- dose[(day-1)]
     if (length(INRs.2days[INRs.2days < 2.5 | INRs.2days > 3.5]) == 0){
      dose[(day+1):(day + 4)] = previous_dose
      return(dose[1:maxDays])
     } else if (length(INRs.1day[INRs.1day < 2.5 | INRs.1day > 3.5]) == 0){
      dose[(day+1):(day + 1)] = previous_dose
      return(dose[1:maxDays])
     } else {
      return(ahc.adjustment.3(dose, INR, day+1))
     }
     #return(dose)
     #return(dose[1:maxDays])
   }
   #return(dose)
  }
  #return(as.numeric(as.character(dose)))
  # return(dose)
} 
}}

