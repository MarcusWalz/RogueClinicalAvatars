
##############################################################################################
## Please note the some of the dosing algorithms in this file are just for training purposes #
###################################################################################################
# In case you wanted to use them for any real simulations, please check their validity beforehand #
###################################################################################################

# Protocol randomization/assignment

# Description:
# 	Specifies the type of protocol to run and creates vector with assgined protocols.
# 	Currently supports 1 or 2 protocols, and works by dividing up population by 
# 	the number of protocols to use.

# Input:
#	numAvatars - Number of avatars in trial
#	protocolList - List of all protocols; protocols to use set to TRUE
#
# Output:
#	returns a vector of protocol assignments for each avatar
 
randomizeProtocols = function(numAvatars, protocolList) {
    
    numProtocolsToUse = length(which(unlist(protocolList)==T))
    if (numProtocolsToUse == 1 | numProtocolsToUse == 2) {
        return (rep(names(unlist(protocolList))[which(unlist(protocolList)==T)], each = numAvatars / numProtocolsToUse))
    } else {
        stop("  Your protocols are messed up - our lazy framework only supports up to 2 protocols, sucker.")
    }
    
}

# Convert doses to categorized doses
# Input:
#	dose - vector of doses to convert
#	warf_list - vector of values to round to
#
# Output:
#	vector of converted doses
 
dose_cat <- function(dose, maxDose){
    warf_list <- seq(0,maxDose,0.5)
	return(sapply(dose, near <- function(d_val){return(warf_list[which.min(abs(warf_list-d_val))])}))
}

# Clinical Avatars Dosing Protocols
 
# Description:
#     Various function calls to run different types of protocols associated with warfarin treatment.
#	All protocols assume that initial dose (dose[1]) has been set externally. The dose on the
#	current day (dose[day]) is the dose administered to achieve the current INR reading, e.g.:
#	day = 3 -> dose = d1, d2, d3, 0, 0, 0
#		     INR  = I1, I2, I3, 0, 0, 0 	
# 	In this case, we will be setting the dose for day 4 and onwards.

# Standard input/output for protocols in use
#
# Input:
#	INR - Today's INR reading
#	dose - Vector of all doses
#	day - Current day of trial
#	maxDose - Maximum possible dose (default 10mg)
#
# Output:
#	returns an updated vector of doses which contains
#	all doses calculated before next INR measurement is
#	required by protocol.

# Kovacs protocol dose lists

# List corresponds to treatment protocol using day 3 INR
# that generates doeses for day 3 and 4

kovacs3 <- list("<1.3" = c(15,15),
		 "1.3" = c(10,10),
		 "1.4" = c(10,10),
		 "1.5" = c(10,5),
		 "1.6" = c(10,5),
		 "1.7" = c(5,5),
		 "1.8" = c(5,5),
		 "1.9" = c(5,5),
		 "2" = c(2.5,2.5),
		 "2.1" = c(2.5,2.5),
		 "2.2" = c(2.5,2.5),
		 "2.3" = c(0,2.5),
		 "2.4" = c(0,2.5),
		 "2.5" = c(0,2.5),
		 "2.6" = c(0,2.5),
		 "2.7" = c(0,2.5),
		 "2.8" = c(0,2.5),
		 "2.9" = c(0,2.5),
		 "3" = c(0,2.5),
		 ">3" = c(0,0))

# List corresponds to treatment protocol using day 5 INR
# that generates doses for day 5,6,7. List is meta-indexed
# by day 4 dose (used as a surrogate for day 3 INR which
# determines which day 5 sub-protocol to use)

kovacs5 <- list("15" = list("<2" = c(15,15,15),
				"2" = c(7.5,5,7.5), 
				"2.1" = c(7.5,5,7.5),
				"2.2" = c(7.5,5,7.5),
				"2.3" = c(7.5,5,7.5),
				"2.4" = c(7.5,5,7.5),
				"2.5" = c(7.5,5,7.5),
				"2.6" = c(7.5,5,7.5),
				"2.7" = c(7.5,5,7.5),
				"2.8" = c(7.5,5,7.5),
				"2.9" = c(7.5,5,7.5),
				"3" = c(7.5,5,7.5),
				"3.1" = c(0,5,5),
				"3.2" = c(0,5,5),
				"3.3" = c(0,5,5),
				"3.4" = c(0,5,5),
				"3.5" = c(0,5,5),
				">3.5" = c(0,0,2.5)),
		 "10" = list("<2" = c(15,15,15),
				"2" = c(7.5,5,7.5), 
				"2.1" = c(7.5,5,7.5),
				"2.2" = c(7.5,5,7.5),
				"2.3" = c(7.5,5,7.5),
				"2.4" = c(7.5,5,7.5),
				"2.5" = c(7.5,5,7.5),
				"2.6" = c(7.5,5,7.5),
				"2.7" = c(7.5,5,7.5),
				"2.8" = c(7.5,5,7.5),
				"2.9" = c(7.5,5,7.5),
				"3" = c(7.5,5,7.5),
				"3.1" = c(0,5,5),
				"3.2" = c(0,5,5),
				"3.3" = c(0,5,5),
				"3.4" = c(0,5,5),
				"3.5" = c(0,5,5),
				">3.5" = c(0,0,2.5)),
		 "5" = list("<2" = c(7.5,7.5,7.5),
				"2" = c(5,5,5), 
				"2.1" = c(5,5,5),
				"2.2" = c(5,5,5),
				"2.3" = c(5,5,5),
				"2.4" = c(5,5,5),
				"2.5" = c(5,5,5),
				"2.6" = c(5,5,5),
				"2.7" = c(5,5,5),
				"2.8" = c(5,5,5),
				"2.9" = c(5,5,5),
				"3" = c(5,5,5),
				"3.1" = c(2.5,2.5,2.5),
				"3.2" = c(2.5,2.5,2.5),
				"3.3" = c(2.5,2.5,2.5),
				"3.4" = c(2.5,2.5,2.5),
				"3.5" = c(2.5,2.5,2.5),
				">3.5" = c(0,2.5,2.5)),
		 "2.5" = list("<2" = c(5,5,5),
				"2" = c(2.5,5,2.5), 
				"2.1" = c(2.5,5,2.5),
				"2.2" = c(2.5,5,2.5),
				"2.3" = c(2.5,5,2.5),
				"2.4" = c(2.5,5,2.5),
				"2.5" = c(2.5,5,2.5),
				"2.6" = c(2.5,5,2.5),
				"2.7" = c(2.5,5,2.5),
				"2.8" = c(2.5,5,2.5),
				"2.9" = c(2.5,5,2.5),
				"3" = c(2.5,5,2.5),
				"3.1" = c(0,2.5,0),
				"3.2" = c(0,2.5,0),
				"3.3" = c(0,2.5,0),
				"3.4" = c(0,2.5,0),
				"3.5" = c(0,2.5,0),
				">3.5" = c(0,0,2.5)),
		 "0" = list("<2" = c(2.5,2.5,2.5),
				"2" = c(2.5,0,2.5), 
				"2.1" = c(2.5,0,2.5),
				"2.2" = c(2.5,0,2.5),
				"2.3" = c(2.5,0,2.5),
				"2.4" = c(2.5,0,2.5),
				"2.5" = c(2.5,0,2.5),
				"2.6" = c(2.5,0,2.5),
				"2.7" = c(2.5,0,2.5),
				"2.8" = c(2.5,0,2.5),
				"2.9" = c(2.5,0,2.5),
				"3" = c(2.5,0,2.5),
				"3.1" = c(0,2.5,0),
				"3.2" = c(0,2.5,0),
				"3.3" = c(0,2.5,0),
				"3.4" = c(0,2.5,0),
				"3.5" = c(0,2.5,0),
				"3.6" = c(0,2.5,0),
				"3.7" = c(0,2.5,0),
				"3.8" = c(0,2.5,0),
				"3.9" = c(0,2.5,0),
				"4" = c(0,2.5,0),
				">4" = c(0,0,2.5)))

# Kovacs protocol (7 days)

kovacs_protocol = function(avatar, simulation) { 
  maxDose = simulation$max_dose
  
  function(INR, dose, day, check){

	maxDays <- length(dose) # Dose vector can be augmented by long recheck intervals; this makes sure that length(dose) is returned constant
	
    	# For days 1-7, the dosing regimen is pre-specified and stored in a list
	# corresponding to the day of adjustment (_3 and _5). After day 7, dose
	# is percent-adjusted based on INR

	if(day == 1){
    	dose[2] = dose[1]
    	return(dose)
    } else if(day == 2){
		# INR is converted to a string to use as a list index
		INR_str <- ifelse(INR < 1.3, "<1.3", ifelse(INR > 3, ">3", as.character(INR)))
		dose[3:4] <- kovacs3[[INR_str]]
		return(dose[1:maxDays])
	} else if(day == 4){
		# This conversion is more complicated because the max INR differs based on previous INR
		INR_str <- ifelse(INR < 2, "<2", ifelse(INR > 4 & dose[4] == 0, ">4", ifelse(INR > 3.5 & dose[4] != 0, ">3.5", as.character(INR))))
		dose[5:7] <- kovacs5[[as.character(dose[4])]][[INR_str]]
		return(dose[1:maxDays]) 
	} else {
		stop("Something is wrong in Kovacs. This protocol is only used for the first 7 days. Check what you did!!")
	}

}}

# Intermountain protocol (INR-based dose adjustment; unlimited)

intermt_protocol <- function(avatar, simulation) {
  maxDose = simulation$max_dose
  
  function(INR, dose, day, check){

	maxDays <- length(dose) # Dose vector can be augmented by long recheck intervals; this makes sure that length(dose) is returned constant
	
	if(dose[day] == 0){ # Only need to check last dose because only INR >= 5 will have last dose = 0
		prev_dose <- dose[(day-2)]
	} else {
		prev_dose <- dose[day]
    	}

	if(INR < 1){
		warning("Your INR was under 1 while using Intermountain...please take note")
		dose[(day+1)] <- prev_dose
		return(dose[1:maxDays])
	} else if(INR >= 1 & INR < 1.6){
		# Immediate extra dose (average of days 5-7 for day 8)
		dose[(day+1)] <- ifelse(day == 7, mean(dose[5:7]), prev_dose*2)
		dose[(day+2):(day+5)] <- min(prev_dose*1.1, 10)
		return(dose[1:maxDays])
	} else if(INR >= 1.6 & INR < 1.8){
		# Immediate extra half-dose (average of days 5-7 for day 8)
		dose[(day+1)] <- ifelse(day == 7, mean(dose[5:7]), prev_dose*1.5)			
		dose[(day+2):(day+7)] <- min(prev_dose*1.05, 10)
		return(dose[1:maxDays])
	} else if(INR >= 1.8 & INR < 2){
		if(dose[day] == 0){ 
			dose[(day+1):(day+14)] <- prev_dose*0.85
		} else {
			dose[(day+1):(day+14)] <- min(prev_dose*1.05, 10)
		}
		return(dose[1:maxDays])
	} else if(INR >= 2 & INR <= 3){
		if(dose[day] == 0){
			dose[(day+1):(day+14)] <- prev_dose*0.85
		} else {
			dose[(day+1):(day+14)] <- prev_dose
		}
		return(dose[1:maxDays])
	} else if(INR > 3 & INR < 3.4){
		if(dose[day] == 0){
			dose[(day+1):(day+14)] <- prev_dose*0.85
		} else {
			dose[(day+1):(day+14)] <- prev_dose*0.95 
		}
		return(dose[1:maxDays])
	} else if(INR >= 3.4 & INR < 5){
		ifelse(INR < 4, dose[(day+1)] <- prev_dose*0.5, dose[(day+1)] <- 0)
		dose[(day+2):(day+7)] <- prev_dose*0.9 
		return(dose[1:maxDays])
	} else if(INR >= 5){
		dose[(day+1):min(day+2)] <- 0 
		return(dose[1:maxDays])
	} else {
		stop("Something is wrong in Intermountain...How did you reach here?")
	}
}}


# Coumagen protocol (unlimited days)
# Converts doses prescribed by Kovacs using "pharmacogenetic coefficient"-
#  {predicted weekly dose}/{guideline weekly dose = 35mg} (days 1-7)
# Assumes that daily dose is provided, and that the first two doses are
# 2*{predicted daily dose}
# After day 7, uses Intermountain to do INR-based adjustment

coumagen_pharm_protocol = 
function(avatar, simulation) {
  maxDose = simulation$max_dose

  function(INR, dose, day, check, maxDose = 10){
  
  pharm_coeff <- (.5*dose[1]*7)/35
  
  if(day == 1){
    dose[2] = dose[1]
    return(dose)
  } else if(day == 2){
    dose <- kovacs(INR, dose, day, check, maxDose = 10)
    dose[3:4] <- dose[3:4]*pharm_coeff
    return(dose)
  } else if(day == 4){
    dose[3:4] <- dose[3:4]*(1/pharm_coeff)
    dose <- kovacs(INR, dose, day, check, maxDose = 10)
    dose[3:7] <- dose[3:7]*pharm_coeff
    return(dose)
  } else if(day >= 7){
    return(intermt(INR, dose, day, check, maxDose = 10))
  } else {
    stop("Something is wrong in coumagen_pharm...check what you did!")
  }
  
}}



fen_intermt_protocol = function(avatar, simulation) {
  maxDose = simulation$max_dose

  function(INR, dose, day, check){
  if (day <= 7) {
    INR_str <- ifelse(INR < 1.4, "<1.4", ifelse(INR > 4.5, ">4.5", as.character(INR)))
    day_str <- as.character(day)
    
    if(is.null(fen[[day_str]][[INR_str]])){
      dose[(day+1)] <- dose[day]
      return(dose)
    }
    
    dose[(day+(1:length(fen[[day_str]][[INR_str]])))] <- fen[[day_str]][[INR_str]]
    return(dose)
    
    }else if(day>7){
      return(intermt(INR, dose, day, check, maxDose = 10))
    }else{
        stop("Something is wrong in coumagen_pharm...check what you did!")
      }
}}

# Coumagen standard protocol (unlimited days)
# uses Kovacs for first 6 days
# After day 7, uses Intermountain to do INR-based adjustment

coumagen_standard_protocol = function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){
	
	if(day == 1){
		dose[2] = dose[1]
		return(dose)
	} else if(day == 2){
		dose <- kovacs_protocol(avatar, simulation)(INR, dose, day, check)
		return(dose)
	} else if(day == 4){
		dose <- kovacs_protocol(avatar, simulation)(INR, dose, day, check)
		return(dose)
	} else if(day >= 7){
		return(intermt_protocol(avatar, simulation)(INR, dose, day, check))
	} else {
		stop("Something is wrong in coumagen_standard...check what you did!")
	}		
}}

# combined coumagen pharm protocol first 7 days followed by wilson
wilson_coumagen_pharm_protocol = 
function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){
    if (day < 7) {
        return(coumagen_pharm_protocol(avatar, simulation)(INR, dose, day, check))
    } else {
        return(wilson_protocol(avatar, simulation)(INR, dose, day, check))
    }
}}

# combined coumagen standard protocol first 7 days followed by wilson
wilson_coumagen_standard_protocol = 
function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){
    if (day < 7) {
        return(coumagen_standard_protocol(avatar, simulation)(INR, dose, day, check))
    } else {
        return(wilson_protocol(avatar, simulation)(INR, dose, day, check))
    }
}}

# Wilson protocol (unlimited days)

wilson_protocol = 
function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){
	maxDays <- length(dose)

	if(INR <= 1.3){ # Recheck in 5 days -> set 5 doses
		dose[(day+1):(day+5)] <- min(dose[day]*1.5, maxDose)
		return(dose[1:maxDays])
	} else if(INR == 1.4){
		dose[(day+1):(day+5)] <- min(dose[day]*1.33, maxDose)
		return(dose[1:maxDays])
	} else if(INR >= 1.5 & INR <= 1.8){
		dose[(day+1):(day+5)] <- min(dose[day]*1.25, maxDose)
		return(dose[1:maxDays])
	} else if(INR == 1.9){ # Recheck in 7 days
		dose[(day+1):(day+7)] <- min(dose[day]*1.1, maxDose)
		return(dose[1:maxDays])
	} else if(INR >= 2 & INR <= 2.8){ # Recheck in 14 days
		dose[(day+1):(day+14)] <- dose[day]
		return(dose[1:maxDays])
	} else if(INR >= 2.9 & INR <= 3.1){
		dose[(day+1):min(day+7)] <- dose[day]*0.9
		return(dose[1:maxDays])
	} else if(INR >= 3.2 & INR <= 3.5){
		dose[(day+1):min(day+7)] <- dose[day]*0.75
		return(dose[1:maxDays])
	} else if(INR >= 3.6 & INR <= 3.7){
		dose[(day+1):(day+7)] <- dose[day]*0.67
		return(dose[1:maxDays])
	} else if(INR >= 3.8 & INR <= 3.9){ # Hold for 1 day, recheck in 5 days
		dose[(day+1)] <- 0
		dose[(day+2):(day+5)] <- dose[day]*0.67
		return(dose[1:maxDays])
	} else if(INR >= 4.0 & INR <= 4.4){
		dose[(day+1)] <- 0
		dose[(day+2):(day+3)] <- dose[day]*0.67
		return(dose[1:maxDays])
	} else if(INR >= 4.5 & INR <= 5.0){
		dose[(day+1):(day+2)] <- 0
		dose[(day+3)] <- dose[day]*0.67
		return(dose[1:maxDays])
	} else if(INR >= 5.1){ 
		dose[(day+1):(day+3)] <- 0
		dose[(day+4)] <- dose[day]*0.5 # added this based on wilson protocol to prevent 0 mg continual doses (VF 01/25/12)
		return(dose[1:maxDays])
	} else {
		stop("Something is wrong...How did you reach here?")
	}
	stop("Something is terribly wrong...")
}}

# Fennerty protocol (4 days)

# List containing Fennerty doses

fen <- list("1" = list("<1.4" = 10),
		"2" = list("<1.4" = 10,
			     "1.4" = 10,
			     "1.5" = 10,
			     "1.6" = 10,
			     "1.7" = 10,
			     "1.8" = 1,
			     "1.9" = 0.5,
                       "2" = 0.5,
			     "2.1" = 0.5,
		           "2.2" = 0.5,
		           "2.3" = 0.5,
		           "2.4" = 0.5,
		           "2.5" = 0.5,
		           "2.6" = 0.5,
		           "2.7" = 0.5,
		           "2.8" = 0.5,
		           "2.9" = 0.5,
		           "3" = 0.5,
		           "3.1" = 0.5,
		           "3.2" = 0.5,
		           "3.3" = 0.5,
		           "3.4" = 0.5,
		           "3.5" = 0.5,
		           "3.6" = 0.5,
		           "3.7" = 0.5,
		           "3.8" = 0.5,
		           "3.9" = 0/5,
		           "4" = 0.5,
		           "4.1" = 0.5,
		           "4.2" = 0.5,
		           "4.3" = 0.5,
		           "4.4" = 0.5,
		           "4.5" = 0.5,
		           ">4.5" = 0.5),
		"3" = list("<1.4" = 10,
			     "1.4" = 10,
			     "1.5" = 10,
			     "1.6" = 10,
			     "1.7" = 10,
			     "1.8" = 10,
			     "1.9" = 10,
                       "2" = 5,
			     "2.1" = 5,
		           "2.2" = 4.5,
		           "2.3" = 4.5,
		           "2.4" = 4,
		           "2.5" = 4,
		           "2.6" = 3.5,
		           "2.7" = 3.5,
		           "2.8" = 3,
		           "2.9" = 3,
		           "3" = 2.5,
		           "3.1" = 2.5,
		           "3.2" = 2,
		           "3.3" = 2,
		           "3.4" = 1.5,
		           "3.5" = 1,
		           "3.6" = 0.5,
		           "3.7" = 0.5,
		           "3.8" = 0.5,
		           "3.9" = 0.5,
		           "4" = 0.5,
		           "4.1" = 0.5,
		           "4.2" = 0.5,
		           "4.3" = 0.5,
		           "4.4" = 0.5,
		           "4.5" = 0.5,
		           ">4.5" = 0.5),
		"4" = list("<1.4" = sample(c(9,9.5,10), 1),
			     "1.4" = 8,
			     "1.5" = 7.5,
			     "1.6" = 7,
			     "1.7" = 7,
			     "1.8" = 6.5,
			     "1.9" = 6,
                       "2" = 5.5,
			     "2.1" = 5.5,
		           "2.2" = 5,
		           "2.3" = 5,
		           "2.4" = 4.5,
		           "2.5" = 4.5,
		           "2.6" = 4.5,
		           "2.7" = 4,
		           "2.8" = 4,
		           "2.9" = 4,
		           "3" = 4,
		           "3.1" = 3.5,
		           "3.2" = 3.5,
		           "3.3" = 3.5,
		           "3.4" = 3.5,
		           "3.5" = 3.5,
		           "3.6" = 3,
		           "3.7" = 3,
		           "3.8" = 3,
		           "3.9" = 3,
		           "4" = 3,
		           "4.1" = c(0,2),
		           "4.2" = c(0,2),
		           "4.3" = c(0,2),
		           "4.4" = c(0,2),
		           "4.5" = c(0,2),
		           ">4.5" = c(0,0,1)))


fennerty_protocol =
function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){
	maxDays <- length(dose)
    if (day <= 4) {
    	INR_str <- ifelse(INR < 1.4, "<1.4", ifelse(INR > 4.5, ">4.5", as.character(INR)))
    	day_str <- as.character(day)
	
    	if(is.null(fen[[day_str]][[INR_str]])){
    		dose[(day+1)] <- dose[day]
    		return(dose)
    	}

    	dose[(day+(1:length(fen[[day_str]][[INR_str]])))] <- fen[[day_str]][[INR_str]]
    	return(dose)
    } else {
        dose[(day+1)] = dose[day]
        return(dose)
    }    
}}

# Cooper protocol (modified Fennerty; 4 days)

# List containing Cooper doses

coop <- list("1" = list("<1.4" = 10),
		 "2" = list("<1.4" = 10,
			     "1.4" = 10,
			     "1.5" = 10,
			     "1.6" = 10,
			     "1.7" = 10,
			     "1.8" = 1,
			     "1.9" = 1,
                       "2" = 1,
			     "2.1" = 0,
		           "2.2" = 0,
		           "2.3" = 0,
		           "2.4" = 0,
		           "2.5" = 0,
		           "2.6" = 0,
		           "2.7" = 0,
		           "2.8" = 0,
		           "2.9" = 0,
		           "3" = 0,
		           "3.1" = 0,
		           "3.2" = 0,
		           "3.3" = 0,
		           "3.4" = 0,
		           "3.5" = 0,
		           "3.6" = 0,
		           "3.7" = 0,
		           "3.8" = 0,
		           "3.9" = 0,
		           "4" = 0,
		           ">4" = 0),
		"3" = list("<1.4" = 10,
			     "1.4" = 10,
			     "1.5" = 10,
			     "1.6" = 10,
			     "1.7" = 10,
			     "1.8" = 10,
			     "1.9" = 10,
                       "2" = 5,
			     "2.1" = 5,
		           "2.2" = 5,
		           "2.3" = 4,
		           "2.4" = 4,
		           "2.5" = 4,
		           "2.6" = 3,
		           "2.7" = 3,
		           "2.8" = 3,
		           "2.9" = 3,
		           "3" = 2,
		           "3.1" = 2,
		           "3.2" = 2,
		           "3.3" = 1,
		           "3.4" = 1,
		           "3.5" = 1,
		           "3.6" = 0,
		           "3.7" = 0,
		           "3.8" = 0,
		           "3.9" = 0,
		           "4" = 0,
		           ">4" = 0),
		"4" = list("<1.4" = sample(c(9,9.5,10), 1),
			     "1.4" = 8,
			     "1.5" = 8,
			     "1.6" = 7,
			     "1.7" = 7,
			     "1.8" = 6,
			     "1.9" = 6,
                       "2" = 5,
			     "2.1" = 5,
		           "2.2" = 5,
		           "2.3" = 5,
		           "2.4" = 4,
		           "2.5" = 4,
		           "2.6" = 4,
		           "2.7" = 4,
		           "2.8" = 4,
		           "2.9" = 4,
		           "3" = 4,
		           "3.1" = 3,
		           "3.2" = 3,
		           "3.3" = 3,
		           "3.4" = 3,
		           "3.5" = 3,
		           "3.6" = 3,
		           "3.7" = 3,
		           "3.8" = 3,
		           "3.9" = 3,
		           "4" = 3,
		           ">4" = 0))

cooper_protocol =
function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){

	INR_str <- ifelse(INR < 1.4, "<1.4", ifelse(INR > 4, ">4", as.character(INR)))
	day_str <- as.character(day)

	if(day > 4 & INR > 3){
		dose[(day+1)] <- 0
		return(dose)
	} else if(day > 4 & INR < 3){
		dose[(day+1)] <- dose[day]
		if (dose[day] == 0) {
		    dose[(day+1)] = 5   # hack - give "average dose" b/c cooper doesn't say what to do hotshot.
		    cat("Cooper here - what the hell are you guys doing to my protocol?\n")
	    }
	    return(dose)
	}
	dose[(day+1)] <- ifelse(is.null(coop[[day_str]][[INR_str]]), dose[day], coop[[day_str]][[INR_str]])
	return(dose)

}}


cooper_intermt_protocol =
function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){
  
  INR_str <- ifelse(INR < 1.4, "<1.4", ifelse(INR > 4, ">4", as.character(INR)))
  day_str <- as.character(day)
  
  if(day <=7  & INR > 3){
    dose[(day+1)] <- 0
    return(dose)
  } else if(day <= 7 & INR < 3){
    dose[(day+1)] <- dose[day]
    if (dose[day] == 0) {
      dose[(day+1)] = 5   # hack - give "average dose" b/c cooper doesn't say what to do hotshot.
      #cat("Cooper here - what the hell are you guys doing to my protocol?\n")
    }
    return(dose)
  }
  dose[(day+1)] <- ifelse(is.null(coop[[day_str]][[INR_str]]), dose[day], coop[[day_str]][[INR_str]])
  return(dose)
  
  if(day > 7){
    return(intermt_protocol(avatar, simulation)(INR, dose, day, check))
  }
  
}}



# Gedge protocol (modified Fennerty; 4 days)

# List containing Gedge doses

ged <- list("1" = list("<1.4" = 10),
		"2" = list("<1.4" = 5,
			     "1.4" = 5,
			     "1.5" = 5,
			     "1.6" = 5,
			     "1.7" = 5,
			     "1.8" = 1,
			     "1.9" = 1,
                       "2" = 1,
			     "2.1" = 0,
		           "2.2" = 0,
		           "2.3" = 0,
		           "2.4" = 0,
		           "2.5" = 0,
		           "2.6" = 0,
		           "2.7" = 0,
		           "2.8" = 0,
		           "2.9" = 0,
		           "3" = 0,
		           "3.1" = 0,
		           "3.2" = 0,
		           "3.3" = 0,
		           "3.4" = 0,
		           "3.5" = 0,
		           "3.6" = 0,
		           "3.7" = 0,
		           "3.8" = 0,
		           "3.9" = 0,
		           "4" = 0,
		           ">4" = 0),
		"3" = list("<1.4" = 5,
			     "1.4" = 5,
			     "1.5" = 5,
			     "1.6" = 5,
			     "1.7" = 5,
			     "1.8" = 5,
			     "1.9" = 5,
                       "2" = 4,
			     "2.1" = 4,
		           "2.2" = 4,
		           "2.3" = 4,
		           "2.4" = 4,
		           "2.5" = 4,
		           "2.6" = 3,
		           "2.7" = 3,
		           "2.8" = 3,
		           "2.9" = 3,
		           "3" = 2,
		           "3.1" = 2,
		           "3.2" = 2,
		           "3.3" = 1,
		           "3.4" = 1,
		           "3.5" = 1,
		           "3.6" = 0,
		           "3.7" = 0,
		           "3.8" = 0,
		           "3.9" = 0,
		           "4" = 0,
		           ">4" = 0),
		"4" = list("<1.4" = sample(c(8,8.5,9,9.5,10), 1),
			     "1.4" = 7,
			     "1.5" = 7,
			     "1.6" = 6,
			     "1.7" = 6,
			     "1.8" = 5,
			     "1.9" = 5,
                 "2" = 4,
			     "2.1" = 4,
		         "2.2" = 4,
		         "2.3" = 4,
		         "2.4" = 3,
		         "2.5" = 3,
		         "2.6" = 3,
		         "2.7" = 3,
		         "2.8" = 3,
		         "2.9" = 3,
		         "3" = 3,
		         "3.1" = 2,
		         "3.2" = 2,
		         "3.3" = 1,
		         "3.4" = 1,
		         "3.5" = 1,
		         "3.6" = 0,
		         "3.7" = 0,
		         "3.8" = 0,
		         "3.9" = 0,
		         ">4" = 0))

gedge_protocol =
function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){

    if (day <= 4) {
    	INR_str <- ifelse(INR < 1.4, "<1.4", ifelse(INR > 4, ">4", as.character(INR)))
    	day_str <- as.character(day)

    	dose[(day+1)] <- ifelse(is.null(ged[[day_str]][[INR_str]]), dose[day], ged[[day_str]][[INR_str]])
    	return(dose)
	} else {
	    dose[(day+1)] = dose[day]
	    return(dose)
	}

}}

gedge_intermt_protocol =
function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){
  
  if (day <= 4) {
    INR_str <- ifelse(INR < 1.4, "<1.4", ifelse(INR > 4, ">4", as.character(INR)))
    day_str <- as.character(day)
    
    dose[(day+1)] <- ifelse(is.null(ged[[day_str]][[INR_str]]), dose[day], ged[[day_str]][[INR_str]])
    return(dose)
  } else if (day %in% c(5, 6, 7)){
    dose[(day+1)] = dose[day]
    return(dose)
  } else{
    return(intermt_protocol(avatar, simulation)(INR, dose, day, check))
  }
  
}}


# Roberts protocol (4 days)

# Lists of Roberts doses by day

rob1 <- list("<1.4" = list("<=50" = 10,
				   "51-65" = 9,
				   "66-80" = 7.5,
				   ">80" = 6))

rob2 <- list("<=1.5" = list("<=50" = 10,
				    "51-65" = 9,
				    "66-80" = 7.5,
				    ">80" = 6),
		 ">=1.6" = list("<=50" = 0.5,
				    "51-65" = 0.5,
				    "66-80" = 0.5,
				    ">80" = 0.5))

rob3 <- list("<=1.7" = list("<=50" = 10,
				    "51-65" = 9,
				    "66-80" = 7.5,
				    ">80" = 6),
		 "1.8" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "1.9" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.1" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.2" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.3" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.4" = list("<=50" = 4,
				 "51-65" = 3.5,
				 "66-80" = 3,
				 ">80" = 2),
		 "2.5" = list("<=50" = 4,
				 "51-65" = 3.5,
				 "66-80" = 3,
				 ">80" = 2),
		 "2.6" = list("<=50" = 4,
				 "51-65" = 3.5,
				 "66-80" = 3,
				 ">80" = 2),
		 "2.7" = list("<=50" = 4,
				 "51-65" = 3.5,
				 "66-80" = 3,
				 ">80" = 2),
		 "2.8" = list("<=50" = 3,
				 "51-65" = 2.5,
				 "66-80" = 2,
				 ">80" = 1),
		 "2.9" = list("<=50" = 3,
				 "51-65" = 2.5,
				 "66-80" = 2,
				 ">80" = 1),
		 "3" = list("<=50" = 3,
				 "51-65" = 2.5,
				 "66-80" = 2,
				 ">80" = 1),
		 "3.1" = list("<=50" = 3,
				 "51-65" = 2.5,
				 "66-80" = 2,
				 ">80" = 1),
		 "3.2" = list("<=50" = 2,
				 "51-65" = 2,
				 "66-80" = 1.5,
				 ">80" = 1),
		 "3.3" = list("<=50" = 2,
				 "51-65" = 2,
				 "66-80" = 1.5,
				 ">80" = 1),
		 "3.4" = list("<=50" = 1.5,
				 "51-65" = 1.5,
				 "66-80" = 1,
				 ">80" = 1),
		 "3.5" = list("<=50" = 1,
				 "51-65" = 1,
				 "66-80" = 1,
				 ">80" = 0.5),
		 "3.6" = list("<=50" = 0.5,
				 "51-65" = 0.5,
				 "66-80" = 0.5,
				 ">80" = 0.5),
		 "3.7" = list("<=50" = 0.5,
				 "51-65" = 0.5,
				 "66-80" = 0.5,
				 ">80" = 0.5),
		 "3.8" = list("<=50" = 0.5,
				 "51-65" = 0.5,
				 "66-80" = 0.5,
				 ">80" = 0.5),
		 "3.9" = list("<=50" = 0.5,
				 "51-65" = 0.5,
				 "66-80" = 0.5,
				 ">80" = 0.5),
		 "4" = list("<=50" = 0.5,
				 "51-65" = 0.5,
				 "66-80" = 0.5,
				 ">80" = 0.5),
		 ">4" = list("<=50" = 0,
				 "51-65" = 0,
				 "66-80" = 0,
				 ">80" = 0))

rob4 <- list("<=1.5" = list("<=50" = 10,
				    "51-65" = 10,
				    "66-80" = 10,
				    ">80" = 9),
		 "1.6" = list("<=50" = 8,
				 "51-65" = 7,
				 "66-80" = 6,
				 ">80" = 5),
		 "1.7" = list("<=50" = 7,
				 "51-65" = 6,
				 "66-80" = 5,
				 ">80" = 4),
		 "1.8" = list("<=50" = 7,
				 "51-65" = 6,
				 "66-80" = 5,
				 ">80" = 4),
		 "1.9" = list("<=50" = 6,
				 "51-65" = 5,
				 "66-80" = 4.5,
				 ">80" = 3.5),
		 "2" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.1" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.2" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.3" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.4" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.5" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.6" = list("<=50" = 5,
				 "51-65" = 4.5,
				 "66-80" = 4,
				 ">80" = 3),
		 "2.7" = list("<=50" = 4,
				 "51-65" = 3.5,
				 "66-80" = 3,
				 ">80" = 2.5),
		 "2.8" = list("<=50" = 4,
				 "51-65" = 3.5,
				 "66-80" = 3,
				 ">80" = 2.5),
		 "2.9" = list("<=50" = 4,
				 "51-65" = 3.5,
				 "66-80" = 3,
				 ">80" = 2.5),
		 "3" = list("<=50" = 4,
				 "51-65" = 3.5,
				 "66-80" = 2,
				 ">80" = 2.5),
		 "3.1" = list("<=50" = 3.5,
				 "51-65" = 3,
				 "66-80" = 2.5,
				 ">80" = 2),
		 "3.2" = list("<=50" = 3.5,
				 "51-65" = 3,
				 "66-80" = 2.5,
				 ">80" = 2),
		 "3.3" = list("<=50" = 3.5,
				 "51-65" = 3,
				 "66-80" = 2.5,
				 ">80" = 2),
		 "3.4" = list("<=50" = 3.5,
				 "51-65" = 3,
				 "66-80" = 2.5,
				 ">80" = 2),
		 "3.5" = list("<=50" = 3.5,
				 "51-65" = 3,
				 "66-80" = 2.5,
				 ">80" = 2),
		 "3.6" = list("<=50" = 3,
				 "51-65" = 2.5,
				 "66-80" = 2,
				 ">80" = 1.5),
		 "3.7" = list("<=50" = 3,
				 "51-65" = 2.5,
				 "66-80" = 2,
				 ">80" = 1.5),
		 "3.8" = list("<=50" = 3,
				 "51-65" = 2.5,
				 "66-80" = 2,
				 ">80" = 1.5),
		 "3.9" = list("<=50" = 3,
				 "51-65" = 2.5,
				 "66-80" = 2,
				 ">80" = 1.5),
		 "4" = list("<=50" = 3,
				 "51-65" = 2.5,
				 "66-80" = 2,
				 ">80" = 1.5),
		 "4.1" = list("<=50" = c(0,1.5),
				 "51-65" = c(0,1),
				 "66-80" = c(0,1),
				 ">80" = c(0,0.5)),
		 "4.2" = list("<=50" = c(0,1.5),
				 "51-65" = c(0,1),
				 "66-80" = c(0,1),
				 ">80" = c(0,0.5)),
		 "4.3" = list("<=50" = c(0,1.5),
				 "51-65" = c(0,1),
				 "66-80" = c(0,1),
				 ">80" = c(0,0.5)),
		 "4.4" = list("<=50" = c(0,1.5),
				 "51-65" = c(0,1),
				 "66-80" = c(0,1),
				 ">80" = c(0,0.5)),	
		 ">4.5" = list("<=50" = 0,
				 "51-65" = 0,
				 "66-80" = 0,
				 ">80" = 0))

# This protocol also uses the avatar's age

roberts_protocol = function(avatar, simulation) {
  maxDose = simulation$max_dose
  age = as.numeric(as.character(avatar$AGE))
  function(INR, dose, day, check){
	age_str <- ifelse(age <= 50, "<=50", ifelse((age > 50 & age <=65), "51-65", ifelse((age > 65 & age <= 80), "66-80", ">80")))

	if(day == 1){
		INR_str <- ifelse(INR < 1.4, "<1.4", as.character(INR))
		dose[(day+1)] <- ifelse(is.null(rob1[[INR_str]][[age_str]]), dose[day], rob1[[INR_str]][[age_str]])
		return(dose)
	} else if(day == 2){
		INR_str <- ifelse(INR <= 1.5, "<=1.5", ">=1.6")
		dose[(day+1)] <- rob2[[INR_str]][[age_str]]
		return(dose)
	} else if(day == 3){
		INR_str <- ifelse(INR <= 1.7, "<=1.7", ifelse(INR > 4, ">4", as.character(INR)))
		dose[(day+1)] <- rob3[[INR_str]][[age_str]]
		return(dose)
	} else if(day == 4){
		INR_str <- ifelse(INR <= 1.5, "<=1.5", ifelse(INR > 4.5, ">4.5", as.character(INR)))
		dose[(day+(1:length(rob4[[INR_str]][[age_str]])))] <- rob4[[INR_str]][[age_str]]
		return(dose)
	} else {
		dose[(day+1)] = dose[day]
		return(dose)
	}
	
}}

rob_intermt_protocol = 
function(avatar, simulation) {
  maxDose = simulation$max_dose
  age = avatar$AGE
  function(INR, dose, day, check){
  age_str <- ifelse(age <= 50, "<=50", ifelse((age > 50 & age <=65), "51-65", ifelse((age > 65 & age <= 80), "66-80", ">80")))
  
  if(day == 1){
    INR_str <- ifelse(INR < 1.4, "<1.4", as.character(INR))
    dose[(day+1)] <- ifelse(is.null(rob1[[INR_str]][[age_str]]), dose[day], rob1[[INR_str]][[age_str]])
    return(dose)
  } else if(day == 2){
    INR_str <- ifelse(INR <= 1.5, "<=1.5", ">=1.6")
    dose[(day+1)] <- rob2[[INR_str]][[age_str]]
    return(dose)
  } else if(day == 3){
    INR_str <- ifelse(INR <= 1.7, "<=1.7", ifelse(INR > 4, ">4", as.character(INR)))
    dose[(day+1)] <- rob3[[INR_str]][[age_str]]
    return(dose)
  } else if(day == 4){
    INR_str <- ifelse(INR <= 1.5, "<=1.5", ifelse(INR > 4.5, ">4.5", as.character(INR)))
    dose[(day+(1:length(rob4[[INR_str]][[age_str]])))] <- rob4[[INR_str]][[age_str]]
    return(dose)
  } else if (day %in% c(5, 6, 7)) {
    dose[(day+1)] = dose[day]
    return(dose)
  } else {
    return(intermt_protocol(avatar, simulation)(INR, dose, day, check))
  }
  
}}





# fixed dose protocol - the dose is constant the entire time and ignores INR
fixed_dose_protocol = 
function(avatar, simulation) {
  function(INR, dose, day, check){

    maxDays = length(dose)
    # check on days 1, 2, 3, 7, every 7, then 14 
    if (day == 1 | day == 2) {
        dose[day+1] = dose[day]
    } else if (day == 3) {
        dose[(day+1):7] = rep(dose[day], (7-day))
    } else if (day == 7) {
        dose[(day+1):14] = rep(dose[day], (14-day))
    } else if (day >= 14) {
        dose[(day+1):(day+14)] = rep(dose[day], 14)
    } else {
        stop("Somthing is horribly wrong with the fixed_dose protocol")
    }
    return(dose[1:maxDays])

}}
# random protocol based off "know_your_patient"
# this protocol will ignore the INR value and provide a random dose according to the days specified
# sets the random seed based on the system time.  Otherwise, it will use the rseed value from the INR function and it
# will be the same number every time.  It's too fast for the time so you can still get the same seed hence divide by day.
# Apparently, the set.seed works globablly 
know_your_patient_random_protocol = 
function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){
    #set.seed(rseed)
    possibleDoses = seq(0, maxDose, 2.5)
    maxDays = length(dose)
    # check on days 1, 2, 3, 7, every 7, then 14 
    if (day == 1 | day == 2) {
        dose[day+1] = sample(possibleDoses, 1)
    } else if (day == 3) {
        dose[(day+1):7] = rep(sample(possibleDoses, 1), (7-day))
    } else if (day == 7) {
        dose[(day+1):14] = rep(sample(possibleDoses, 1), (14-day))
    } else if (day >= 14) {
        dose[(day+1):(day+14)] = rep(sample(possibleDoses, 1), 14)
    } else {
        stop("Somthing is horribly wrong with the know_your_patient_random protocol")
    }
    return(dose[1:maxDays])
}}

# based on the nomogram from the BWH warfarin clinic - it's very vague so we made some assumptions and guesses
# in the clinic they start everyone at 5 mg and adjust in 2.5 mg increments
# this is the INR 2-3 protocol
# sets the random seed based on the system time.  Otherwise, it will use the rseed value from the INR function and it
# will be the same number every time.  It's too fast for the time so you can still get the same seed hence divide by day.
# Apparently, the set.seed works globablly.
# for day 28 and beyond we check if the previous 14 doses are equal to the current dose and if the INR is in range to indicate stability
know_your_patient_protocol = function(avatar, simulation) {
  maxDose = simulation$max_dose
  function(INR, dose, day, check){
    set.seed(rseed)
    possibleDoses = seq(0, maxDose, 2.5)
    maxDays = length(dose)
    # check on days 1, 2, 3, 7, every 7, then 14 
    
    if (day == 1 | day == 2) {
        dose[day+1] = know_your_patient23(INR, dose, day, check, possibleDoses, 1)
    } else if (day == 3) {
        dose[(day+1):7] = know_your_patient23(INR, dose, day, check, possibleDoses, (7-day))
    } else if (day == 7) {
        dose[(day+1):14] = know_your_patient23(INR, dose, day, check, possibleDoses, (14-day))
    } else if (day >= 14 & day < 28) {
        if (INR < 2.0) {
            dose[(day+1):(day+7)] = know_your_patient23(INR, dose, day, check, possibleDoses, 7)
        } else if (INR >= 2.0 & INR <= 3.0) {
            dose[(day+1):(day+14)] = know_your_patient23(INR, dose, day, check, possibleDoses, 14) 
        } else {
            dose[(day+1):(day+7)] = know_your_patient23(INR, dose, day, check, possibleDoses, 7)
        }
    } else if (day >= 28) {
        if (INR < 2.0) {
            dose[(day+1):(day+7)] = know_your_patient23(INR, dose, day, check, possibleDoses, 7)
        } else if (INR >= 2.0 & INR <= 3.0 & dose[day] == mean(dose[(day-14):day])) {
            dose[(day+1):(day+30)] = know_your_patient23(INR, dose, day, check, possibleDoses, 30) 
        } else {
            dose[(day+1):(day+7)] = know_your_patient23(INR, dose, day, check, possibleDoses, 7)
        }
    } 
    else {
        stop("Somthing is horribly wrong with the know_your_patient protocol")
    }
    return(dose[1:maxDays])
}}

know_your_patient23 = function(INR, dose, day, check, possibleDoses, numDoses) {
    if (INR < 2.0) {
        tmpDoses = possibleDoses[possibleDoses >= dose[day]]
        if (length(tmpDoses) == 1) {
            adjDose = rep(tmpDoses, numDoses)
        } else {
            adjDose = rep(sample(tmpDoses, 1), numDoses)   # sample doesn't work with length = 1 - lame
        }
    } else if (INR >= 2.0 & INR <= 3.0) {
        adjDose = rep(dose[day], numDoses)
    } else if (INR > 3 & INR <= 3.5) {
        tmpDoses = possibleDoses[possibleDoses <= dose[day]]
        if (length(tmpDoses) == 1){
            adjDose = rep(tmpDoses, numDoses)
        } else {
            adjDose = rep(sample(tmpDoses, 1), numDoses)        
        }
    } else if (INR > 3.5 & INR <= 4.0) {
        adjDose = c(rep(0, sample(c(0,1), 1)))  # hold 0 or 1 dose randomly
        tmpDoses = possibleDoses[possibleDoses <= dose[day]]
        if (length(tmpDoses) == 1){
            adjDose = c(adjDose, rep(tmpDoses, (numDoses - length(adjDose))))
        } else {
            adjDose = c(adjDose, rep(sample(tmpDoses, 1), (numDoses - length(adjDose))))
        }
    } else {
        adjDose = c(rep(0, sample(c(0,1,2), 1))) # hold 0 - 2 doses randomly
        tmpDoses = possibleDoses[possibleDoses <= dose[day]]
        if (length(tmpDoses) == 1){
            adjDose = c(adjDose, rep(tmpDoses, (numDoses - length(adjDose))))
        } else {
            adjDose = c(adjDose, rep(sample(tmpDoses, 1), (numDoses - length(adjDose))))
        }        
    }
    return(adjDose) 
}

# adjusts the dose by X% (not based on any protocol)
# Not updated and assumedly unused

adjustDoseByPercent = function(INR, percent = 0.10, maxDose = 10) {
    if (!is.nan(INR)) {
         
        # adjusting dose by X% - most simplistic case
        if (INR < 2) {
            dose = dose * (1 + percent)
        }
        else if (INR > 3) {
            dose = dose * (1 - percent)
        } 
        else {
            dose = dose # do nothing  
        }
        
        # the max dose
        if (dose > maxDose) {
            dose = maxDose
        }
    }
  
    return(dose)
}

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
  # }else if (INRs.3days <- tail(INRS, 3) > 5){
		  # dose[day:(day + 3)]= 0
	  # }
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
     
    if(INRs[day-3] > 5){
      if(INR > 5){
        return(ahc.adjustment.2.5(dose, INR, day+1))
      }else if(INR > 3 & INR <= 5){
        return(ahc.adjustment.3(dose, INR, day+1))      
      }else{
        dose[(day+1):(day + 5)] = previous_dose * 0.85
        return(dose[1:maxDays])
      }    
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
     
    if(INRs[day-3] > 5){
      if(INR > 5){
        return(ahc.adjustment.3(dose, INR, day+1))
      }else if(INR > 3.5 & INR <= 5){
        return(ahc.adjustment.3(dose, INR, day+1))
      }else{
        dose[(day+1):(day + 5)] = previous_dose * 0.85
        return(dose[1:maxDays])
      }    
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
}   }

##############################################
########## EU_PACT_INTERMT Protocol ##########
######## This version of EU_PACT uses ######## 
###### Intermountain as the Maintenance ######
############## Dosing Algorithm ##############
##############################################
# Lenzini protocol (EU_PACT version)
lenzini_eu_pact <- function(Dose4, Dose3, Dose2, INR, BSA, AGE, VKORC1G, AMI, TINR, CYP2C9){
  # CYP2C9_2  CYP2C9_3  Genotype
  #   C/C       A/A      *1/*1
  #   C/T       A/A      *1/*2
  #   C/C       A/C      *1/*3
  #   T/T       A/A      *2/*2
  #   C/T       A/C      *2/*3
  #   C/C       C/C      *3/*3
  Amiodarone_Use <- ifelse(AMI == "Y", 1 , 0)
 
  if(CYP2C9=="*1/*1"){
  CYP2C9_2 <- "C/C"       
  CYP2C9_3 <- "A/A"
  }else if(CYP2C9=="*1/*2"){
  CYP2C9_2 <- "C/T"       
  CYP2C9_3 <- "A/A"
  }else if(CYP2C9=="*1/*3"){
  CYP2C9_2 <- "C/C"       
  CYP2C9_3 <- "A/C"
  }else if(CYP2C9=="*2/*2"){
  CYP2C9_2 <- "T/T"
  CYP2C9_3 <- "A/A"
  }else if(CYP2C9=="*2/*3"){
  CYP2C9_2 <- "C/T"
  CYP2C9_3 <- "A/C"
  }else if(CYP2C9=="*3/*3"){
  CYP2C9_2 <- "C/C"       
  CYP2C9_3 <- "C/C"
  }
  
  CYP2C9_2 <- ifelse(CYP2C9_2 == "T/T", 2, ifelse(CYP2C9_2 == "C/T", 1, 0))
  CYP2C9_3 <- ifelse(CYP2C9_3 == "C/C", 2, ifelse(CYP2C9_3 == "A/C", 1, 0))
  
  VKORC1_1639_G_A <- ifelse(VKORC1G == "G/G", 0, ifelse(VKORC1G == "G/A", 1, 2))
  Age_in_years <- AGE
  BSA <- BSA
  Target_INR <- ifelse(TINR == "low", 2.5, 3)
  
  doseLenz <- exp(3.10894 - 0.00767 * Age_in_years - 0.51611 * log(INR) - 0.23032 * VKORC1_1639_G_A - 0.14745 * CYP2C9_2 - 0.3077 * CYP2C9_3 + 0.24597 * BSA + 0.26729 * Target_INR - 0.1035 * Amiodarone_Use + 0.0169 * Dose2 + 0.02018 * Dose3 + 0.01065 * Dose4)
  
  return(doseLenz/7)
}

eu_pact_intermt_protocol = function(avatar, simulation) {
  attach(avatar)
  maxDose = simulation$max_dose

  function(INR, dose, day, check) {
		
	maxDays <- length(dose)
	
#Today's INR is considered tomorrow's pre-dose INR. Accordingly, we set tomorrow's dose based on today's INR.
  
	if(day == 1){
    D<-round(((5.6044 
               -0.02614*AGE
               +0.0087*2.54*HEIGHT#HEIGHT is by cm.
               +0.0128*0.454*WEIGHT#WEIGHT is by kg.
               -0.8677*(VKORC1G=="G/A")#rs9923231
               -1.6974*(VKORC1G=="A/A")
               -0.5211*(CYP2C9=="*1/*2")
               -0.9357*(CYP2C9=="*1/*3")
               -1.0616*(CYP2C9=="*2/*2") 
               -1.9206*(CYP2C9=="*2/*3")
               -2.3312*(CYP2C9=="*3/*3")
               -0.5503*(AMI=="Y"))^2)/7,2) 
 
	    dose[2] <- (dose[1]-D)*(2/3)+D
     dose[3] <- (dose[1]-D)*(1/3)+D
	    #dose[(day+1):(day+6)] <- dose[1]
     return(dose)
	} else if(day == 3){#Dose_4 is zero
	  dose[4:5] <- lenzini_eu_pact(Dose4 = 0, Dose3 = dose[2], Dose2 = dose[3], INR, BSA, AGE, VKORC1G, AMI, TINR, CYP2C9)
	  return(dose)
	} else if(day >= 5){
	  return(intermt(INR, dose, day, check, maxDose = 10))
	} else {
	  stop("Something is wrong in eu_pact...check what you did!")
	}			
}	}
  
##############################################
############ EU_PACT_AHC Protocol ############
######## This version of EU_PACT uses ######## 
########### AHC as the Maintenance ###########
############## Dosing Algorithm ##############
##############################################
# Lenzini protocol (EU_PACT version) is the 
# same as the one for EU_PACT_INTERMT (ABOVE).

eu_pact_ahc_protocol = function(avatar, simulation) {
  attach(avatar)
  maxDose = simulation$max_dose

  function(INR, dose, day, check){

	maxDays <- length(dose)
	
#Today's INR is considered tomorrow's pre-dose INR. Accordingly, we set tomorrow's dose based on today's INR.
  
	if(day == 1){
    D<-round(((5.6044 
               -0.02614*AGE
               +0.0087*2.54*HEIGHT#HEIGHT is by cm.
               +0.0128*0.454*WEIGHT#WEIGHT is by kg.
               -0.8677*(VKORC1G=="G/A")#rs9923231
               -1.6974*(VKORC1G=="A/A")
               -0.5211*(CYP2C9=="*1/*2")
               -0.9357*(CYP2C9=="*1/*3")
               -1.0616*(CYP2C9=="*2/*2") 
               -1.9206*(CYP2C9=="*2/*3")
               -2.3312*(CYP2C9=="*3/*3")
               -0.5503*(AMI=="Y"))^2)/7,2) 
 
	    dose[2] <- (dose[1]-D)*(2/3)+D
     dose[3] <- (dose[1]-D)*(1/3)+D
	    #dose[(day+1):(day+6)] <- dose[1]
     return(dose)
	} else if(day == 3){#Dose_4 is zero
	  dose[4:5] <- lenzini_eu_pact(Dose4 = 0, Dose3 = dose[2], Dose2 = dose[3], INR, BSA, AGE, VKORC1G, AMI, TINR, CYP2C9)
	  return(dose)
	} else if(day >= 5){
	  return(ahc_clinical_protocol(INR, dose, day, check))
	} else {
	  stop("Something is wrong in eu_pact...check what you did!")
	}			
}	}

# returns a list of protocols
list_protocols = function() ( ls(pattern="*_protocol" all.names="T"))

# run protocol based on simulation param
get_protocol = function(avatar, simulation) {
  get(paste(simulation$protocol, "protocol", sep="_"))(avatar, simulation)
}

