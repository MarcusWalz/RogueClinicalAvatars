



#  NOT FULLY TESTED AND LIKELY DOES NOT WORK!!!!!!!!
#










#
# Hamberg 2010 INR Simulation Model
#
# Matthew Crawford
#
# Description:
#	This function uses a patient's Age and CYP2C9 and VKORC1 genotypes
#		to predict a series of INR readings.
#
# Input:
#	dose:		Patient's warfarin dose (float)
#	AGE:		Patient's age (integer)
#	CYP2C9:		Patient's CYP2C9 genotype (strin controlled dictionary)
#					{*1/*1, *1/*2, *1/*3, *2/*2, *2/*3, *3/*3}
#	VKORC1:		Patient's VKORC1 genotype (string controlled dictionary)
#					{G/G, G/A, A/A} 
#	times:		Times to "measure" INR (float vector of hours)
#	rseed:		Random seed (integer; used for consistency in testing)
#
# Output:
#	INR:		Vector of length "times" input with INR values (float vector)
#
# Notes: 
#	-Currently loads deSolve package in function; might want to remove this
#		and make it a library dependency
#	-Currently includes * in CYP2C9 genotype (e.g. *1/*1). Must make sure
#		that input is in this format as well.
#	-RSE% = SD / mean * 100, so when we solve for SD we get: SD = RSE / 100 * mean


hamberg_2010 <- function(dose, AGE, CYP2C9, VKORC1, SS, maxTime=24, rseed=45678) {

	library(deSolve)
    set.seed(rseed)

	# EC_50 in mg/l
	if(VKORC1 == "G/G"){
		EC_50 = (2*rnorm(1, 2.05, 2.05*.102))
	} else if(VKORC1 == "G/A"){
		EC_50 = (rnorm(1, 2.05, 2.05*.102)+(rnorm(1, 0.96, .96*.097)))
	} else {
		EC_50 = (2*rnorm(1, 0.96, 0.96*.097))
	}

	# CL in l/h
	# Affect of Age on CL
	CL = 1
	if(AGE != 71) {
		CL = (1-(rnorm(1, .00571, .00571*.578)*(AGE-71)))
	} 

	if(CYP2C9 == "*1/*1"){
		CL = CL*(2*rnorm(1, .174, .174*.0448))
	} else if(CYP2C9 == "*1/*2"){
		CL = CL*(rnorm(1, .174, .174*.0488)+rnorm(1, .0879, .0879*.15))
	} else if(CYP2C9 == "*1/*3"){
		CL = CL*(rnorm(1, .174, .174*.0488)+rnorm(1, .0422, .0422*.46))
	} else if(CYP2C9 == "*2/*2"){
		CL = CL*(2*rnorm(1, .0879, .0879*.15))
	} else if (CYP2C9 == "*2/*3"){
		CL = CL*(rnorm(1, .0879, .0879*.15)+rnorm(1, .0422, .0422*.46))
	} else {
		CL = CL*(2*rnorm(1, .0422, .0422*.46))
	}

	F = 0.9 						# bioavailability fracion (ref: "Applied Pharmacokinetics & Pharmacodynamics 4th edition, p.717", also other references)
	ka = 2 							# absorption rate (1/hr)
	V = rnorm(1, 14.3, 14.3*.0345) 	# apparent volume of distribution
	KDE = CL/V 						# rate constant governing distribution of drug to the site of action
	EDK_50 = CL*EC_50				# dose rate leading to 50% inhibition
	
	times = c(0, 1:maxTime) # prepend time 0 to the list of times for deSolve initial conditions (remove when returning list of times)

	MTT_1 = rnorm(1, 28.6, 28.6*.024) 		# hours
	MTT_2 = 118.3							# hours
	E_MAX = 1								# no units
	gamma = rnorm(1, 1.15, 1.39*.044)		# no units
	
	C = (((F*ka*dose)/(V*(ka-KDE)))*(exp(-KDE*times)-exp(-ka*times))) # S-warfarin Concentration
	DR = (C[2] * KDE)  							                            # Dose Rate
	EFF = (E_MAX * DR)/(EDK_50 + DR) 			                            # Effect Rate

	# This is a one compartment model
	
	p <- c("KDE" = KDE, "C" = C[2], "MTT_1" = MTT_1, "MTT_2" = MTT_2, "EFF" = EFF)
	
	# Initial values for "C1_1", "C2_1" , "C" taken directly from 2010 paper, 
	# assume "C1_2", "C1_3", "C2_2", and "C2_3" to also be set to unity,
	# based on assumptions from 2007 paper
	val <- c("C" = 0, "C1_1" = 1, "C2_1" = 1, "C1_2" = 1, "C1_3" = 1, "C2_2" = 1, "C2_3" = 1)

    cat(c("p:", p, "\n", "val:", val, "\n"))
	hamberg_ode <- function(t, val, p){
	
    	dC = -p["KDE"] * p["C"]
	
    	dC1_1 = ((1 - p["EFF"])*(3/p["MTT_1"])) - (val["C1_1"]*(3/p["MTT_1"]))
    	dC1_2 = (val["C1_1"]*(3/p["MTT_1"]) - (val["C1_2"]*(3/p["MTT_1"])))
    	dC1_3 = (val["C1_2"]*(3/p["MTT_1"]) - (val["C1_3"]*(3/p["MTT_1"])))
	
    	dC2_1 = ((1 - p["EFF"])*(3/p["MTT_2"]) - (val["C2_1"]*(3/p["MTT_2"])))
    	dC2_2 = (val["C2_1"]*(3/p["MTT_2"]) - (val["C2_2"]*(3/p["MTT_2"])))
    	dC2_3 = (val["C2_2"]*(3/p["MTT_2"]) - (val["C2_3"]*(3/p["MTT_2"])))
	    
	    cat(c(dC, dC1_1, dC1_2, dC1_3, dC2_1, dC2_2, dC2_3))
		list(c(dC, dC1_1, dC1_2, dC1_3, dC2_1, dC2_2, dC2_3))
    	
	}

	out <- lsoda(val, times, hamberg_ode, p, verbose=TRUE)
	C1_3 <- out[,6][length(times)]
	C2_3 <- out[,8][length(times)]

    C1_3v = out[,6]
    C2_3v = out[,8]
    
	BASE = 1 #1.03 ?Hamberg used this value in NONMEM code
	INR_max = 20
	
	INR <- BASE + (INR_max*(1-(C1_3 + C2_3)/2))
	INRv <- BASE + (INR_max*(1-(C1_3v + C2_3v)/2))
	
	list("INR" = INR, "C" = C, "out" = out, "INRv" = INRv)
}





