#
# Hamberg INR Simulation Model
#
# Prasad Patil & Vincent Fusaro
#
# Description: 
#	This function uses a patient's Age and CYP2C9 and 
#		VKORC1 genotypes to predict a series of INR readings.
#
# Input:
#	dose:		Patient's wafarin dose (float; currently unused)
#	AGE:		Patient's age (integer)
#	CYP2C9:	Patient's CYP2C9 genotype (string controlled dictionary)
#				{*1/*1, *1/*2, *1/*3, *2/*2, *2/*3, *3/*3}
#	VKORC1:	Patient's VKORC1 genotype (string controlled dictionary)
#				{G/G, G/A, A/A} 
#	SS:		Patient's steady state status (integer controlled dictionary)
#				{0,1}
#	times:	Times to "measure" INR (float vector of hours)
#	rseed:	Random seed (integer; used for consistency in testing)
#
# Output:
#	INR:		Vector length of "times" input with INR values (float vector)
#
# Notes: 
#	-Currently loads deSolve package in function; might want to remove this
#		and make it a library dependency
#	-Currently includes * in CYP2C9 genotype (e.g. *1/*1). Must make sure
#		that input is in this format as well.
#	-RSE% = SD / mean * 100, so when we solve for SD we get: SD = RSE / 100 * mean


hamberg_2007 <- function(dose, Cs_super, AGE, CYP2C9, VKORC1, SS, maxTime=24, rseed=12345) {

	library(deSolve)
	
    # if (CYP2C9 == "*1/*1") {
    #     rseed = 123805  # *1/*1 & G/G
    # } else if (CYP2C9 == "*3/*3") {
    #     rseed = 123790  # *3/*3 & G/G
    # }

    set.seed(rseed)
	
	parameters = data.frame("MTT_1"=NA, "MTT_2"=NA, "gamma"=NA, "EC_50"=NA, "cyp_1_1"=NA, "V1"=NA, "V2"=NA, "Q"=NA, "C_s_error"=NA, "lambda"=NA, "e_INR"=NA)
	
	times = c(0, 1:maxTime)	# prepend time 0 to the list of times for deSolve initial conditions (remove when returning list of times)
	                        # times also equals the time-step for deSolve
    
    ############### Random Univariate Parameters ###############                    
	# based on suggestion from Marc Gastonguay from Metrum
	MTT_1 = rlnormRestricted(log(11.6), sqrt(0.141))		# hours
	parameters$MTT_1 = MTT_1

	MTT_2 = rlnormRestricted(log(120), sqrt(1.02))     		# hours
	parameters$MTT_2 = MTT_2

	gamma = 0.424	# no units
    parameters$gamma = gamma

    # EC_50 in mg/L
	if(VKORC1 == "G/G"){ # Order of genotypes changed
		EC_50 = rlnormRestricted(log(4.61), sqrt(0.409))
	} else if((VKORC1 == "G/A") | (VKORC1 == "A/G")){
		EC_50 = rlnormRestricted(log(3.02), sqrt(0.409))    #rnorm(1, 3.02, 0.409*3.02) # not used in japanese paper
	} else if (VKORC1 == "A/A") {
		EC_50 = rlnormRestricted(log(2.20), sqrt(0.409))    #rnorm(1, 2.20, 0.409*2.20) # not used in japanese paper
	} else {
	    stop("ERROR IN HAMBERG_2007.R: the VKORC1 genotype is not supported!")
	}
    parameters$EC_50 = EC_50

	cyp_1_1 = rlnormRestricted(log(0.314), sqrt(0.31))
    parameters$cyp_1_1 = cyp_1_1

	V1 = rlnormRestricted(log(13.8), sqrt(0.262)) # volume in central compartment (L)
	parameters$V1 = V1

	V2 = rlnormRestricted(log(6.59), sqrt(0.991))  # volume in peripheral compartment (L)
	parameters$V2 = V2

	Q = 0.131    # (L/h)
	parameters$Q = Q

	C_s_error = exp(rnorm(1, 0, 0.09))
    parameters$C_s_error = C_s_error

    lambda = 3.61
	parameters$lambda = lambda

	e_INR = rnorm(1,0, 0.0325)
	parameters$e_INR = e_INR


    ###### OLD CODE #######
    # MTT_1 = rnorm(1, 11.6, 11.6*.0465)        # hours
    # parameters$MTT_1 = MTT_1
    # 
    # MTT_2 = rnorm(1, 120, 120*.23)        # hours
    # parameters$MTT_2 = MTT_2
    #   
    # gamma = rnorm(1, 0.424, 0.424*0.124)  # no units
    #         parameters$gamma = gamma
    #     
    #         # EC_50 in mg/L
    # if(VKORC1 == "G/G"){ # Order of genotypes changed
    #   EC_50 = rnorm(1, 4.61, 4.61*0.414)
    # } else if(VKORC1 == "G/A"){
    #   EC_50 = rnorm(1, 3.02, 3.02*0.374)
    # } else {
    #   EC_50 = rnorm(1, 2.20, 2.20*0.386)
    # }
    #         parameters$EC_50 = EC_50
    #   
    # cyp_1_1 = rnorm(1,0.314,0.314*0.0395)
    #         parameters$cyp_1_1 = cyp_1_1
    #     
    # V1 = rnorm(1, 13.8, 13.8*0.0364) # volume in central compartment (L)
    # parameters$V1 = V1
    #   
    # V2 = rnorm(1, 6.59, 6.59*0.241)  # volume in peripheral compartment (L)
    # parameters$V2 = V2
    #   
    # Q = rnorm(1, 0.131, 0.131*0.163)    # (L/h)
    # parameters$Q = Q
    #   
    # C_s_error = exp(rnorm(1, 0.301, 0.301*0.0993))
    #         parameters$C_s_error = C_s_error
    #     
    #         lambda = rnorm(1,3.61,3.61*0.0922)
    # parameters$lambda = lambda
    #   
    # e_INR = rnorm(1,0.0325,0.0325*0.112)
    # parameters$e_INR = e_INR
    ###########################################################
    
	ktr1 = 6/MTT_1					# 1/hours; changed from 1/MTT_1
	ktr2 = 1/MTT_2					# 1/hours
	E_MAX = 1						# no units
	
	CL_s = 1
	if(AGE > 71) { 
		#CL_s = (1-(rnorm(1,0.0091,0.0091*0.289)*(AGE-71)))
		CL_s = (1 - (0.0091 * (AGE-71)))  
	}
	
	if(CYP2C9 == "*1/*1"){
		CL_s = CL_s*cyp_1_1
	} else if(CYP2C9 == "*1/*2"){
		CL_s = CL_s*(1-0.315)*cyp_1_1
	} else if(CYP2C9 == "*1/*3"){
		CL_s = CL_s*(1-0.453)*cyp_1_1
	} else if(CYP2C9 == "*2/*2"){
		CL_s = CL_s*(1-0.722)*cyp_1_1
	} else if(CYP2C9 == "*2/*3"){
		CL_s = CL_s*(1-0.69)*cyp_1_1
	} else if(CYP2C9 == "*3/*3") {
		CL_s = CL_s*(1-0.852)*cyp_1_1
	} else {
	    stop("ERROR IN HAMBERG_2007.R: CYP2C9 genotype not recognized fool!")
	}
	
 	F = 0.9 # bioavilability fraction 0-1 (from: "Applied Pharmacokinetics & Pharmacodynamics 4th edition, p.717", some other references)

	ka = 2 # absorption rate (1/hr)
	
    k12 = Q / V1
    k21 = Q / V2
    k10 = CL_s / V1
    b = k10 + k21 + k12
    c = k10 * k21
    alpha = (b + sqrt(b^2 -4*c)) / 2
    beta = (b - sqrt(b^2 - 4*c)) / 2
    
	# 1-compartment model 
	# we divide the dose by 2 because 50% = S-warfarin and 50% = R-warfarin
	#C_s = (((F * ka * dose / 2) / (V1 * (ka - CL_s))) * (exp(-CL_s * times) - exp(-ka * times)))
    
    # 2-compartment model
    C_s_pred = ((ka * F * dose / 2) / V1) * (   (((k21 - alpha) / ((ka - alpha)*(beta - alpha))) * exp(-alpha * times)) + (((k21 - beta) / ((ka - beta)*(alpha - beta))) * exp(-beta * times)) + (((k21 - ka) / ((ka - alpha)*(ka - beta))) * exp(-ka * times))    )
    C_s = C_s_error * C_s_pred
    
    # plot to look at C_s values
    # ry=range(c(C_s, C_s2))
    # plot(C_s2, ylim=ry, type="l", col="red", lwd=2)
    # points(C_s, type="l", col="blue", lwd=2)
    # legend("topright", c("1-compartment","2-compartment"), col=c("blue", "red"), bty="n", bg="white",lty=c(1,1), lwd=2)
    
    #debug
    #cat(c("C_s:", "\n", C_s, "\n"))
	#cat(c("Cs_super:", "\n", Cs_super, "\n"))
	#cat(c("Cs_pred:", "\n", C_s_pred, "\n"))
	
	p <- c("ktr1" = ktr1, "ktr2" = ktr2, "E_MAX" = E_MAX, "C_s_gamma" = (C_s[2] + Cs_super)^gamma, 
	 	 "EC_50_gamma" = (EC_50)^gamma)

    #debug
    #cat(c("p vector:", "\n", p, "\n"))
    
    # initial values taken directly from Hamberg paper
	val = c("A1" = 1, "A2" = 1, "A3" = 1, "A4" = 1, "A5" = 1, "A6" = 1, "A7" = 1)

	hamberg_ode <- function(t,val,p) { 
		dA1 = p["ktr1"]*(1 - ((p["E_MAX"] * p["C_s_gamma"])/(p["EC_50_gamma"] + p["C_s_gamma"]))) - p["ktr1"]*val["A1"]
		dA2 = p["ktr1"]*val["A1"] - p["ktr1"]*val["A2"]
		dA3 = p["ktr1"]*val["A2"] - p["ktr1"]*val["A3"]
		dA4 = p["ktr1"]*val["A3"] - p["ktr1"]*val["A4"]
		dA5 = p["ktr1"]*val["A4"] - p["ktr1"]*val["A5"]
		dA6 = p["ktr1"]*val["A5"] - p["ktr1"]*val["A6"]
		dA7 = p["ktr2"]*(1 - ((p["E_MAX"] * p["C_s_gamma"])/(p["EC_50_gamma"] + p["C_s_gamma"]))) - p["ktr2"]*val["A7"]
		list(c(dA1, dA2, dA3, dA4, dA5, dA6, dA7))
	}

	out = lsoda(val, times, hamberg_ode, p)
	A6 = out[,7][length(times)]
	A7 = out[,8][length(times)]
    
    A6v = out[,7]
    A7v = out[,8]
    
	INR_max = 20

	# debug
	# cat(c("e_INR:", e_INR, "\n"))
	# cat(c("A6:", A6, "\n"))
	# cat(c("A7:", A7, "\n"))
	# cat(c("EC_50:", EC_50, "\n"))
	# cat(c("lambda:", lambda, "\n"))
	# cat(c("gamma:", gamma, "\n"))
	# cat(c("p:", p, "\n"))
	# cat(c("diff eq. out:", out, "\n"))

	# old incorrect formula - retained for reference just in case...
	#INR = lastINR + (INR_max*(1-A6*A7)^lambda) + exp(e_INR)
    
    baseINR = 1
    INR = (baseINR + (INR_max*(1-A6*A7)^lambda)) * exp(e_INR)
	INRv = (baseINR + (INR_max*(1-A6v*A7v)^lambda)) * exp(e_INR)
	
	#debug
	#cat(c("INR:", "\n", INR, "\n"))
		
	list("INR" = INR, "Cs" = C_s, "out" = out, "INRv" = INRv, "parameters" = parameters)
}

# assumes the meanVal and stdev are appropriately transformed
rlnormRestricted = function(meanVal, stdev) {
    quartileRange = c(0.25, 0.75)   # capture 50% of the data.  This restricts the log values to a "reasonable" range
    qValues = qlnorm(quartileRange, meanVal, stdev)
    values = rlnorm(1000, meanVal, stdev)
    values = values[(values > qValues[1]) & (values < qValues[2])]
    value = sample(values, 1)
    return(value)
}
