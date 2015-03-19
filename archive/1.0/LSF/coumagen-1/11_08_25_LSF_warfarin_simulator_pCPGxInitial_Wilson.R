# clinical trial simulation using CA and INR dose response
# To run: bsub -J "200k_trial[1-1000]" -o "out/output.%J.%I" -u prasad_patil@hms.harvard.edu R CMD BATCH --vanilla --slave /home/ptone_a/avatars/LSF_Coumagen_trial_simulator.R output.\LSB_JOBINDEX
# bsub -q shared_2h -J "cpgx[1-1000]" -o "/home/vaf3/avatars/Coumagen/out/output.%J.%I" -R "rusage[mem=200]" -u vfusaro@hms.harvard.edu R CMD BATCH --vanilla --slave 11_08_18_LSF_warfarin_simulator_pCPGx.R simoutput.%J.%I

##################################
###        avatar loop         ###
##################################
processAvatar = function(avatars, protocol, initialDose, numDaysToSimulate, maxDose=15, numReplicates=1, maxTime=24, rseed=1234) {

require("deSolve", lib.loc="/home/vaf3/avatars/R_libs")
source("/home/vaf3/avatars/hamberg_2007.R")
source("/home/vaf3/avatars/protocols.R")

    ####### associate the protocol dose with the avatar group
    avatars = cbind(avatars, "Protocol" = rep(protocol, nrow(avatars)))

    ####### associate the initial dose with the avatar group
    if (initialDose <= 15) {
        avatars = cbind(avatars, "InitialDose" = rep(initialDose, nrow(avatars)))
    } else if (initialDose == 16) {
        avatars = cbind(avatars, "InitialDose" = avatars$DOSE_AND*2)
    } else if (initialDose == 17) {
        avatars = cbind(avatars, "InitialDose" = avatars$DOSE_GAGE)
    } else if (initialDose == 18) {
        avatars = cbind(avatars, "InitialDose" = avatars$DOSE_SCONCE)
    }

    numAvatars = nrow(avatars)

    INR.array = array(NA, dim=c(numDaysToSimulate, numReplicates, numAvatars))
    INR.check.array = array(0, dim=c(numDaysToSimulate, numReplicates, numAvatars))
    dose.array = INR.array
    big_array = c()
    INRerrorDist = array(rnorm(numDaysToSimulate*numAvatars*numReplicates,0,0.1), dim=c(numDaysToSimulate, numReplicates, numAvatars))   # INR error distribution
    
	for(a in 1:numAvatars) {
    		
    		for (r in 1:numReplicates) {
    		    
    		    # configure the random seed
    		    if (is.vector(rseed) & numReplicates == 1) {
    		        if (numReplicates == 1) {
    		            seed = rseed[a]
    		        } else {
    		            seed = rseed[r]
    		        }
    		    } else if (is.matrix(rseed)) {
    		        seed = rseed[a, r]
		        } else {
		            stop("Your random seed isn't working like you think it should!")
		        }
		        set.seed(seed)
        	    rnums = round(abs(rnorm(numDaysToSimulate)*100*numDaysToSimulate)) # just a list of random numbers that will be used as seeds for some protocols

                INRs = array(NA, numDaysToSimulate)
        		INR.check = array(0, numDaysToSimulate)
			    Cs_super_out = array(0, dim=c(maxTime*numDaysToSimulate+1, numReplicates))     # keep track of the dose superpositioning for each avatar
        		doses = INRs
        		INRtimePoints = c(1:numDaysToSimulate) # this is in days
		        INRtimePoints = INRtimePoints * 24
                
			    # set the initial dose
			    doses[1] = avatars$InitialDose[a]
    
		        Cs_rows = maxTime*numDaysToSimulate+1
		        Cs = matrix(0, nrow = Cs_rows, ncol=numDaysToSimulate)
		        Cs_super = 0
		        #cat(c("Simulating avatar:", a, "for replicate:", r, "\n"))

		        for (i in 1:numDaysToSimulate) {
		            pill = dose_cat(doses[i], maxDose)    # converts dose into closest pill form
		            testPatient = hamberg_2007(pill, Cs_super, avatars$AGE[a], as.character(avatars$CYP2C9[a]), as.character(avatars$VKORC1G[a]), 1, maxTime*numDaysToSimulate, seed)
		            INRerror = INRerrorDist[i, r, a]
		            measuredINR = round(testPatient$INRv[i*24+1] + INRerror, digits=1)  # round this to 1 decimal, add 1 b/c time is zero based
	            
		            # keep track of the dose and INR values per person over time
		            if (is.na(measuredINR)) {
		                cat(c("Aw Snap...INR is NA:", "avatar:", avatarID, "day:", i, "\n"))
		                print(testPatient$parameters)
		                doses[i] = NA
		                INRs[i] = NA
		            } else {                
		                Cs[((i-1)*24 + 1):Cs_rows, i] = testPatient$Cs[1:length(((i-1)*24 + 1):Cs_rows)]
		                Cs_super = apply(Cs, 1, sum)[i*24+2]    # add 2 because there are two zero based times from the previous day and the next day
		                
		                # this only sums the previous 8 days in the dose superpositioning to get the concentration
		                # this is justifiable b/c the half life of warfarin is 40 hours and 5 half lives is considered gone so 40*5/24 = 8.33 days
		                # MAYBE THIS SHOULD BE RELATED TO GENOTYPE AND CLS OF WARFARIN (*1/*1 = 30 HR VS *3/*3 = 205 HR) ??
                        # if (i > 8) {
                        #     Cs_super = apply(Cs[,(i-8):i], 1, sum)[i*24+2]
                        # } else {
                        #     Cs_super = apply(Cs, 1, sum)[i*24+2]    # add 2 because there are two zero based times from the previous day and the next day
                        #                       }
    		                            
		                INRs[i] = measuredINR
		                #INRv[, i] = testPatient$INRv
		                # pick a protocol
		                if (is.na(doses[min(i+1, numDaysToSimulate)])) {
		                    INR.check[i] = i  # checked INR on this day (due to looping we are off by one day when we check INRs so 2,4,7 days are clinically 3,5,8 but don't worry it all works out)
                            if (avatars$Protocol[a] == "coumagen_pharm") {
		                        doses = coumagen_pharm(measuredINR, doses, i, maxDose)
		                    } else if (avatars$Protocol[a] == "coumagen_standard") {
		                        doses = coumagen_standard(measuredINR, doses, i, maxDose)
		                    } else if (avatars$Protocol[a] == "wilson") {
		                        doses = wilson(measuredINR, doses, i, maxDose)
		                    } else if (avatars$Protocol[a] == "know_your_patient") {
		                        doses = know_your_patient(measuredINR, doses, i, rnums[i], maxDose) # requires setting a new rseed 
		                    } else if (avatars$Protocol[a] == "know_your_patient_random") {
    		                    doses = know_your_patient_random(measuredINR, doses, i, rnums[i], maxDose) # requires setting a new rseed
    		                } else if (avatars$Protocol[a] == "fixed_dose") {
        		                doses = fixed_dose(doses, i) 
        		            } else if (avatars$Protocol[a] == "wilson_coumagen_pharm") {
        		                doses = wilson_coumagen_pharm(measuredINR, doses, i, maxDose)
        		            } else if (avatars$Protocol[a] == "wilson_coumagen_standard") {
        		                doses = wilson_coumagen_standard(measuredINR, doses, i, maxDose)
        		            } else {
    		                    stop("Hey buddy, you forgot to add the protocol to the simulator - duh...")
    		                }  
			            }
		            }
		    }

		    Cs_super_out[, r] = apply(Cs, 1, sum)
            INR.array[, r, a] = INRs
            dose.array[, r, a] = doses
            INR.check.array[, r, a] = INR.check
	
		    #list("INR" = INR.array, "Dose" = dose.array, "Cs" = Cs_super_out, "INR.check" = INR.check.array)
		    #av_tmp <-  cbind("INR" = INR.array, "Dose" = dose.array, "INR.check" = INR.check.array)
		}
		#big_array <- rbind(big_array, av_tmp)
	}
	#big_array
	#write.table(big_array, paste("Simulation ", date(), ".txt", sep = ""), sep = "\t")
	list("INR"=INR.array, "Dose"=dose.array, "Check"=INR.check.array)
}

############################################
#### LSF ####
avatars = read.table("/home/vaf3/avatars/Coumagen/pharm_200k_dosed.txt", sep="\t", header=T)

av_per = 101	# Number of avatars per file (per run)
block = av_per - 1

av_index = as.numeric(Sys.getenv("LSB_JOBINDEX"))
#av_index = 570

to = av_index*av_per
from = to - block

av_sub = avatars[from:to,]

# need to create a global set of random seeds so when the jobs are distributed the values are random and
# don't repeat with every block
set.seed(4321)
numReplicates = 1
randomValues = array(round(abs(rnorm(nrow(avatars)*numReplicates)*nrow(avatars)*numReplicates)), dim=c(nrow(avatars), numReplicates))
rand_sub = randomValues[from:to,]

# If output is an array
av_out_array = processAvatar(avatars=av_sub, protocol="wilson", initialDose=16, numDaysToSimulate=90, maxDose=15, numReplicates=numReplicates, maxTime=24, rseed=rand_sub)

save(av_out_array, file=paste("Coumagen_pharm_wilson_", av_index, ".RData", sep = ""))





