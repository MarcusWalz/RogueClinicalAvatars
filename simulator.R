
#This function takes the following arguments and simulates a course of anticoagulation therapy.
#avatars: A file includes avatars with assigned initial warfarin doses.
#protocols: A desired dosing protocol for warfarin maintenance dosing.
#initialDose: A relevant code for the dosing algorithm used for initial dosing.
#numDaysToSimulate: A desired number of days for simulation.
#maxDose: The upper limit of a range of warfarin dose.
#numReplicates: Number of replication of a simulation.
#maxTime: The time interval for getting INR and Dose calculations outputs.
#rseed: random seeds.

##################################
###    simulation function     ###
##################################
processAvatar = function(avatars, initialDose, protocol, numDaysToSimulate, maxDose, numReplicates, maxTime=24, rseed) {
###### Note: maxTime arguement is defined based on the adopted PK/PD model. So, since Hamberg PK/PD
###### model is based on a 24-hr time period, we decided to make argument "maxTime" constant "24".
###### So, the regular user of the simulator does not need to change it when calling function "processAvatar".

require(deSolve)
source("hamberg_2007.R")
source("maintenance_protocols.R")
source("initial_dose.R")
source("job.distributor.R")
    
    
    ####### Call function "distribute" in "job.distributor.R"
    # distribute.out <- distribute(avatars, av_per, av_index, numReplicates)
    # rseed <- unlist(distribute.out[2][[1]])
    # avatars <- as.data.frame(distribute.out[1])
    
    ####### associate the protocol dose with the avatar group
    avatars = cbind(avatars, "Protocol" = rep(protocol, nrow(avatars)))

    ####### Call function "initial_dose" in "initial.dosing.library.R" to calculate and assign 
    ####### an initial dose to each avatar using the selected initial dosing algorithm by
    ####### the user of the simulator when function "processAvatar" is called.
    avatars <- initial_dose(avatars, initialDose)
    
    numAvatars = nrow(avatars)
    
    ID.array <- array(NA, dim=c(numDaysToSimulate, numAvatars))

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
		        cat(c("Simulating avatar:", a, "for replicate:", r, "\n"))
          print("hi-1")
          print(r)
          print(a)
		        for (i in 1:numDaysToSimulate) {
              print(paste("Dose: ", doses[i]))
              print(paste("INRs: ", INRs[i]))
		            pill = dose_cat(doses[i], maxDose)    # converts dose into closest pill form              
		            testPatient = hamberg_2007(pill, Cs_super, avatars$AGE[a], as.character(avatars$CYP2C9[a]), as.character(avatars$VKORC1G[a]), 1, maxTime*numDaysToSimulate, seed)
		            INRerror = INRerrorDist[i, r, a]
		            measuredINR = round(testPatient$INRv[i*24+1] + INRerror, digits=1)  # round this to 1 decimal, add 1 b/c time is zero based
              print(i)
		            # keep track of the dose and INR values per person over time
		            if (is.na(measuredINR)) {
		                cat(c("Aw Snap...INR is NA:", "avatar:", avatarID, "day:", i, "\n"))
		                print(testPatient$parameters)
		                doses[i] = NA
		                INRs[i] = NA
		            } else {                
		                Cs[((i-1)*24 + 1):Cs_rows, i] = testPatient$Cs[1:length(((i-1)*24 + 1):Cs_rows)]
		                Cs_super = apply(Cs, 1, sum)[i*24+2]    
                  
		                INRs[i] = measuredINR
		                print("hi")
                  print(paste("INRs _ MeasuredINR: ", INRs[i]))
		                # pick a protocol
		                if (is.na(doses[min(i+1, numDaysToSimulate)])) {
		                    INR.check[i] = i  # checked INR on this day (due to looping we are off by one day when we check INRs so 2,4,7 days are clinically 3,5,8 but don't worry it all works out)
		                    if (avatars$Protocol[a] == "coumagen_pharm") {
		                        doses = coumagen_pharm(measuredINR, doses, i, maxDose)
		                    } else if (avatars$Protocol[a] == "coumagen_standard") {
		                        doses = coumagen_standard(measuredINR, doses, i, maxDose)
		                    } else if (avatars$Protocol[a] == "wilson") {
		                        doses = wilson(measuredINR, doses, i, maxDose)		                    
    		                } else if (avatars$Protocol[a] == "fixed_dose") {
        		                doses = fixed_dose(doses, i) 
        		            } else if (avatars$Protocol[a] == "wilson_coumagen_pharm") {
        		                doses = wilson_coumagen_pharm(measuredINR, doses, i, maxDose)
        		            } else if (avatars$Protocol[a] == "wilson_coumagen_standard") {
        		                doses = wilson_coumagen_standard(measuredINR, doses, i, maxDose)
        		            } else if (avatars$Protocol[a] == "cooper_intermt") {
        		                doses = cooper_intermt(measuredINR, doses, i, maxDose)
        		            } else if (avatars$Protocol[a] == "fen_intermt") {
        		              doses = fen_intermt(measuredINR, doses, i, maxDose)
        		            } else if (avatars$Protocol[a] == "rob_intermt") {
        		              doses = rob_intermt(measuredINR, doses, i, maxDose)
        		            } else if (avatars$Protocol[a] == "gedge_intermt") {
        		              doses = gedge_intermt(measuredINR, doses, i, maxDose)
        		            } else if (avatars$Protocol[a] == "ahc_clinical") {
                        doses = ahc_clinical(as.numeric(as.character(measuredINR)), as.numeric(as.character(INRs[1:i])), as.numeric(as.character(INR.check[1:i])), as.numeric(as.character(doses)), i, maxDose, as.character(avatars$TINR[a]))
                        print("hi2")
                      } else if (avatars$Protocol[a] == "eu_pact_intermt") {#initial and alteration: eu_pact || maintenance: intermountain
                        doses = eu_pact_intermt(measuredINR, doses, i, maxDose, avatars$AGE[a], as.character(avatars$VKORC1G[a]), avatars$BSA[a], avatars$TINR[a], as.character(avatars$AMI[a]), as.character(avatars$CYP2C9[a]), avatars$HEIGHT[a], avatars$WEIGHT[a])
                     } else if (avatars$Protocol[a] == "eu_pact_ahc") {#initial and alteration: eu_pact || maintenance: ahc
                        doses = eu_pact_ahc(measuredINR, doses, i, maxDose, avatars$AGE[a], as.character(avatars$VKORC1G[a]), avatars$BSA[a], avatars$TINR[a], as.character(avatars$AMI[a]), as.character(avatars$CYP2C9[a]), avatars$HEIGHT[a], avatars$WEIGHT[a], as.numeric(as.character(INRs[1:i])), as.numeric(as.character(INR.check[1:i])))
                     } else {
    		                    stop("Hey buddy, you forgot to add the protocol to the simulator - duh...")
    		                }                
			            }
		            }
		    }

		    Cs_super_out[, r] = apply(Cs, 1, sum)
            print(paste("inr array: ", INRs))
            print(paste("dose array: ", doses))
            print(paste("check array: ", INR.check))
            INR.array[, r, a] = INRs
            dose.array[, r, a] = doses
            INR.check.array[, r, a] = INR.check	
            ID.array[, a] <- avatars$ID[a]
		}		
	}
 #return(avatars)
	as.data.frame(list("INR"=INR.array, "Dose"=dose.array, "Check"=INR.check.array, "ID"= ID.array))
  #list("INR"=INR.array, "Dose"=dose.array, "Check"=INR.check.array, "ID"= ID.array)
}










