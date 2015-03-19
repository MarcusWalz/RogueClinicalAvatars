
# Clinical trial Time in Therapeutic Range Counts
# assumes vectors of INR and days when you checked INR
ttr = function(INR, DaysChecked, min=2, max=3) {
    
    possibleINRDays = 1:length(INR)
    INRDays = possibleINRDays[DaysChecked]
    
    ttrIn = sum(summary(cut(INR[INRDays[INRDays > 3]], c(0, min, max, Inf)))[2]) / length(INR[INRDays[INRDays > 3]]) * 100
    ttrLow = sum(summary(cut(INR[INRDays[INRDays > 3]], c(0, min, max, Inf)))[1]) / length(INR[INRDays[INRDays > 3]]) * 100
    ttrHigh = sum(summary(cut(INR[INRDays[INRDays > 3]], c(0, min, max, Inf)))[3]) / length(INR[INRDays[INRDays > 3]]) * 100    
    
    list("ttrIn"=ttrIn, "ttrLow"=ttrLow, "ttrHigh"=ttrHigh, "measurements"=length(INRDays))
}

# calculates various risks of events based on INR
# _risk is the median risk for all INR values
# _sampled_risk is the median risk for just the days you checked INR based on the protocol
# _values are all the risk values for each INR
risk = function(INR, DaysChecked = c(1,2,3,5,7)) {
    possibleINRDays = 1:length(INR)
    INRDays = possibleINRDays[DaysChecked]
    
    # ischemic stroke (is) and intracranial hemorrhage (ih) from Amouyel 2009
    is = -0.38 - (3.52 * pmin(INR, rep(2.0, length(INR)))) + (0.68 * pmax(INR, rep(3.0, length(INR))))
    is_event = exp(is) / (1 + exp(is)) * 100
    
    ih = -8.93 + (1.67 * pmax(INR, rep(3.0, length(INR))))
    ih_event = exp(ih) / (1 + exp(ih)) * 100
    
    list("ischemicStroke_risk" = median(is_event), "ischemicStroke_sampled_risk" = median(is_event[INRDays]), "ischemicStroke_values" = is_event,
        "intracranialHemorrhage_risk" = median(ih_event), "intracranialHemorrhage_sampled_risk" = median(ih_event[INRDays]), "intracranialHemorrhage_values" = ih_event)
}

# calculate output metrics from Coumagen paper
# assumes input is in the form of a 3D array with columns INR, dose, INR.check by days by avatar
trialResults = function(trial.out, nsample=10, sampleGroups=c(101,99), sampleOutOf=c(101000, 99000), rseed=54321) {
    pgAvatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/pharm_200k_dosed.txt", sep="\t", header=T)
    stdAvatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/st_200k_dosed.txt", sep="\t", header=T)

    ### assign protocol ###
    protocolList = list("coumagen_pharm" = T, "coumagen_standard" = F)
    pgAvatars = cbind(pgAvatars, "Protocol" = randomizeProtocols(nrow(pgAvatars), protocolList))

    protocolList = list("coumagen_pharm" = F, "coumagen_standard" = T)
    stdAvatars = cbind(stdAvatars, "Protocol" = randomizeProtocols(nrow(stdAvatars), protocolList))

    avatars = rbind(pgAvatars, stdAvatars)

    set.seed(rseed)
    ttrOut = array(NA, dim=c(nsample, 6))
    colnames(ttrOut) = c("meanPG", "sdPG", "meanSTD", "sdSTD", "Ts", "Pval")
    ttrIn = ttrOut
    ttrOutMV = ttrOut
    ttrOutWT = ttrOut
    ttrOutWTMV = ttrOut
    ttrOutSV = ttrOut
    INR.measurements = ttrOut 
    possible.INR.days = 1:nrow(trial.out)
    pg_means = array(NA, dim=c(nsample, 15), dimnames = list(NULL, c("Age", "Weight_m", "Weight_sd", "%Males", "%DVT", "%White", "%Smoker", "CYP2C9*2.C/C", "CYP2C9*2.C/T", "CYP2C9*3.A/A", "CYP2C9*3.A/C", "CYP2C9*3.C/C", "VKORC1.1173.C/C", "VKORC1.1173.C/T", "VKORC1.1173.T/T")))
    std_means = pg_means
    ttrInMatrix = array(NA, dim=c(nsample, sum(sampleGroups)))
    
    for (i in 1:nsample) {
        pgindex = sample(sampleOutOf[1], sampleGroups[1])
        stdindex = sample(sampleOutOf[2], sampleGroups[2]) + sampleOutOf[1]
	    pg_means[i,] = calc_means(avatars[pgindex,])
	    std_means[i,] = calc_means(avatars[stdindex,])
        tempAvatars = avatars[c(pgindex, stdindex),]
        if (nrow(tempAvatars) != sum(sampleGroups)) {
            stop("Hello??? - you can't add your sample groups")
        }
        tempTrial = trial.out[,,c(pgindex, stdindex)]
        INR.freq = numeric(sum(sampleGroups))
        ttrOut.temp = INR.freq
        ttrIn.temp = INR.freq
        ttrOutMV.temp = INR.freq
        ttrOutWT.temp = INR.freq
        ttrOutWTMV.temp = INR.freq
        ttrOutSV.temp = INR.freq
        for (a in 1:nrow(tempAvatars)) {
            INR.days = possible.INR.days[tempTrial[,3,a]]
            INR.freq[a] = length(INR.days)
            ttrOut.temp[a] = sum(summary(cut(tempTrial[INR.days[INR.days > 3], 1, a], c(0, 1.8, 3.2, Inf)))[c(1,3)]) / length(tempTrial[INR.days[INR.days > 3], 1, a]) * 100
            ttrHigh = summary(cut(tempTrial[INR.days[INR.days <= 3], 1, a], c(0, 1.8, 3.2, Inf)))[c(3)]
            if (ttrHigh > 0){
                cat(c("WARNING: you have a high INR value before day 4 for avatar:", c(pgindex,stdindex)[a], "\n"))
            }
            ttrIn.temp[a] = summary(cut(tempTrial[INR.days[INR.days > 3], 1, a], c(0, 1.8, 3.2, Inf)))[2] / length(tempTrial[INR.days[INR.days > 3], 1, a]) * 100
        }
        genotypes = matrix(NA, nrow=nrow(tempAvatars), ncol=3)
        genotypes[tempAvatars$CYP2C92=="C/C",1] = 0
        genotypes[tempAvatars$CYP2C92=="C/T",1] = 1
        genotypes[tempAvatars$CYP2C92=="T/C",1] = 1
        genotypes[tempAvatars$CYP2C92=="T/T",1] = 2
        
        genotypes[tempAvatars$CYP2C93=="A/A",2] = 0
        genotypes[tempAvatars$CYP2C93=="A/C",2] = 1
        genotypes[tempAvatars$CYP2C93=="C/A",2] = 1
        genotypes[tempAvatars$CYP2C93=="C/C",2] = 2
        
        genotypes[tempAvatars$VKORC1A=="C/C",3] = 0
        genotypes[tempAvatars$VKORC1A=="C/T",3] = 1
        genotypes[tempAvatars$VKORC1A=="T/C",3] = 1
        genotypes[tempAvatars$VKORC1A=="T/T",3] = 2
        
        genoSum = apply(genotypes, 1, sum)
        tempAvatars = cbind(tempAvatars, "genoSum"=genoSum)
        
        # multiple variant patients
        avatarIndex1 = which(tempAvatars$genoSum > 1 & tempAvatars$Protocol=="coumagen_pharm")
        avatarIndex2 = which(tempAvatars$genoSum > 1 & tempAvatars$Protocol=="coumagen_standard")
        ttrOutMV.pharm = ttrOut.temp[avatarIndex1]
        ttrOutMV.std = ttrOut.temp[avatarIndex2]
        ttrOutMV[i,] = c(mean(ttrOutMV.pharm), sd(ttrOutMV.pharm), mean(ttrOutMV.std), sd(ttrOutMV.std), NA, NA) #, t.test(ttrOutMV.pharm, ttrOutMV.std)$statistic, t.test(ttrOutMV.pharm, ttrOutMV.std)$p.value)

        # wild type out of range
        avatarIndex1 = which(tempAvatars$genoSum==0 & tempAvatars$Protocol=="coumagen_pharm")
        avatarIndex2 = which(tempAvatars$genoSum==0 & tempAvatars$Protocol=="coumagen_standard")
        ttrOutWT.pharm = ttrOut.temp[avatarIndex1]
        ttrOutWT.std = ttrOut.temp[avatarIndex2]
        ttrOutWT[i,] = c(mean(ttrOutWT.pharm), sd(ttrOutWT.pharm), mean(ttrOutWT.std), sd(ttrOutWT.std), NA, NA) #, t.test(ttrOutWT.pharm, ttrOutWT.std)$statistic, t.test(ttrOutWT.pharm, ttrOutWT.std)$p.value)

        # wild type & multiple variant patients
        avatarIndex1 = which(tempAvatars$genoSum!=1 & tempAvatars$Protocol=="coumagen_pharm")
        avatarIndex2 = which(tempAvatars$genoSum!=1 & tempAvatars$Protocol=="coumagen_standard")
        ttrOutWTMV.pharm = ttrOut.temp[avatarIndex1]
        ttrOutWTMV.std = ttrOut.temp[avatarIndex2]
        ttrOutWTMV[i,] = c(mean(ttrOutWTMV.pharm), sd(ttrOutWTMV.pharm), mean(ttrOutWTMV.std), sd(ttrOutWTMV.std), t.test(ttrOutWTMV.pharm, ttrOutWTMV.std)$statistic, t.test(ttrOutWTMV.pharm, ttrOutWTMV.std)$p.value)

        # single variant patients
        avatarIndex1 = which(tempAvatars$genoSum==1 & tempAvatars$Protocol=="coumagen_pharm")
        avatarIndex2 = which(tempAvatars$genoSum==1 & tempAvatars$Protocol=="coumagen_standard")
        ttrOutSV.pharm = ttrOut.temp[avatarIndex1]
        ttrOutSV.std = ttrOut.temp[avatarIndex2]
        ttrOutSV[i,] = c(mean(ttrOutSV.pharm), sd(ttrOutSV.pharm), mean(ttrOutSV.std), sd(ttrOutSV.std), NA, NA) #, t.test(ttrOutSV.pharm, ttrOutSV.std)$statistic, t.test(ttrOutSV.pharm, ttrOutSV.std)$p.value)
        
        ttrOut[i,] = c(mean(ttrOut.temp[1:sampleGroups[1]]), sd(ttrOut.temp[1:sampleGroups[1]]), mean(ttrOut.temp[(sampleGroups[1]+1):sum(sampleGroups)]), sd(ttrOut.temp[(sampleGroups[1]+1):sum(sampleGroups)]), t.test(ttrOut.temp[1:sampleGroups[1]], ttrOut.temp[sampleGroups[1]+1:sum(sampleGroups)])$statistic, t.test(ttrOut.temp[1:sampleGroups[1]], ttrOut.temp[sampleGroups[1]+1:sum(sampleGroups)])$p.value)
        ttrIn[i,] = c(mean(ttrIn.temp[1:sampleGroups[1]]), sd(ttrIn.temp[1:sampleGroups[1]]), mean(ttrIn.temp[(sampleGroups[1]+1):sum(sampleGroups)]), sd(ttrIn.temp[(sampleGroups[1]+1):sum(sampleGroups)]), t.test(ttrIn.temp[1:sampleGroups[1]], ttrIn.temp[sampleGroups[1]+1:sum(sampleGroups)])$statistic, t.test(ttrIn.temp[1:sampleGroups[1]], ttrIn.temp[sampleGroups[1]+1:sum(sampleGroups)])$p.value)
        INR.measurements[i,] = c(mean(INR.freq[1:sampleGroups[1]]), sd(INR.freq[1:sampleGroups[1]]), mean(INR.freq[(sampleGroups[1]+1):sum(sampleGroups)]), sd(INR.freq[(sampleGroups[1]+1):sum(sampleGroups)]), t.test(INR.freq[1:sampleGroups[1]], INR.freq[sampleGroups[1]+1:sum(sampleGroups)])$statistic, t.test(INR.freq[1:sampleGroups[1]], INR.freq[sampleGroups[1]+1:sum(sampleGroups)])$p.value)
        ttrInMatrix[i,] = ttrIn.temp
    }
    list("tempAvatars"=tempAvatars, "ttrOut"=ttrOut, "ttrOutMV"=ttrOutMV, "ttrOutWT"=ttrOutWT, "ttrOutWTMV"=ttrOutWTMV, "ttrOutSV"=ttrOutSV, "ttrIn"=ttrIn, "ttrInMatrix"= ttrInMatrix, "INR.measurements"=INR.measurements, "pg_trial_means"=pg_means, "std_trial_means"=std_means, "pg_total_mean"=apply(pg_means,2,mean), "std_total_mean"=apply(std_means,2,mean))
}

calc_means <- function(pop){
	Age_m <- mean(pop$AGE)
	Weight_m <- mean(pop$WEIGHT)
	Weight_sd <- sd(pop$WEIGHT)
	Gender_M <- length(which(pop$GENDER == "M"))/length(pop$GENDER)
	DVT_Y <- length(which(pop$DVT == "Y"))/length(pop$DVT)
	Race_Wh <- length(which(pop$RACE == "White"))/length(pop$RACE)
	Smoke_Y <- length(which(pop$SMOKER == "Y"))/length(pop$SMOKER)
	CYP2C9.2 <- summary(pop$CYP2C92)/length(pop$CYP2C92)
	CYP2C9.3 <- summary(pop$CYP2C93)/length(pop$CYP2C93)
	VKORC1.1173 <- summary(pop$VKORC1A)/length(pop$VKORC1A)

	round(c("Age" = Age_m, "Weight_m"=Weight_m, "Weight_sd" = Weight_sd, "%Males" = Gender_M, "%DVT" = DVT_Y, "%White" = Race_Wh, "%Smoker" = Smoke_Y, "CYP2C9*2" = CYP2C9.2, "CYP2C9*3" = CYP2C9.3, "VKORC1.1173" = VKORC1.1173), digits = 3)
	
}

