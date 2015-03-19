# clinical trial simulation using CA and INR dose response

source("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/hamberg_2007.R")

# setup the validation script
table5 = F
fig3 = F
Cs = T

# wrapper function to reproduce the table from 2007 hamberg paper
hambergWrapper <- function(numPeople, dose = 10, AGE = 71, CYP2C9 = "*1/*1", VKORC1 = "G/G", SS = 1) {
	cat("Procssing stuff...\n")
	
	numDaysToSimulate = 60
	INRs = matrix(NA, nrow=numDaysToSimulate, ncol=numPeople)
    maxTime = 24    # hours
    Cs_rows = maxTime*numDaysToSimulate+1
    Cs = matrix(0, nrow = Cs_rows, ncol=numDaysToSimulate)
    Cs_out = array(0, dim=c(Cs_rows, numDaysToSimulate, numPeople))
    Cs_super = 0
    
	for (p in 1:numPeople) {	
	    rseed = as.numeric(paste(123*p, sep=""))   # rseed upper limit is 1e15 but that's pretty large
	    for (i in 1:numDaysToSimulate) {	
    		x = hamberg_2007(dose, Cs_super, AGE, CYP2C9, VKORC1, SS, maxTime*numDaysToSimulate, rseed)
    		INRs[i, p] = x$INRv[i*24+1]
    		Cs[((i-1)*24 + 1):Cs_rows, i] = x$Cs[1:length(((i-1)*24 + 1):Cs_rows)]
            Cs_super = apply(Cs, 1, sum)[i*24+2]    # add 2 because there are two zero based times from the previous day and the next day
        }
        Cs_out[,,p] = Cs
	}
	
	list("INR" = INRs[numDaysToSimulate,], "INRv" = INRs, "Cs" = Cs_out)
}

###################################################################################################
#
# Cs plot, Hamberg-2007
#
###################################################################################################

if (Cs) {
    numPeople = 1
    x=hambergWrapper(numPeople, 10, 57, "*3/*3")
    ry = range(x$Cs[1:72,1,])
    plot(x$Cs[1:72,1,1], type="n", xlab="hours", ylab="S-warfarin Concentration (mg/L)", ylim=ry)
    for (i in 1:numPeople) {
        points(x=1:72, y=x$Cs[1:72,1,i], type="l", col="lightgray")
    }
    points(x=1:72, y=apply(x$Cs[1:72,1,],1,mean), type="l", col="black", lwd=2)
}

###################################################################################################
#
# Generating Table 5, Hamberg-2007
#
###################################################################################################

if (table5) {
    # Hamberg validation from their paper - given the specified dose can we predict an appropriate INR
    numPeople = 100
    x=hambergWrapper(numPeople, 9.08, 50, "*1/*1", "G/G", 1); hamVal = x$INR
    x=hambergWrapper(numPeople, 6.22, 50, "*1/*2", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 5.04, 50, "*1/*3", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.54, 50, "*2/*2", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.82, 50, "*2/*3", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.38, 50, "*3/*3", "G/G", 1); hamVal = rbind(hamVal, x$INR)

    x=hambergWrapper(numPeople, 7.72, 70, "*1/*1", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 5.3, 70, "*1/*2", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 4.3, 70, "*1/*3", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.16, 70, "*2/*2", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.4, 70, "*2/*3", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.18, 70, "*3/*3", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    
    x=hambergWrapper(numPeople, 6.36, 90, "*1/*1", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 4.4, 90, "*1/*2", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 3.58, 90, "*1/*3", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.77, 90, "*2/*2", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.97, 90, "*2/*3", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.0, 90, "*3/*3", "G/G", 1); hamVal = rbind(hamVal, x$INR)
    
    #####
    x=hambergWrapper(numPeople, 5.94, 50, "*1/*1", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 4.1, 50, "*1/*2", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 3.3, 50, "*1/*3", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.66, 50, "*2/*2", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.85, 50, "*2/*3", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 0.9, 50, "*3/*3", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    
    x=hambergWrapper(numPeople, 5.06, 70, "*1/*1", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 3.5, 70, "*1/*2", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.82, 70, "*1/*3", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.41, 70, "*2/*2", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.57, 70, "*2/*3", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 0.77, 70, "*3/*3", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    
    x=hambergWrapper(numPeople, 4.16, 90, "*1/*1", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.9, 90, "*1/*2", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.34, 90, "*1/*3", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.16, 90, "*2/*2", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.29, 90, "*2/*3", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 0.65, 90, "*3/*3", "G/A", 1); hamVal = rbind(hamVal, x$INR)
    
    #####
    x=hambergWrapper(numPeople, 4.32, 50, "*1/*1", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.98, 50, "*1/*2", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.4, 50, "*1/*3", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.21, 50, "*2/*2", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.35, 50, "*2/*3", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 0.66, 50, "*3/*3", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    
    x=hambergWrapper(numPeople, 3.68, 70, "*1/*1", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.54, 70, "*1/*2", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.06, 70, "*1/*3", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.03, 70, "*2/*2", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.14, 70, "*2/*3", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 0.56, 70, "*3/*3", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    
    x=hambergWrapper(numPeople, 3.04, 90, "*1/*1", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 2.10, 90, "*1/*2", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 1.71, 90, "*1/*3", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 0.84, 90, "*2/*2", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 0.94, 90, "*2/*3", "A/A", 1); hamVal = rbind(hamVal, x$INR)
    x=hambergWrapper(numPeople, 0.47, 90, "*3/*3", "A/A", 1); hamVal = rbind(hamVal, x$INR)

    # range plot
    # rx = range(hamVal, na.rm=T)
    # plot(x=range(hamVal[1,], na.rm=T), y=c(1,1), xlim=rx, ylim=c(1,54), type="l", xlab="INR", ylab="Combinations CYP2C9, VKORC1, Age")
    # points(x=mean(hamVal[1,], na.rm=T), y=1, pch=19, col="red")
    # for (i in 2:54) {
    #   points(x=range(hamVal[i,], na.rm=T), y=c(i,i), type="l")
    #   points(x=mean(hamVal[i,], na.rm=T), y=i, pch=19, col="red")
    # }
    # abline(v=2.5, lty=2, col="blue")

    # points/density plots
    quartz()
    rx = range(hamVal, na.rm=T)
    plot(x=hamVal[1,], y=rep(1,numPeople), xlim=rx, ylim=c(1,54), type="n", pch="|", cex=0.4, xlab="INR", ylab="Combinations CYP2C9, VKORC1, Age")
    for (i in 1:54) {
        points(x=hamVal[i,], y=rep(i,numPeople), xlim=rx, ylim=c(1,54), type="p", pch="|", cex=0.4)
        points(x=median(hamVal[i,], na.rm=T), y=i, pch=19, col="red", cex=0.7)
    }
    abline(v=2.5, lty=2, col="blue")
    
}


###################################################################################################
#
# Generating plots from Figure 3, Hamberg-2007
#
###################################################################################################
if (fig3) {
    numDaysToSimulate = 60
    maxTime = 24
    randSeed = 123456
    nRand = 1
    ################################### CYP2C9
    avatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/INR/INR_Models/Hamberg_2007_CYP2C9_Validation_Avatars.txt", header=T)

    INRs = matrix(0, nrow=numDaysToSimulate, ncol=nrow(avatars))
    
    for (a in 1:nrow(avatars)) {
        x = hambergWrapper(1, avatars$DOSE_GAGE[a], avatars$Age[a], as.character(avatars$CYP2C9[a]), as.character(avatars$VKORC1..1639.[a]), 1)
        INRs[, a] = x$INRv
    }
    

    ##### plot #####
    quartz()
    colors=c("blue", "red", "orange", "green", "purple", "black")
    #ry = range(INRs, na.rm=T)
    ry = c(0,10)
    x = 1:nrow(INRs)
    par(mai=c(1, 0.5, 0.5, 1), mfrow=c(2,2))
    plot(x=x, y=INRs[,1], ylim=c(0, 10), type="n", xlab="Days", ylab="INR", main="INR")
    for (i in 1:nrow(avatars)) {
        points(x=x, y=INRs[,i], type="l", pch=19, col=colors[i], lwd = 2)
        mtext(avatars$CYP2C9[i], side=4, at=INRs[nrow(INRs), i], las=1, line=1, cex=0.8)
    }

    ################################### VKORC1
    avatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/INR/INR_Models/Hamberg_2007_VKORC1_Validation_Avatars.txt", header=T)

    INRs = matrix(0, nrow=numDaysToSimulate, ncol=nrow(avatars))
    
    for (a in 1:nrow(avatars)) {
        x = hambergWrapper(1, avatars$DOSE_GAGE[a], avatars$Age[a], as.character(avatars$CYP2C9[a]), as.character(avatars$VKORC1..1639.[a]), 1)
        INRs[, a] = x$INRv
    }


    ##### plot #####
    colors=c("blue", "red", "orange", "green", "purple", "black")
    #ry = range(INRs, na.rm=T)
    x = (1:nrow(INRs))     # convert to days
    plot(x=x, y=INRs[,1], ylim=ry, type="n", xlab="Days", ylab="INR", main="INR")
    for (i in 1:nrow(avatars)) {
        points(x=x, y=INRs[,i], type="l", pch=19, col=colors[i], lwd = 2)
        mtext(avatars$VKORC1..1639.[i], side=4, at=INRs[nrow(INRs), i], las=1, line=1, cex=0.8)
    }

    ################################### CYP2C9 + VKORC1
    avatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/INR/INR_Models/Hamberg_2007_CYP2C9-VKORC1_Validation_Avatars.txt", header=T)

    INRs = matrix(0, nrow=numDaysToSimulate, ncol=nrow(avatars))
    
    for (a in 1:nrow(avatars)) {
        x = hambergWrapper(1, avatars$DOSE_GAGE[a], avatars$Age[a], as.character(avatars$CYP2C9[a]), as.character(avatars$VKORC1..1639.[a]), 1)
        INRs[, a] = x$INRv
    }

    ##### plot #####
    colors=c("blue", "red", "orange", "green", "purple", "black")
    #ry = range(INRs, na.rm=T)
    x = (1:nrow(INRs))     # convert to days
    plot(x=x, y=INRs[,1], ylim=ry, type="n", xlab="Days", ylab="INR", main="INR")
    for (i in 1:nrow(avatars)) {
        points(x=x, y=INRs[,i], type="l", pch=19, col=colors[i], lwd = 2)
        mtext(paste(avatars$CYP2C9[i], avatars$VKORC1..1639.[i], sep=", "), side=4, at=INRs[nrow(INRs), i], las=1, line=1, cex=0.8)
    }

    ################################### AGES
    avatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/INR/INR_Models/Hamberg_2007_AGES_Validation_Avatars.txt", header=T)

    INRs = matrix(0, nrow=numDaysToSimulate, ncol=nrow(avatars))
    
    for (a in 1:nrow(avatars)) {
        x = hambergWrapper(1, avatars$DOSE_GAGE[a], avatars$Age[a], as.character(avatars$CYP2C9[a]), as.character(avatars$VKORC1..1639.[a]), 1)
        INRs[, a] = x$INRv
    }

    ##### plot #####
    colors=c("blue", "red", "orange", "green", "purple", "black")
    #ry = range(INRs, na.rm=T)
    x = (1:nrow(INRs))    # convert to days
    plot(x=x, y=INRs[,1], ylim=ry, type="n", xlab="Days", ylab="INR", main="INR")
    for (i in 1:nrow(avatars)) {
        points(x=x, y=INRs[,i], type="l", pch=19, col=colors[i], lwd = 2)
        mtext(avatars$Age[i], side=4, at=INRs[nrow(INRs), i], las=1, line=1, cex=0.8)
    }
}