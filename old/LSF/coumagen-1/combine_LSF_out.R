combine_LSF_out <- function(outdir, numDaysToSimulate, numFiles, fileLength){

	setwd(outdir)
	
	trial_out <- array(NA, dim=c(numDaysToSimulate, 3, numFiles*fileLength))
	inc <- 0	

	for(i in 1:numFiles){
		tmp <- read.table(paste("Coumagen_", i, ".txt", sep = ""), header = T, sep = "\t")
		for(j in 1:fileLength){
			to <- j*numDaysToSimulate  
			from <- to - (numDaysToSimulate - 1)
			trial_out[,,j+inc] <- data.matrix(tmp[from:to,])
		}
		inc <- fileLength*i
	}	
	
	trial_out
}

# notes for later (VF)
# x = array(NA, 90,3,100000)
# x[,,1]=cbind(av_out_array$Dose[,,1], av_out_array$INR[,,1], av_out_array$Check[,,1])
# y = array(NA, dim=c(90,9,200))
# y[,1,1:100] = av_out_array$INR

# njobs = number of LSF jobs from the job array
# ngroups = number of avatars processed within each job array
# njobs * ngroups = total number of avatars
combine_LSF_warfarin_simulator = function(outdir, filePrefix, njobs, ngroups, dim1=90, dim2=1) {
    
    setwd(outdir)
    
    dose = array(NA, dim=c(dim1, dim2, njobs*ngroups))
    inr = dose
    check = dose
    all = array(NA, dim=c(dim1, 3, njobs*ngroups))
    
    #d = c(1,2,25,3,4,5,6,75,10)
    #filePrefix = "Coumagen_pharm_d" or "Coumagen_std_d"
    for (i in 1:dim2) {
        for (k in 1:njobs) {
            load(file=paste(filePrefix, k, ".RData", sep="")) # loads a variable called av_out_array
            to = k * ngroups # number of avatars in each sub file
            from = to - (ngroups - 1)
            dose[,i,from:to] = av_out_array$Dose
            inr[,i,from:to] = av_out_array$INR
            check[,i,from:to] = av_out_array$Check
            all[,1,from:to] = av_out_array$INR
            all[,2,from:to] = av_out_array$Dose
            all[,3,from:to] = av_out_array$Check
        }
    }
    list("dose" = dose, "inr" = inr, "check" = check, "all" = all)
}

trial.out = array(NA, dim=c(90,3,200000))
i = 0
for (a in 1:101000) {
    i = i+1
    trial.out[,,i] = wpgx$all[,,a]
}
for (b in 1:99000) {
    i = i+1
    trial.out[,,i] = wstd$all[,,b]
}
