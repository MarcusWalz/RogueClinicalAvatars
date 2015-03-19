
rawData = c("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/Coumagen_redo_08-22-11/Combined_STD_PGX.RData",
            "/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/Coumagen_redo_08-22-11/Combined_WilsonCSTD_WilsonCPGx_After_2_days_fixed_protocol.RData")

sdAvatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/st_200k_dosed.txt", sep="\t", header=T)
sdAvatars = cbind(sdAvatars, group=rep("STD", nrow(sdAvatars)))
                  
pgAvatars = read.table("/Users/vincentfusaro/Dev/ClinicalAvatars/LPM-INR-Modeling/Datasets/Coumagen_Files/pharm_200k_dosed.txt", sep="\t", header=T)
pgAvatars = cbind(pgAvatars, group=rep("PGx", nrow(pgAvatars)))

avatars = rbind(pgAvatars, sdAvatars)
avatars = cbind(index=1:nrow(avatars), avatars)

load(rawData[1])
#coum = trial.out[,,c(1:5,101001:101005)]
coum = trial.out

load(rawData[2])
#wilson = trial.out[,,c(1:5,101001:101005)]
wilson = trial.out


coum.m = melt(coum[,1:2,])
wilson.m = melt(wilson[,1:2,])

sims = rbind(coum.m, wilson.m)