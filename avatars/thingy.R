
args <- commandArgs(trailingOnly = T)


source('categorical_to_continuous.R')
read.delim(args[1])->orig
#read.delim("..//newjackdata.txt")->newjack
read.delim("BOOT_1_HW_T.txt")->tdata
cutsH<-c(140.37,155.78,171.1872,186.59)
cutsW<-c(71.54,113.08,154.62,196.16)
cutsINT<-c(2.3,2.67,3)

orig$HEIGHT=categorical_to_continuous(training_data = tdata$HEIGHT[tdata$HEIGHT!="Unknown"],cut_points = cutsH,categorical_data = orig$HEIGHT) 
orig$WEIGHT=categorical_to_continuous(training_data = tdata$WEIGHT[tdata$WEIGHT!="Unknown"],cut_points = cutsW,categorical_data = orig$WEIGHT)
orig$T_INR=categorical_to_continuous(training_data = tdata$TINR[tdata$TINR!="Unknown"],cut_points = cutsINT,categorical_data = orig$T_INR)

levels(orig$AGE)
ages=sample(10:100,10000,replace=T)
cutsA=c(10,20,30,40,50,60,70,80,90)
orig$AGE=categorical_to_continuous(training_data = ages,cut_points = cutsA,categorical_data = orig$AGE)

# levels(orig$VKORC1G) <- c("A/A","G/A","G/G")
levels(orig$GENDER) <- c("F", "M")
# sexm<-orig$GENDER=="male"
# orig$GENDER[sexm]="M"
#sexf<-orig$GENDER=="female"
#orig$GENDER[sexf]="F"
#repRace<-orig$RACE=="Missing"
#orig$RACE[repRace]="Unknown"
#repDVTA<-orig$DVT=="A"
#orig$DVT[repDVTA]="N"
#repDVTB<-orig$DVT=="B"
#orig$DVT[repDVTB]="Y"


levels(orig$VALVE_REP) <- c("N", "Y")
levels(orig$AMI) <- c("N", "Y")
levels(orig$DVT) <- c("N", "Y")
levels(orig$TYLENOL) <- c("N", "Y")
levels(orig$ASPRIN) <- c("N", "Y")
levels(orig$DIABETES) <- c("N", "Y")
levels(orig$FLUVASTATIN) <- c("N", "Y")
#repAMIA<-orig$AMI=="A"
#orig$AMI[repAMIA]="N"
#repAMIB<-orig$AMI=="B"
#orig$AMI[repAMIB]="Y"

#repSMOKERA<-orig$SMOKER=="A"
levels(orig$SMOKER) <- c("N", "Y")
#orig$SMOKER[orig$SMOKER == "A"] <- "N"
#orig$SMOKER[orig$SMOKER == "B"] <- "Y"
#repSMOKERB<-orig$SMOKER=="B"
#orig$SMOKER[repSMOKERB]="Y"

levels(orig$ENZYME) <- c("N", "Y")
#repENZYMEA<-orig$ENZYME=="A"
#orig$ENZYME[repENZYMEA]="N"
#repENZYMEB<-orig$ENZYME=="B"
#orig$ENZYME[repENZYMEB]="Y"

write.table(orig, args[2], sep="\t")
#avout=processAvatar(orig, "AHC", "ahc_clinical", 90, 15, 10, maxTime=24, 9774, 1)
##write.table(av_out_array, file=paste("output/ahc_avatars_eupactInitial_eupactAlteration_ahc_algorithm_", av_index,".txt", sep = ""), sep="|", row.names=FALSE)
