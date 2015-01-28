### Library of Initial Dosing Algorithms 
### This is a function which takes the following arguments to run: 
#-"avatars": A data.frame including the avatar study population, 
#-"dosing_algorithm": A desired initial dosing algorithm, 
#-"units": "English" or "Non_English" as described below.
### Concerning the argument "units", please note:
### 1- If the avatars' characteristics are based on English units of measurements (e.g., pound, inch), then the arguments should be set to "English". The initial dosing function takes care of the conversion of the characteristics' values to the appropriate ones that each initial dosing algorithms accepts.
### 2- If the avatars' characteristics are NOT based on English units of measurements (e.g., kg, meter), then the arguments should be set to "Non_English".

###################################################################################################
####################################** NAMING CONVENTIONS **#######################################
### The data frame must include the following column names, with the corresponding values #########
### VARIABLE: "COLUMN NAME" AND CORRESPONDING VALUE                                          ######
### Race:     "RACE" takes a value of "Unknown","Asian","White", or "Black or African American"####
### Age:      "AGE" takes an integer value representing the number of years lived by the patient###
### Height:   "HEIGHT" takes a numeric value representing the height of patient. see units note ###
### Weight:   "WEIGHT" takes a numeric value representing the weight of patient. see units note ### 
### CYP2C9:   "CYP2C9" takes a value of *1/*1, *1/*2, *1/*3, *2/*2, *2/*3, *3/*3, or "Unknown"  ###
### VKORC1.1639: "VKORC1G" takes a value of "A/A", "A/G", "G/G", or "Unknown"                   ###
### VKORC1.1173: "VKORC1T" takes a value of C/C, C/T, T/T, or "Unknown"                         ###
### Enzyme inducer status: "ENZ" takes a value of Y or N                                        ### 
### Amiodarone status: "AMI" takes a value of Y or N                                            ###
### Gender:   "GENDER" takes a value of Y or N                                                  ###
### Smoker:   "SMOKER" takes a value of Y or N                                                  ### 
### Deep vein thrombosis: "DVT" takes a value of Y or N                                         ###
### Target INR: "TINR" takes a numeric value, usually 2.5 or 3                                  ###
###################################################################################################
###################################################################################################


initial_dose <- function(avatars, dosing_algorithm, units="English"){

##### Units conversion
  if(units=="English"){
    unitw=.454
    unith=2.54
  }else{
    unitw=1
    unith=1
  }

##### Create an array to hold calculated initial doses 
  InitialDose=array(0,dim=c(nrow(avatars),2))
  colnames(InitialDose)<-c("InitialDose","BSA")
  
##### BSA Calculation
  BSA=((avatars$WEIGHT*unitw)^.425*(avatars$HEIGHT*unith)^.725*.007184)#BSA is calculated based
    #on the DuBois' method
  #Citation: DuBois D, Du"Bois DF. A formula to estimate the approximate surface area if 
    #height and weight be known.  Arch Int Med 1916;17:863-71.
  InitialDose[,2]<-BSA
  
  ############ Dosing Algorithms Start Here #############
  
  ### PGx initial dosing algorithm derived from COAG paper (PG-COAG). According to the paper
  ### this is used to calculate daily dosage for days 1, 2, and 3.
  ### Citation: Kimmel, Stephen E., Benjamin French, Scott E. Kasner, Julie A. Johnson, Jeffrey L. Anderson, Brian F. Gage, Yves D. Rosenberg et al."A pharmacogenetic versus a clinical algorithm for warfarin dosing." New England Journal of Medicine 369, no. 24 (2013): 2283-2293.


  
  if(dosing_algorithm=="pginitial_COAG"){
  CYPdummy3<-avatars$CYP2C9
  CYPdummy2<-avatars$CYP2C9
  VKORdummy<-avatars$VKORC1G
  levels(CYPdummy3)<-list(absent=c("*1/*1","*1/*2","*2/*2"),#takes integer value of 1
                          hetero=c("*1/*3","*2/*3"),		#takes integer value of 2
                          homo=c("*3/*3"))					#takes integer value of 3
  levels(CYPdummy2)<-list(absent=c("*1/*1","*1/*3","*3/*3"),#takes integer value of 1
                          hetero=c("*1/*2","*2/*3"),		#takes integer value of 2
                          homo=c("*2/*2"))					#takes integer value of 3
  levels(VKORdummy)<-list(GG="G/G",AG="A/G",AA="A/A")       #takes integer value of 1,2, and 3 respectively                      
  InitialDose[,1]=round(exp(0.9751 
                            -0.2066*(as.integer(CYPdummy2)-1)
                            -0.4008*(as.integer(CYPdummy3)-1)
                            -0.3238*(as.integer(VKORdummy)-1)
                            -0.00745*avatars$AGE
                            -0.0901*(avatars$RACE=="Black or African American")
                            +0.0922*(avatars$SMOKER=="Y")
                            +0.4317*BSA
                            -0.2538*(avatars$AMI=="Y")
                            +0.2029*avatars$TINR
                            +0.0664*(avatars$DVT=="Y")#DVT/PE as indication for Warfarin 
  ),2)
  avatars<-cbind(avatars,InitialDose)
  avatars
}



  ### Clinical initial dosing algorithm derived from COAG paper (Clinical-COAG). According to 
  ### the paper this is used to calculate daily dosage for days 1, 2, and 3.
  ### Citation: Kimmel, Stephen E., Benjamin French, Scott E. Kasner, Julie A. Johnson, Jeffrey L. Anderson, Brian F. Gage, Yves D. Rosenberg et al. "A pharmacogenetic versus a clinical algorithm for warfarin dosing." New England Journal of Medicine 369, no. 24 (2013): 2283-2293.
  else if(dosing_algorithm=="clinitial_COAG"){
  InitialDose[,1]=round(exp(0.613 
                            -0.0075*avatars$AGE
                            +0.156*(avatars$RACE=="Black or African American")
                            +0.108*(avatars$SMOKER=="Y")
                            +0.425*BSA 
                            -0.257*(avatars$AMI=="Y")
                            +0.216*avatars$TINR
                            +0.0784*(avatars$DVT=="Y")),2)#DVT/PE as indication for Warfarin
  avatars<-cbind(avatars,InitialDose)
}


  ### PGx initial dosing algorithm derived from CoumaGen-I paper. According to the paper
  ### this is used to calculate dosage for days 1 and 2. Twice the calculated dose is used for
  ### days 1 and 2.  
  ### Citation: Anderson, J. L., Horne, B. D., Stevens, S. M., Grove, A. S., Barton, S., Nicholas, Z. P., ... & Carlquist, J. F. (2007). Randomized trial of genotype-guided versus standard warfarin dosing in patients initiating oral anticoagulation. Circulation, 116(22), 2563-2570.
  ### Here, we use VKORC1T to indicate the genotype VKORC1 

  else if(dosing_algorithm=="pginitial_couma1"){
    for(i in 1:nrow(avatars)){
      InitialDose[i,1]=if(avatars$AMI[i]=="Y"){#25% dose reduction for patients taking Amiodarone
        round(.75*(1.64 +exp(3.984
                             + 0 *(avatars$CYP2C9[i]=="*1/*1")
                             - 0.197 *(avatars$CYP2C9[i]=="*1/*2")
                             - 0.360 *(avatars$CYP2C9[i]=="*1/*3")
                             - 0.947 *(avatars$CYP2C9[i]=="*2/*3")
                             - 0.265 *(avatars$CYP2C9[i]=="*2/*2")
                             - 1.892 *(avatars$CYP2C9[i]=="*3/*3")
                             - 0.304 *(avatars$VKORC1T[i]=="C/T") 
                             - 0.569 *(avatars$VKORC1T[i]=="T/T") 
                             + 0 *(avatars$VKORC1T[i]=="C/C")
                             - 0.009 *avatars$AGE[i]
                             + 0.094 *(avatars$GENDER[i]=="M")
                             + 0 *(avatars$GENDER[i]=="F")
                             + 0.003 * avatars$WEIGHT[i]*unitw))*2/7,2)
      } else{
        round((1.64 +exp(3.984
                             + 0 *(avatars$CYP2C9[i]=="*1/*1")
                             - 0.197 *(avatars$CYP2C9[i]=="*1/*2")
                             - 0.360 *(avatars$CYP2C9[i]=="*1/*3")
                             - 0.947 *(avatars$CYP2C9[i]=="*2/*3")
                             - 0.265 *(avatars$CYP2C9[i]=="*2/*2")
                             - 1.892 *(avatars$CYP2C9[i]=="*3/*3")
                             - 0.304 *(avatars$VKORC1T[i]=="C/T") 
                             - 0.569 *(avatars$VKORC1T[i]=="T/T") 
                             + 0 *(avatars$VKORC1T[i]=="C/C")
                             - 0.009 *avatars$AGE[i]
                             + 0.094 *(avatars$GENDER[i]=="M")
                             + 0 *(avatars$GENDER[i]=="F")
                             + 0.003 * avatars$WEIGHT[i]*unitw))*2/7,2)
      }
    }
    avatars=cbind(avatars,InitialDose)
    return(avatars)
  }
  ### PGx initial dosing algorithm derived from CoumaGen-II paper for arm PG-1 (PG-Couma2). 
  ###According to the paper this is used to calculate daily dosage for days 1 and 2. 
  ###Twice the calculated dose is used for days 1 and 2.
  ### Citation: Anderson, Jeffrey L., Benjamin D. Horne, Scott M. Stevens, Scott C. Woller, Kent M. Samuelson, Justin W. Mansfield, Michelle Robinson et al. "A randomized and clinical effectiveness trial comparing two pharmacogenetic algorithms and standard care for individualizing warfarin dosing (CoumaGen-II)."Circulation 125, no. 16 (2012): 1997-2005.

  
  else if(dosing_algorithm=="pg1initial_couma2"){
  InitialDose[,1]<-round(((5.5922
                           -0.2523*(avatars$AGE%/%10)#converting years to decades 
                           +0.0089*avatars$HEIGHT*unith 
                           +0.0124*avatars$WEIGHT*unitw
                           -0.8410*(avatars$VKORC1G=="A/G")#VKORC1- rs9923231
                           -1.6901*(avatars$VKORC1G=="A/A")
						   -0.4199*(avatars$VKORC1G=="Unknown")
                           -0.5202*(avatars$CYP2C9=="*1/*2")
                           -0.9356*(avatars$CYP2C9=="*1/*3")
                           -0.9789*(avatars$CYP2C9=="*2/*2")
                           -0.8313*(avatars$CYP2C9=="*2/*3")
                           -2.1565*(avatars$CYP2C9=="*3/*3")
                           -0.1486*(avatars$CYP2C9=="Unknown")
                           +0.0821*(avatars$RACE=="Asian")
                           -0.2953*(avatars$RACE=="Black or African American")
                           -0.1661*(avatars$RACE=="Unknown")
                           +1.1889*(avatars$ENZ=="Y")
                           -0.6427*(avatars$AMI=="Y")
                           -0.3468*(avatars$AMI=="Unknown"))^2)/7,2)
  avatars<-cbind(avatars,InitialDose)
}


  ### PGx initial dosing algorithm derived from CoumaGen-II paper for arm PG-2 (PG-Couma2). 
  ###According to the paper this is used to calculate daily dosage for days 1 and 2. 
  ###Twice the calculated dose is used for days 1 and 2.
  ### Citation: Anderson, Jeffrey L., Benjamin D. Horne, Scott M. Stevens, Scott C. Woller, Kent M. Samuelson, Justin W. Mansfield, Michelle Robinson et al. "A randomized and clinical effectiveness trial comparing two pharmacogenetic algorithms and standard care for individualizing warfarin dosing (CoumaGen-II)."Circulation 125, no. 16 (2012): 1997-2005.

  else if(dosing_algorithm=="pg2initial_couma2"){
    InitialDose[,1]<-round(((5.5922
                           -0.2523*(avatars$AGE%/%10)#converting years to decades  
                           +0.0089*avatars$HEIGHT*unith 
                           +0.0124*avatars$WEIGHT*unitw
                           -0.8410*(avatars$VKORC1G=="A/G")#VKORC1- rs9923231
                           -1.6901*(avatars$VKORC1G=="A/A")
						   -0.4199*(avatars$VKORC1G=="Unknown")
                           +0.0821*(avatars$RACE=="Asian")
                           -0.2953*(avatars$RACE=="Black or African American")
                           -0.1661*(avatars$RACE=="Unknown")
                           +1.1889*(avatars$ENZ=="Y")
                           -0.6427*(avatars$AMI=="Y")
                           -0.3468*(avatars$AMI=="Unknown"))^2)/7,2)
    avatars<-cbind(avatars,InitialDose)
  }


  ### PGx initial dosing algorithm derived from Gage paper (PG-Gage). According to the paper
  ### this is used to calculate maintenance dose. 
  ### We assume twice the calculated maintenance dose is used for days 1 and 2.
  ### Citation: Gage, B. F., C. Eby, J. A. Johnson, E. Deych, M. J. Rieder, P. M. Ridker, P. E. Milligan et al. "Use of pharmacogenetic and clinical factors to predict the therapeutic dose of warfarin." Clinical Pharmacology & Therapeutics 84, no. 3 (2008): 326-331.

  
else if(dosing_algorithm=="pginitial_GAGE"){
  InitialDose[,1]<-round(
    exp(0.9751+0.423*BSA
        -0.00745*avatars$AGE
        -0.3238*(avatars$VKORC1G=="A/G")#VKOR3673G
        -0.4008*(avatars$CYP2C9=="*1/*3")
        -0.4008*(avatars$CYP2C9=="*2/*3")
        -0.4008*2*(avatars$CYP2C9=="*3/*3")
        -0.2066*(avatars$CYP2C9=="*1/*2")
        -0.2066*(avatars$CYP2C9=="*2/*3")
        -0.2066*2*(avatars$CYP2C9=="*2/*2")
        +0.2029*avatars$TINR
        -0.2538*(avatars$AMI=="Y")
        +0.0922*(avatars$SMOKER=="Y")
        +0.0901*(avatars$RACE=="Black or African American")
        +0.0664*(avatars$DVT=="Y"))#DVT/PE as indication for Warfarin
    ,2)
  avatars<-cbind(avatars,InitialDose)
}

  ### PGx initial dosing algorithm derived from Gage paper (PG-Gage). According to the paper
  ### this is used to calculate maintenance dose. 
  ### We assume twice the calculated maintenance dose is used for days 1 and 2.
  ### Citation: Gage, B. F., C. Eby, J. A. Johnson, E. Deych, M. J. Rieder, P. M. Ridker, P. E. Milligan et al. "Use of pharmacogenetic and clinical factors to predict the therapeutic dose of warfarin." Clinical Pharmacology & Therapeutics 84, no. 3 (2008): 326-331
  else if(dosing_algorithm=="clinical_GAGE"){
  InitialDose[,1]<-round(
    exp(0.613+.425*BSA
        -0.0075*avatars$AGE
        +0.1560*(avatars$RACE=="Black or African American")
        +0.2160*avatars$TINR
        -0.2570*(avatars$AMI=="Y")
        +0.1080*(avatars$SMOKER=="Y")
        +0.0784*(avatars$DVT=="Y"))*2
    ,2)
}

  
  ### PGx initial dosing algorithm derived from IWPC paper (PG-IWPC). According to the paper
  ### this is used to calculate daily dosage for days 1 and 2.
  ### Citation: International Warfarin Pharmacogenetics Consortium. "Estimation of the warfarin dose with clinical and pharmacogenetic data."The New England journal of medicine 360, no. 8 (2009): 753-64.
  
  
  else if(dosing_algorithm=="pginitial_IWPC"){
    InitialDose[,1]<-round(((		   5.6044 
                                       -0.2614*(avatars$AGE%/%10)#converting years to decades
                                       +0.0087*unith*avatars$HEIGHT
                                       +0.0128*unitw*avatars$WEIGHT
                                       -0.8677*(avatars$VKORC1G=="A/G")#rs9923231
                                       -1.6974*(avatars$VKORC1G=="A/A")
                                       -0.4854*(avatars$VKORC1G=="Unknown")
                                       -0.5211*(avatars$CYP2C9=="*1/*2")
                                       -0.9357*(avatars$CYP2C9=="*1/*3")
                                       -1.0616*(avatars$CYP2C9=="*2/*2") 
                                       -1.9206*(avatars$CYP2C9=="*2/*3")
                                       -2.3312*(avatars$CYP2C9=="*3/*3")
                                       -0.2188*(avatars$CYP2C9=="Unknown")
                                       -0.1092*(avatars$RACE=="Asian") 
                                       -0.2760*(avatars$RACE=="Black or African American")
                                       -1.032*(avatars$RACE=="Unknown")#Unknown: Missing or Mixed race
                                       +1.1816*(avatars$ENZ=="Y")
                                       -0.5503*(avatars$AMI=="Y"))^2)/7,2)
    
    avatars<-cbind(avatars,InitialDose)
    avatars
  }
  
  
  ### Clinical initial dosing algorithm derived from IWPC paper (Clinical-IWPC). According to the paper
  ### this is used to calculate daily dosage for days 1 and 2.
  ### Citation: International Warfarin Pharmacogenetics Consortium. "Estimation of the warfarin dose with clinical and pharmacogenetic data." The New England journal of medicine 360, no. 8 (2009): 753.
  
  else if(dosing_algorithm=="clinitial_IWPC"){
    InitialDose[,1]<-
        ((4.0376 
        -0.2546*(avatars$AGE%/%10)#converting years to decades
        +0.0118*avatars$HEIGHT*unith
        +0.0134*avatars$WEIGHT*unitw
        -0.6752*(avatars$RACE=="Asian")
        +0.4060*(avatars$RACE=="Black or African American")
        +0.0443*(avatars$RACE=="Unknown")#Unknown: Missing of Mixed race
        +1.2799*(avatars$ENZ=="Y")
        -0.5695*(avatars$AMI=="Y"))^2)/7
  
  avatars=cbind(avatars,InitialDose)
    return(avatars)
  }  
  
  else if(dosing_algorithm=="STD_couma1"){ 
    InitialDose[,1]<- 2 * 5
    avatars<-cbind(avatars,InitialDose)
  } else if (dosing_algorithm=="STD_couma2"){
    InitialDose[,1]<- 2 * 5
    avatars<-cbind(avatars,InitialDose)  
  } else if (dosing_algorithm=="STD_EU_PACT"){
	D<-round(((		   				             5.6044 
                                       -0.02614*(avatars$AGE)
                                       +0.0087*unith*avatars$HEIGHT
                                       +0.0128*unitw*avatars$WEIGHT
                                       -0.8677*(avatars$VKORC1G=="A/G")#rs9923231
                                       -1.6974*(avatars$VKORC1G=="A/A")
                                       -0.5211*(avatars$CYP2C9=="*1/*2")
                                       -0.9357*(avatars$CYP2C9=="*1/*3")
                                       -1.0616*(avatars$CYP2C9=="*2/*2") 
                                       -1.9206*(avatars$CYP2C9=="*2/*3")
                                       -2.3312*(avatars$CYP2C9=="*3/*3")
                                       -0.5503*(avatars$AMI=="Y"))^2)/7,2)
    k<-vector("numeric",nrow(avatars))
	for(i in 1:nrow(avatars)){
		if((avatars$CYP2C9[i]=="*1/*1")){
			k[i]=0.0189}
		else if((avatars$CYP2C9[i]=="*1/*2")){
			k[i]=0.0158
		}		
		else if((avatars$CYP2C9[i]=="*1/*3")){
			k[i]=0.0132
		}
		else if((avatars$CYP2C9[i]=="*2/*2")){
			k[i]=0.0130
		}
		else if((avatars$CYP2C9[i]=="*2/*3")){
			k[i]=0.009
		}
		else if((avatars$CYP2C9[i]=="*3/*3")){
			k[i]=0.0075
		}		
	}
	LD3<-D/((1-exp(k*-24))*(1+exp(k*-24)+exp(-2*k*24)))#where 24 is the number of hours
	x<-round((LD3-D)*(1.5)+D,2)
	InitialDose[,1]<-x
  avatars<-cbind(avatars,InitialDose)
    avatars
  }
    else{
    print("wacka wacka")
  }
  return(avatars)
}