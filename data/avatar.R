# Avatar Definition


# Factors 

# "RACE" takes a value of "Unknown","Asian","White", or "Black or African American" 
RACE    = as.factor(c("White", "Black or African American", "Asian", "Unknown")) 
# "VKORC1G" takes a value of "A/A", "A/G", "G/G", or "Unknown"
VKORC1G = as.factor(c("G/G", "A/A", "G/A", "Unknown"))
# "VKORC1T" takes a value of C/C, C/T, T/T, or "Unknown"                         
VKORC1T = as.factor(c("C/C", "C/T", "T/T", "Unknown"))
# "CYP2C9" takes a value of *1/*1, *1/*2, *1/*3, *2/*2, *2/*3, *3/*3, or "Unknown"  
CYP2C9 = as.factor(c("*1/*1", "*1/*2", "*1/*3", "*2/*1","*2/*2", "*2/*3", "*3/*3", "Unknown"))
# "GENDER" takes a value of Y or N
GENDER = as.factor(c("M", "F"))

# Yes/No

# Enzyme inducer status: "ENZ" takes a value of Y or N                       
# Smoker:   "SMOKER" takes a value of Y or N 
# Deep vein thrombosis: "DVT" takes a value of Y or N
# Amiodarone status: "AMI" takes a value of Y or N

YesNoFactor = as.factor(c("Y", "N"))

# Numeric

# Age:      "AGE" takes an integer value representing the number of years lived by the patient
# Height:   "HEIGHT" takes a numeric value representing the height of patient. see units note 
# Weight:   "WEIGHT" takes a numeric value representing the weight of patient. see units note  
# Target INR: "TINR" takes a numeric value, usually 2.5 or 3


avatars_valid = function(avatars) {
  
  ( avatars$RACE    %in% RACE 
  * avatars$VKORC1G %in% VKORC1G
  * avatars$VKORC1T %in% VKORC1T
  * avatars$CYP2C9  %in% CYP2C9
  * avatars$GENDER  %in% GENDER
  * avatars$ENZ     %in% YesNoFactor
  * avatars$SMOKER  %in% YesNoFactor
  * avatars$DVT     %in% YesNoFactor
  * avatars$AMI     %in% YesNoFactor
  * sapply(avatars$AGE, is.numeric)
  * sapply(avatars$WEIGHT, is.numeric)
  * sapply(avatars$HEIGHT, is.numeric)
  * sapply(avatars$TINR, is.numeric)
  ) == 1
}
