# Avatar Definition


# Factors 

# "RACE" takes a value of "Missing","Asian","White", or "Black or African American" 
RACE    = as.factor(c("White", "Black or African American", "Asian", "Missing")) 
# "VKORC1G" takes a value of "A/A", "A/G", "G/G", or "Unknown"
VKORC1G = as.factor(c("G/G", "A/A", "A/G", "Unknown"))
# "VKORC1T" takes a value of C/C, C/T, T/T, or "Unknown"                         
VKORC1T = as.factor(c("C/C", "C/T", "T/T", "Unknown"))
# "CYP2C9" takes a value of *1/*1, *1/*2, *1/*3, *2/*2, *2/*3, *3/*3, or "Unknown"  
CYP2C9 = as.factor(c("*1/*1", "*1/*2", "*1/*3", "*2/*1","*2/*2", "*2/*3", "*3/*3", "Missing"))
# "GENDER" takes a value of Y or N
GENDER = as.factor(c("M", "F"))

# Yes/No

# Enzyme inducer status: "ENZYME" takes a value of Y or N                       
# Smoker:   "SMOKER" takes a value of Y or N 
# Deep vein thrombosis: "DVT" takes a value of Y or N
# Amiodarone status: "AMI" takes a value of Y or N

YesNoFactor = as.factor(c("Y", "N"))

# Numeric

# Age:      "AGE" takes an integer value representing the number of years lived by the patient
# Height:   "HEIGHT" takes a numeric value representing the height of patient. see units note 
# Weight:   "WEIGHT" takes a numeric value representing the weight of patient. see units note  
# Target INR: "TINR" takes a numeric value, usually 2.5 or 3


# Checks if a dataframe of avatar is formatted correctly in order to run the hamburg model.
validate_avatars = function(avatars) {
  # exits if an unknown value is picked up!
  match_factors = function(column, factor) {
    if(!column %in% colnames(avatars)) {
      stop(paste("Avatars lack column:", column))
    }
    present = (levels(avatars[,column]) %in% factor)
    if(!all(present)) {
      stop(paste("Unknown factors in", column, ":", levels(avatars[,column])[!present]))
    } 
  }

  match_numeric = function(column) {
    if(!column %in% colnames(avatars)) {
      stop(paste("Avatars lack column:", column))
    }
    if(!is.numeric(avatars[,column])) {
      stop(paste("Non-numeric value in column:", column))
    }
  }

  match_factors("RACE"   , RACE)
  match_factors("VKORC1G", VKORC1G)
  match_factors("VKORC1T", VKORC1T)
  match_factors("CYP2C9" , CYP2C9)
  match_factors("GENDER" , GENDER)
  match_factors("ENZYME" , YesNoFactor)
  match_factors("SMOKER" , YesNoFactor)
  match_factors("DVT"    , YesNoFactor)
  match_factors("AMI"    , YesNoFactor)


  match_numeric("AGE")
  match_numeric("WEIGHT")
  match_numeric("HEIGHT")
  match_numeric("TINR")

}
