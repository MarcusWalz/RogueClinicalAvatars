
# Generates a random avatar. Nothing Fancy goes on to make the avatar realistic.
# E.g. BMI is completly independent of height and weight.
avatar_random = function () {
  list (
    "AGE"    = rnorm(1,50,10)
  , "AMI" = sample(c("Y", "N"), 1)
  , "CYP2C9" = sample(c("*1/*1", "*1/*2", "*1/*3", "*2/*1","*2/*2", "*2/*3", "*3/*3"), 1)
  , "DVT" = sample(c("Y", "N"), 1)
  , "ENZYME" = sample(c("Y", "N"), 1)
  , "GENDER" = sample(c("M", "F"), 1)
  , "HEIGHT" = rnorm(1,62,10)
  , "RACE"   = sample(c("White", "Black or African American", "Asian", "Unknown"), 1) 
  , "SMOKER" = sample(c("Y", "N"), 1)
  , "TINR" = rnorm(1, 2.7, 0.3)
  , "VKORC1G" = sample(c("G/G", "A/A", "G/A", "Unknown"), 1)
  , "VKORC1T" = sample(c("C/C", "C/T", "T/T", "Unknown"), 1)
  , "WEIGHT" = rnorm(1,180,20)
  )

}

# Grabs a random avatar from a dataframe
avatar_random_from_table = function(avatars) {
  avatars[sample(nrow(avatar),1), ]
}
