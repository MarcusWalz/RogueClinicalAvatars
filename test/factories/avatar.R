
# Generates a random avatar. Nothing Fancy goes on to make the avatar realistic.
# E.g. BMI is completly independent of height and weight.
random_avatar = function () {
  list (
    "GENDER" = sample(c("M", "F"), 1)
  , "AGE"    = rnorm(1,50,10)
  , "RACE"   = sample(c("White", "Black or African American", "Asian"), 1) 
  , "HEIGHT" = rnorm(1,62,10)
  , "WEIGHT" = rnorm(1,180,20)
  , "SMOKER" = sample(c("Y", "N"), 1)
  , "AMI" = sample(c("Y", "N"), 1)
  , "FLUVASTATIN" = sample(c("Y", "N"), 1)
  , "ENZYME" = sample(c("Y", "N"), 1)
  , "CYP2C9" = sample(c("*1/*1", "*1/*2", "*1/*3", "*2/*1","*2/*2", "*2/*3"), 1)
  , "VKORC1G" = sample(c("G/G", "A/A", "G/A"), 1)
  , "TINR" = sample(c("low", "N"), 1)
  , "CONDITION" = "condition_1"
  , "BMI" = rnorm(1,28,3)
  )
}
