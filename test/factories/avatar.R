
# Generates a random avatar. Nothing Fancy goes on to make the avatar realistic.
# E.g. BMI is completly independent of height and weight.
avatars_random = function (n = 1) {
  # over ride the sample function
  sample_ <- function(fact, n)  sample(fact, n, replace=T)
  as.data.frame(
    list (
      "AGE"    = rnorm(n,50,10)
    , "AMI" = sample_(YesNoFactor, n)
    , "CYP2C9" = sample_(CYP2C9, n)
    , "DVT" = sample_(YesNoFactor, n)
    , "ENZYME" = sample_(YesNoFactor, n)
    , "GENDER" = sample_(GENDER, n)
    , "HEIGHT" = rnorm(n,62,10)
    , "RACE"   = sample_(RACE, n) 
    , "SMOKER" = sample_(YesNoFactor, n)
    , "TINR" = rnorm(n, 2.7, 0.3)
    , "VKORC1G" = sample_(VKORC1G, n)
    , "VKORC1T" = sample_(VKORC1T, n)
    , "WEIGHT" = rnorm(n,180,20)
    )
  )

}

# Grabs a random avatar from a dataframe
  avatar_random_from_table = function(n =1, avatars) {
  avatars[sample(nrow(avatar),n), ]
}
