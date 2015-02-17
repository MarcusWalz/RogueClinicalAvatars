source("hamberg_2007.R")
source("protocols.R")
source("stable_dose.R")
source("initial_dosing.R")

source("data/avatar.R")


test=NA

# LOAD TEST FACTORIES
if(exists("test")) {

  source("test/factories/avatar.R")
  source("test/factories/check.R")
  source("test/factories/dose.R")
  source("test/factories/inr.R")
  source("test/factories/simulation.R")
  source("test/factories/sim_factory.R")

}

scripts = commandArgs( trailingOnly = TRUE )

source( scripts[1] )
