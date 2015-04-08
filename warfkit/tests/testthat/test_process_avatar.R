library(warfkit)

# Get some sample data

data("sample_warf_avatars")


my_sim = list (
    days = 20
  , protocol = "ahc_clinical"
  , initial_dose = "AHC"
  , replicates = 3
  , max_time = 24
  , max_dose = 100
  )

small_samp = sample_warf_avatars[1:3,]
simulation_in = preprocess_avatars(small_samp, my_sim)

fst = simulation_in[[1]]
a = process_avatar(fst)
b = process_avatar(fst)

test_that("process_avatars is stateless", {
  expect_equal(a, b)
})

test_that("a simulation is ran for each replicate", {
  expect_equal(length(a$sim_out), a$simulation$replicates)
})

test_that("replicates are not consistant", {
  expect_false(all(a$sim_out[[1]]$INR == a$sim_out[[2]]$INR))
})
