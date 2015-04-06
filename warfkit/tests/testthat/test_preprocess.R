library(warfkit)

# Get some sample data

data("sample_warf_avatars")


my_sim = list (
    days = 5
  , protocol = "ahc_clinical"
  , initial_dose = "AHC"
  , replicates = 1
  , max_time = 24
  , max_dose = 100
  )

test_that("Preprocess requires valid avatars",  {
    expect_error(preprocess_avatars(as.data.frame(), my_sim))
    expect_error(preprocess_avatars(NA, my_sim))
})

small_samp = sample_warf_avatars[1:3,]
out = preprocess_avatars(small_samp, my_sim)

test_that("Preprocess outputs a list equal to input", {
    
    expect_is(out, "list")
    expect_equal(length(out), 3)
})

fst = out[[1]]

test_that("Preprocess is in the described format", {

  expect_is(fst, "list")
  expect_is(fst$avatar, "list")
  expect_is(fst$simulation, "list")
  expect_is(fst$simulation$seed, "integer")
  expect_equal(length(fst$simulation$seed), fst$simulation$replicates)
})

snd = out[[2]]

test_that("Assigned Seeds are semi-unique", {

  expect_false(fst$simulation$seed == snd$simulation$seed) 

})

test_that("Order is retained", {
  expect_equal(as.list(small_samp[1,]), fst$avatar)
  expect_equal(as.list(small_samp[2,]), snd$avatar)
})
