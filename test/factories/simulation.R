simulation_random = function (n = 1) {
  as.data.frame(
    list( seed         = sample(1:n, n)
        , protocol     = sample(protocols, n, replace=TRUE)
        , initial_dose = "test" # TODO
        , max_dose     = 10
        , days         = 100
        )
  )
}
