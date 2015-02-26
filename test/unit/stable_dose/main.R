test_truth = function() { TRUE }

av = avatars_random()
sim = simulation_random()

p = function(obj, comp, value) { 
  comp(obj,value)
}

# One dose splice, should get three dfs back
test_group_by_dose_1 = function () {
  length(group_by_dose()(sim_out_df(av,sim)( dose = dose_spike(20)))) == 3
}

# Constant dose, should get only 1 df back
test_group_by_dose_2 = function () {
  length(group_by_dose()(sim_out_df(av,sim)( dose = dose_constant() ))) == 1
}

test_group_by_similar_dose = NA

test_group_by_inr_stability = NA

test_filter_unstable_inr = NA
test_filter_unchecked_days = NA

tets_calc_days_elapsed = NA
test_calc_num_stable_consecutive_checkups = NA

test_filter_dfs_by_days_elapsed = NA

test_filter_dfs_by_rows = NA
test_filter_dfs_by_checks = NA
test_filter_dfs_by_stable_checks = NA
test_filter_dfs_by_consecutive_stable_checks = NA
test_filter_dfs_has_check_gte_days_apart = NA
test_filter_dfs_has_check_lte_days_apart = NA
test_preds = NA
test_choose_by_rows = NA
test_choose_by_days_elapsed = NA

NUM_NA_TESTS = 0
NUM_FAILED_TESTS = 0
NUM_SUCCESS_TESTS = 0

for(test_name in ls(pattern="test_")) {
  current_test = get(test_name)
  if(is.na(current_test)) {
    print(paste(test_name, "UNDEFINED")) 
    NUM_NA_TESTS = NUM_NA_TESTS+1
  } else if(is.function(current_test)) {
    if(current_test()) {
      NUM_SUCCESS_TESTS = NUM_SUCCESS_TESTS+1
    } else {
      print(paste(test_name, "FAILED")) 
      NUM_FAILED_TESTS = NUM_FAILED_TESTS+1
    }
  } else {
      print(paste(test_name, "is not a valid test object")) 
  }
}
print(paste("SUCC:", NUM_SUCCESS_TESTS, "FAIL:", NUM_FAILED_TESTS, "UNDEFINED:", NUM_NA_TESTS)) 

