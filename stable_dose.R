#  source("data/inr.R")

# 
# ARGS:
#  definiton: Integer between one through twenty
# RETURNS:
#  a function that takes the following three arguments:
#  avatar
#  inr:   Vector containing INR data
#  dose:  Vector containing dose data
#  check: Vector containing check data


# Sketch. Calculate stable dose through a special kind of
# function composition. These functions can do two things:
#
#  (1) Filter rows in a data frame.
#  (2) Split data frames up by some value
# 
# To make things simple the functions act on a single data 
# frame. They can return many data frames. Empty data frames
# with zero rows act as null value. 
# 
# This approach is rather versatile. Defs 1,4,6,7,8,9,10,11,
# 13,14,15,16,17,18,19,20, and 21 can all be calculated using
# the same six functions set to different constants. The
# other definitions are too ambigious (q.v. def 12 if you're 
# really in the mode for a good wince) or are too trivial to
# even use this approach. 
# 
# If all goes correctly the dose definitions will look like this:
# 
# combine_functions = function(c(
#   group_by_dose()
# , group_by_dose_stability(2,3)
# , filter_unstable_inr(2,3)
# , filter_by_days_elapsed(6)
# ))(sim_out)
#
# If R was a typed language and things in square brackets 
# denoted lists our functions would be like this:
# 
# group_by_dose :: DataFrame -> [DataFrame] 
# combine_functions :: [DataFrame -> [DataFrame]] -> DataFrame -> [DataFrame]
# combine_functions2 :: (DataFrame -> [DataFrame), (DataFrame -> [Data-Frame])
#    -> DataFrame -> [DataFrame]
#
# The problem I'm encountering is that keeping lists or vectors of data
# frames is hard. Adding a single data frame to a list creates a list of columns
# in the data frame. Even scarier stuff happens if I try to use vectors.
# 
# Luckily the solution is simple (although I don't quite understand it). Basically
# we need to append to lists like this:
# 
#   append(list, list(new_element))
# 
# and then everything works as expected. 
# 
# The last step would be using a sort of "choice" function
# that picks out the best data frame to grab the dose quatitiy. 



cf = function(f,g) {
  function(sim_out) {
    output = list()
    results = f(sim_out) 

    for(result in results) {
      print("f output")
      # filtering the dataframe, doesn't seem to delete any rows. However, columns it does
      if(ncol(result) > 0) {
        results2 = g(result)
        for(result2 in results2) {
          if(ncol(result2) > 0) {
            print("g output")
            output=append(output, list(result2))
          }
        }
      }
    }
    return(output)
  }
}

combine_f = function(combs) { function(sim_out) {
  Reduce(function(a,b) { combine_functions2(a,b) }, combs, right=F)
  (sim_out) } }

# just for test
id = function() {function(sim_out) { list(sim_out) }}

group_by_dose = function() { function(sim_out) {
  if(nrow(sim_out) == 1) { return(list(sim_out)) } 

  last_dose = sim_out$Dose[1]

  frames = list()
  current_frame = sim_out[1,]

  for(i in 2:nrow(sim_out)) {
    if(sim_out$Dose[i] == last_dose) {
      # add to current frame
      current_frame = rbind(current_frame, sim_out[i,])
    } else {
      # append current frame and reset
      frames = append(frames, list(current_frame))
      current_frame=sim_out[i,]
      last_dose=sim_out$Dose[i]
    }
  }
  frames = append(frames, list(current_frame))
}}

group_by_inr_stability = function(min, max) {

  is_stable = function(x) { x >= min && x <= max }

  function(sim_out) {
    if(nrow(sim_out) == 1) { return(list(sim_out)) } 

    stable = is_stable(sim_out$INR[1])

    frames = list()
    current_frame = sim_out[1,]

    for(i in 2:nrow(sim_out)) {
      if(is_stable(sim_out$INR[i]) == stable) {
        # add to current frame
        current_frame = rbind(current_frame, sim_out[i,])
      } else {
        # append current frame, and reset
        frames = append(frames, list(current_frame))
        current_frame=sim_out[i,]
        stable = is_stable(sim_out$INR[i])
      }
    }
    frames = append(frames, list(current_frame))
  }
}

filter_unchecked_days = function() { function(sim_out) {
  list(sim_out[sim_out$check != 0])  
}
}

# calculate the days elapsed in a dataframe
calc_days_elapsed = function(df) {
  as.numeric(rownames(df)[nrow(df)] - as.numeric(rownames(df)[0])) + 1
}

# calculate the number of checkups within a dataframe
calc_num_checkups = function(df) {
  sum(df$INR != 0)
}

filter_dfs_by_days_elapsed = function(days) { function(df) {
  calc_days_elapsed(df) >= days
}}

filter_dfs_by_rows = function(rows) { function(df) {
  nrow(df) >= rows
}}

filter_dfs_by_min_check_interval = function(days) {
  function(df) {
    checks=df[df$Check !=0,]$Check
    if(length(checks) == 0) { return(FALSE) }

    max(checks) - min(checks) >= days

  }
}

filter_dfs_by_num_checkups = function(checkups) { function(df) {
  calc_num_checkups(df) >= checkups
}}

# roll predicates into one
preds = function(predicates) { function(df) {
  if(!is.vector(predicates)) {
    predicates <- c(predicates)
  }
  for (predicate in predicates) {
    if(!predicate(df)) { return(FALSE) }
    else { print("okay") }
  }
  return(TRUE)
}}

# Selectors. Reduce lists of dataframes to one dataframe. Pick the latest (i.e. rightmost) dataframe in a tie.
choose_by_rows = function(df1, df2) {
  if(nrow(df1) > nrow(df2)) { df1 } else { df2 }
}

choose_by_days_elapsed = function(df1, df2 ) {
  if(calc_days_elapsed(df1) > calc_days_elapsed(df2)) { df1 } else { df2 } 
}


remove_unstable_inr = function(min, max) {
  is_stable = function(x) { ((x >= min) * (x <= max)) == 1}

  function(sim_out) {
    list(sim_out[is_stable(sim_out$INR),]) 
  }
}


# execute the defs
exe = function(groupers_and_filters, df_predicates, choice_function) { function(sim_out) {
  result = Reduce(choice_function, (Filter(preds(df_predicates), (cfv(groupers_and_filters)(sim_out)))), init=data.frame())
  if( nrow(result) == 0 ) { NA } else { mean(result$Dose) }
}}


cfv = function(vect) {
  str=Reduce(function(a,b) { paste("cf(",a,",vect[[",b,"]])",sep="") }, 1:length(vect), init="id()")
  print(str)
  eval(parse(text=str))
}

cfv2 = function(vect) {
  Reduce(function(a,b) { cf(a,b)}, vect)
}

high_low = function(f, a, b) { paste(f, "(", a, ",", b, ")") }

# Definition of Stable Dose of Warfarin for Each Research Group

# 1 The dose (unchanged for 6 days) that yielded an INR within 0.5 of the
# target INR.
stable_def_1 = function (simulation) { 
  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5

  # Group doses together
  # Then SubGroup stable INR together
  # Filter out instances of unstable INR. Empty DFs get removed.
  # Filter out data frames with less than 6 rows
  # Choose the data frame with the most rows

  exe(c( group_by_dose()
       , group_by_inr_stability(inr_low, inr_high) 
       , filter_unstable_inr(inr_low, inr_high) 
       )
     , filter_dfs_by_rows(6)
     , choose_by_rows
     )(simulation$sim_out) 
}


only_on_checks = function(sim_out) {
  sim_out[sim_out$Check != 0,]
}

filter_unstable_inr = function(min=2,max=3) { function(sim_out) {
  if(nrow(sim_out) == 0) { return(list()) }

  list(sim_out[sim_out$INR >= min && sim_out$INR <= max])
}}

filter_uncheked_days = function(min=2,max=3) { function(sim_out) {
  if(nrow(sim_out) == 0) { return(list()) }

  list(sim_out[sim_out$Check != 0])

}}



# 2 Average weekly dose (irrespective of achieved INR) that the patient received
# during the observation period, excluding the first 28 days after warfarin initiation.
stable_def_2 = function (simulation) { 
  if(nrow(simution$sim_out) <= 28) { return(NA) }
  mean(simulation$sim_out$Dose[29:NROW(simulation$sim_out)])
}

# 3 The chronic (> 30 days) warfarin dose that led to an INR in the
# therapeutic target range on several occasions.
stable_def_3 = function (simulation) { 
# Several occasions as in 3 checks.
# Theraputic as in 2-3
  exe(c( group_by_dose()
       , filter_uncheck_days()
       , group_by_inr_stability(2, 3)
       , filter_unstable_inr(2, 3) 
       )
     ,c( filter_dfs_by_elapsed(30)
       , filter_dfs_by_checks(3)
       )
     , choose_by_elapsed
     )(simulation$sim_out) 
}

# 4 Warfarin therapeutic dose was defined as dose given when patients reach
# stable therapeutic INR. Stable therapeutic INR was defined as at least two
# consecutive INR measurements between 1.7-3 on the same warfarin daily or
# weekly dose measured at least one week apart.
stable_def_4 = function (simulation) { 
  exe(c( group_by_dose()
       , filter_uncheck_days()
       , group_by_inr_stability(1.7, 3)
       , filter_unstable_inr(1.7, 3) 
       )
     ,c( filter_dfs_by_min_check_interval(7)
       )
     , choose_by_elapsed
     )(simulation$sim_out)
}

# 5 Warfarin Therapeutic Dose was defined as a single mean weekly warfarin dose
# that was calculated by averaging the warfarin dose at each of the 3
# consecutive clinic visits. A Stable Weekly Maintenance Dose of Warfarin was
# defined as a dose that did not vary by more than 10% between clinic visits.
# In addition, the INR at each of the 3 visits had to be in the patient's specific
# goal INR range. 
stable_def_5 = function (simulation) { 
  FALSE #TODO
}

# 6 The warfarin dose that led to an INR in the therapeutic
# range (2-3) on at least 3 consecutive clinic visits over a minimum period of 3
# months.
stable_def_6 = function (simulation) { 
  exe(c( group_by_dose()
       , filter_uncheck_days()
       , group_by_inr_stability(2, 3)
       , filter_unstable_inr(2, 3) 
       )
     ,c( filter_dfs_by_days_elapsed(90)
       , filter_dfs_by_checks(3)
       )
     , choose_by_elapsed
     )
}


# 7 Two consecutive INRs in the target range of 2-3.5 while on a constant dose
# where INR measures are taken at least 3 days but less than 8 days apart.
stable_def_7 = function (simulation) { 
  FALSE #TODO
}

# 8 Stable dose in our data set is defined as the dose of 3 consecutive clinic
# visits, within therapeutic range of INR, the same daily dose over 3 months
# based on Higashi et al., JAMA 2002 (PMID 11926893).
stable_def_8 = function (simulation) { 
  FALSE #TODO
}

# 9 INR between 2 and 3 for a period >1 month.
stable_def_9 = function (simulation) { 
  exe(c( group_by_inr_stability(2, 3)
       , filter_unstable_inr(2, 3) 
       )
     ,c( filter_dfs_by_days_elapsed(30)
       )
     , choose_by_elapsed
     )(simulation$sim_out)

}

# 10 The definition of stable dose reported here is as follows: Dose at which
# INR was within therapeutic range (+/- 0.2 INR units) on 3 consecutive visits, with <90
# days between subsequent visits.
stable_def_10 = function (simulation) { 
  FALSE #TODO
}

# 11 The same warfarin dose for at least 3 consecutive clinic visits. No INR criteria to
# define stable dose was used because it was assumed that either the INR was in
# range at each visit or was not sufficiently out-of-range to elicit a change in dose.
stable_def_11 = function (simulation) { 
  FALSE #TODO
}


# 12 A cross-sectional study of patients treated with warfarin for at least 2
# months and with relatively stable anticoagulation.
stable_def_12 = function (simulation) { 
  FALSE #TODO
}

# 13 Patients whose warfarin dose requirement was 1.5 mg per day or less and
# had a stable warfarin dose requirement for at least 3 consecutive clinic
# visits with a target International Normalised Ratio (INR) of 2.0 to 4.0 and
# no apparent cause for low dose requirement such as drug interactions or
# liver disease.
stable_def_13 = function (simulation) { 
  FALSE #TODO
}

# 14 Dose that lead to stable INR over 3 visits.
stable_def_14 = function (simulation) { 
  FALSE #TODO
}

# 15 Warfarin therapeutic dose was defined as warfarin dose at stable anticoagulation.
# Stable anticoagulation was defined when 2 consecutive INR measurements done
# at least 7 days apart were within the desired therapeutic range (i.e. 2-3),
# while warfarin dose was not changed.
stable_def_15 = function (simulation) { 
  FALSE #TODO
}

# 16 Warfarin dose that lead to the target INR, usually 2-3 on 1 or more
# occasions over a minimum of 30 days.
stable_def_16 = function (simulation) { 
  FALSE #TODO
}

# 17 The dose of warfarin that led to an INR in the target range (2-3 for
#all indications except valve prosthesis; for valve prosthesis the range is
#2.5 - 3.5) on 3 consecutive measurements.
stable_def_17 = function (simulation) { 
  FALSE #TODO
}

# 18 INR within therapeutic range of between 2-3 months for at least 3 months.
stable_def_18 = function (simulation) { 
  FALSE #TODO
}

# 19 Same dose on 3 or more consecutive clinic visits, with INR within therapeutic
# range.
stable_def_19 = function (simulation) { 
  FALSE #TODO
}

# 20 Subjects were required to be on warfarin and managed in the warfarin clinic for a
# minimum of 3 months before entry into the study, and thus dosing had
# stabilized by the time data collection for the study began. Values reported
# for warfarin dose are the average over the entire time of participation in
# the study (average = 20.6 months). 
stable_def_20 = function (simulation) { 
  FALSE #TODO
}

# 21 Stable dose defined as the dose that leads to a stable INR over three
# consecutive visits following initiation of the drug, with these INR
# measurements encompassing a period of at least 2 weeks, with a maximum
# difference between the mean daily dosages of 10%.
stable_def_21 = function (simulation) { 
  FALSE #TODO
}

getStableDefinition = function (group_number) {

  get(paste("stable_def_", as.character(group_number), sep = ""))

} 
