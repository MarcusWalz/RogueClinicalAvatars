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



# Compose grouping row-filter functions. 
cf = function(f,g) {
  function(sim_out) {
    output = list()
    results = f(sim_out) 

    for(result in results) {
      # filtering the dataframe, doesn't seem to delete any rows. However, columns it does
      if(ncol(result) > 0) {
        results2 = g(result)
        for(result2 in results2) {
          if(ncol(result2) > 0) {
            output=append(output, list(result2))
          }
        }
      }
    }
    return(output)
  }
}

# Compose a vector/list of row-filter splitter functions 
cfv = function(vect) {
  str=Reduce(function(a,b) { paste("cf(",a,",vect[[",b,"]])",sep="") }, 1:length(vect), init="id()")
  eval(parse(text=str))
}

# does absolutely nothing. q.v. cfv for use case
id = function() {function(sim_out) { list(sim_out) }}

# grouping functions:


# sequentially partition into dfs with the same dose
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

# Make sure the dose doesn't change more than diff% between
# consecutive visits.
group_by_similar_dose = function(diff=0.1) { function(sim_out) {
  if(nrow(sim_out) == 1) { return(list(sim_out)) } 

  last_dose = sim_out$Dose[1]

  frames = list()
  current_frame = sim_out[1,]

  for(i in 2:nrow(sim_out)) {
    if(abs(sim_out$Dose[i] - last_dose) <= (diff*last_dose)) {
      # add to current frame
      current_frame = rbind(current_frame, sim_out[i,])
      last_dose=sim_out$Dose[i]
    } else {
      # append current frame and reset
      frames = append(frames, list(current_frame))
      current_frame=sim_out[i,]
      last_dose=sim_out$Dose[i]
    }
  }
  frames = append(frames, list(current_frame))
}}


# sequentially partition into dfs with all stable all unstable 
group_by_inr_stability = function(min=default_stable_inr_low
                                 , max=default_stable_inr_high) {

  is_stable = function(x) { x >= min & x <= max }

  function(sim_out) {
    if(nrow(sim_out) <= 1) { return(list(sim_out)) } 

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

# filter out tables with unstable inr
filter_unstable_inr = function(min=default_stable_inr_low
                              ,max=default_stable_inr_high) { function(sim_out) {
  if(nrow(sim_out) == 0) { return(list()) }

  list(sim_out[sim_out$INR >= min && sim_out$INR <= max,])
}}

# filter out days where no check was taken
filter_unchecked_days = function() { function(sim_out) {
  if(nrow(sim_out) == 0) { return(list()) } 
  list(sim_out[sim_out$Check != 0,])  
}}

# calculate the days elapsed in a dataframe
calc_days_elapsed = function(df) {
  if(nrow(df) == 0) { return(0) } 
  as.numeric(rownames(df)[nrow(df)]) - as.numeric(rownames(df)[1]) + 1
}

# calculate the number of checkups within a dataframe
calc_num_checkups = function(df) {
  sum(df$Check != 0)
}

# calculate the number of stable checkups
calc_num_stable_consecutive_checkups = function(df, min, max) {
  check_only = df[df$Check != 0,]
  checks = (check_only$INR >= min) * (check_only$INR <= max) == 1

  counter = 0
  my_max = 0
  for(i in checks) {
    if(i) {
      counter = counter + 1
      my_max = max(c(counter, my_max))
    } else {
      counter = 0
    }
  }

  my_max
}

# filter out dfs where fewer than n days elapsed
filter_dfs_by_days_elapsed = function(days) { function(df) {
  calc_days_elapsed(df) >= days
}}

# filter out dfs with fewer than n rows
filter_dfs_by_rows = function(rows) { function(df) {
  nrow(df) >= rows
}}

# filter dfs where fewer than n checks take place
filter_dfs_by_checks = function(checks) { function(df) {
  calc_num_checkups(df) >= checks
}}

filter_dfs_by_stable_checks = function(checks,min,max) { function(df) {
  nrow(df[df$Check != 0 & df$INR >= min & df$INR <= max, ]) >= checks
}}

filter_dfs_by_consecutive_stable_checks = function(checks,min,max) { function(df) {
  calc_num_stable_consecutive_checkups(df, min, max) >= checks
}}
# keep dfs where max(checks) - min(checks)+1 >= days
# i.e. where at least n days elapsed between checks
filter_dfs_has_check_gte_days_apart = function(days) {
  function(df) {
    checks=df[df$Check !=0,]$Check
    if(length(checks) <= 1) { return(FALSE) }

    max(checks) - min(checks) + 1 >= days

  }
}

filter_dfs_has_check_lte_days_apart = function(days) {
  function(df) {
    checks=df[df$Check !=0,]$Check

    if(length(checks) <= 1) { return(FALSE) }

# checks are ordered

    for(i in 2:length(checks)) {
      if(checks[i] - checks[i-1] + 1 <= days) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
}


# roll multiple predicates into one. Used in exe.
preds = function(predicates=c()) { function(df) {
  if(length(predicates) == 0) { return(TRUE) }
  all(unlist(Map(function(p) { p(df) }, predicates)))
}}

# Selectors. Reduce lists of dataframes to one dataframe. Pick the latest (i.e. rightmost) dataframe in a tie.

# pick the biggest df in terms of number of rows
choose_by_rows = function(df1, df2) {
  if(nrow(df1) > nrow(df2)) { df1 } else { df2 }
}

# pick the biggest df in terms of the number of days elapsed
choose_by_days_elapsed = function(df1, df2) {
  if(calc_days_elapsed(df1) > calc_days_elapsed(df2)) { df1 } else { df2 } 
}


# execute the defs
exe = function(groupers_and_filters, df_predicates, choice_function, debug=FALSE) { function(sim_out) {
  # Round the dose to lowest 0.5mg
  sim_out$Dose = floor(sim_out$Dose * 2)/2
  if(debug) {
    a = cfv(groupers_and_filters)(sim_out)
    print("groupers")
    print(a)
    print("df preds")
    b = Filter(preds(df_predicates), a)
    print(b)
    print("reducer")
    result = Reduce(choice_function, b, init=data.frame())
    print(result)
  }
  else {
    result = Reduce(choice_function, (Filter(preds(df_predicates), (cfv(groupers_and_filters)(sim_out)))), init=data.frame())
  }
  if( nrow(result) == 0 ) { NA } else { mean(result$Dose) }
}}


# stable does definitions take simulation output a list where
# 
# $avatar     = clincial avatar
# $simulation = simulation params (not used)
# $sim_out    = simulation output where:
#   rownames consists of the day (as a string)
#   $Dose (in mg)
#   $INR
#   $Check (=rowname as an int)

# returns a dose or...
# returns NA if no stable dose exists
# returns FALSE if stable dose definition isn't coded


# Definition of Stable Dose of Warfarin for Each Research Group

# 1 The dose (unchanged for 6 days) that yielded an INR within 0.5 of the
# target INR.
stable_def_1 = function (simulation) { 
  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5

  exe(c( group_by_dose()
        )
  ,c( filter_dfs_by_days_elapsed(6)
     , filter_dfs_by_consecutive_stable_checks(2, inr_low, inr_high)
     )
  , choose_by_days_elapsed
  )(simulation$sim_out) 
}

# 2 Average weekly dose (irrespective of achieved INR) that the patient received
# during the observation period, excluding the first 28 days after warfarin initiation.
stable_def_2 = function (simulation) { 
  if(nrow(simulation$sim_out) <= 28) { return(NA) }
  mean(simulation$sim_out$Dose[29:NROW(simulation$sim_out)]) # they say weekly dose
}

# 3 The chronic (> 30 days) warfarin dose that led to an INR in the
# therapeutic target range on several occasions.
stable_def_3 = function (simulation) { 
  # Assumption: several = 3
  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5

  exe(c( group_by_dose()
        )
  ,c( filter_dfs_by_days_elapsed(30)
     , filter_dfs_by_stable_checks(3, inr_low, inr_high)
     )
  , choose_by_days_elapsed
  )(simulation$sim_out) 
}


# 4 Warfarin therapeutic dose was defined as dose given when patients reach
# stable therapeutic INR. Stable therapeutic INR was defined as at least two
# consecutive INR measurements between 1.7-3 on the same warfarin daily or
# weekly dose measured at least one week apart.
stable_def_4 = function (simulation) { 
  exe(c( group_by_dose()
        )
  ,c( filter_dfs_by_consecutive_stable_checks(2, 1.7, 3)
     )
  , choose_by_days_elapsed
  )(simulation$sim_out)
}

# 5 Warfarin Therapeutic Dose was defined as a single mean weekly warfarin dose
# that was calculated by averaging the warfarin dose at each of the 3
# consecutive clinic visits. A Stable Weekly Maintenance Dose of Warfarin was
# defined as a dose that did not vary by more than 10% between clinic visits.
# In addition, the INR at each of the 3 visits had to be in the patient's specific
# goal INR range. 
stable_def_5 = function (simulation) { 
  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5

  exe(c( group_by_similar_dose(0.1)
        )
  ,c( filter_dfs_by_consecutive_stable_checks(3, inr_low, inr_high)
     )
  , choose_by_days_elapsed
  )(simulation$sim_out)
}

# 6 The warfarin dose that led to an INR in the therapeutic
# range (2-3) on at least 3 consecutive clinic visits over a minimum period of 3
# months.
stable_def_6 = function (simulation) { 
  exe(c( group_by_dose()
        )
  ,c( filter_dfs_by_days_elapsed(90)
     , filter_dfs_by_consecutive_stable_checks(3, 2, 3)
     )
  , choose_by_days_elapsed
  )(simulation$sim_out)
}


# 7 Two consecutive INRs in the target range of 2-3.5 while on a constant dose
# where INR measures are taken at least 3 days but less than 8 days apart.
stable_def_7 = function (simulation) { 
  exe(c( group_by_dose()
       )
     ,c( filter_dfs_has_check_gte_days_apart(3)
       , filter_dfs_has_check_lte_days_apart(8)
       , filter_dfs_by_consecutive_stable_checks(2, 2, 3.5)
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 8 Stable dose in our data set is defined as the dose of 3 consecutive clinic
# visits, within therapeutic range of INR, the same daily dose over 3 months
# based on Higashi et al., JAMA 2002 (PMID 11926893).
stable_def_8 = function (simulation) { 
  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5

  exe(c( group_by_dose()
       )
     ,c( filter_dfs_by_consecutive_stable_checks(3, inr_low, inr_high)
       , filter_dfs_by_days_elapsed(90)
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 9 INR between 2 and 3 for a period >1 month.
stable_def_9 = function (simulation) { 
  exe(c( filter_unchecked_days()
       , group_by_inr_stability(2, 3)
       , filter_unstable_inr(2, 3) 
       )
     ,c( filter_dfs_by_days_elapsed(30)
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 10 The definition of stable dose reported here is as follows: Dose at which
# INR was within therapeutic range (+/- 0.2 INR units) on 3 consecutive visits, with <90
# days between subsequent visits.
stable_def_10 = function (simulation) { 

  inr_low  = simulation$avatar$TINR - 0.2
  inr_high = simulation$avatar$TINR + 0.2

  exe(c( group_by_dose()
       )
     ,c( filter_dfs_by_consecutive_stable_checks(3, inr_low, inr_high)
       , filter_dfs_has_check_lte_days_apart(90) # a bit redundent
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 11 The same warfarin dose for at least 3 consecutive clinic visits. No INR criteria to
# define stable dose was used because it was assumed that either the INR was in
# range at each visit or was not sufficiently out-of-range to elicit a change in dose.
stable_def_11 = function (simulation) { 
  exe(c( group_by_dose()
       )
     ,c( filter_dfs_by_checks(3)            
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}


# 12 A cross-sectional study of patients treated with warfarin for at least 2
# months and with relatively stable anticoagulation.
stable_def_12 = function (simulation) { 
  simulation$sim_out$Dose[90]
}

# 13 Patients whose warfarin dose requirement was 1.5 mg per day or less and
# had a stable warfarin dose requirement for at least 3 consecutive clinic
# visits with a target International Normalised Ratio (INR) of 2.0 to 4.0 and
# no apparent cause for low dose requirement such as drug interactions or
# liver disease.
stable_def_13 = function (simulation) { 
# Assumption, first phrase in nonsense
  exe(c( group_by_dose()
       )
     ,c( filter_dfs_by_consecutive_stable_checks(3, 2, 4)            
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 14 Dose that lead to stable INR over 3 visits.
stable_def_14 = function (simulation) { 

  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5

  exe(c( group_by_dose()
       )
     ,c( filter_dfs_by_stable_checks(3,inr_low, inr_high)            
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}


# 15 Warfarin therapeutic dose was defined as warfarin dose at stable anticoagulation.
# Stable anticoagulation was defined when 2 consecutive INR measurements done
# at least 7 days apart were within the desired therapeutic range (i.e. 2-3),
# while warfarin dose was not changed.
stable_def_15 = function (simulation) { 


  exe(c( group_by_dose()
       , filter_unchecked_days()
       , group_by_inr_stability(2,3)
       , filter_unstable_inr(2,3) 
       )
     ,c( filter_dfs_by_checks(2)            
       , filter_dfs_has_check_gte_days_apart(7)
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 16 Warfarin dose that lead to the target INR, usually 2-3 on 1 or more
# occasions over a minimum of 30 days.
stable_def_16 = function (simulation) { 
# assumption:
  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5

  exe(c( group_by_dose()
       )
     ,c( filter_dfs_by_stable_checks(1, inr_low, inr_high)            
       , filter_dfs_by_days_elapsed(30)
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 17 The dose of warfarin that led to an INR in the target range (2-3 for
#all indications except valve prosthesis; for valve prosthesis the range is
#2.5 - 3.5) on 3 consecutive measurements.
stable_def_17 = function (simulation) { 
  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5
  
  exe(c( group_by_dose()
       )
     ,c( filter_dfs_by_stable_checks(3, inr_low, inr_high)            
       , filter_dfs_by_days_elapsed(30)
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 18 INR within therapeutic range of between 2-3 months for at least 3 months.
stable_def_18 = function (simulation) { 
  exe(c( group_by_dose()
       , filter_unchecked_days()
       , group_by_inr_stability(2,3)
       , filter_unstable_inr(2,3) 
       )
     ,c( filter_dfs_by_days_elapsed(90) )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 19 Same dose on 3 or more consecutive clinic visits, with INR within therapeutic
# range.
stable_def_19 = function (simulation) { 
  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5

  exe(c( group_by_dose()
       , filter_unchecked_days()
       , group_by_inr_stability(inr_low,inr_high)
       , filter_unstable_inr(inr_low,inr_high) 
       )
     ,c( filter_dfs_by_checks(3) )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}

# 20 Subjects were required to be on warfarin and managed in the warfarin clinic for a
# minimum of 3 months before entry into the study, and thus dosing had
# stabilized by the time data collection for the study began. Values reported
# for warfarin dose are the average over the entire time of participation in
# the study (average = 20.6 months). 
stable_def_20 = function (simulation) { 
  mean(simulation$sim_out$Dose)
}

# 21 Stable dose defined as the dose that leads to a stable INR over three
# consecutive visits following initiation of the drug, with these INR
# measurements encompassing a period of at least 2 weeks, with a maximum
# difference between the mean daily dosages of 10%.
stable_def_21 = function (simulation) { 
  inr_low  = simulation$avatar$TINR - 0.5
  inr_high = simulation$avatar$TINR + 0.5

  exe(c( group_by_similar_dose(0.1)
       )
     ,c( filter_dfs_by_consecutive_stable_checks(3, inr_low, inr_high)
       , filter_dfs_by_days_elapsed(14)
       )
     , choose_by_days_elapsed
     )(simulation$sim_out)
}


stable_def_22 = function(simulation) {
  pro_site     = simulation$avatar$PRO_SITE

  get_stable_def( pro_site )( simulation )
}


get_stable_def = function (project_site) {

  if(!is.numeric(project_site)) {
    alpha = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"
               , "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U"
               , "V", "W", "X", "Z")
    project_site = which(alpha == as.character(project_site))
  }

  get(paste("stable_def_", as.character(project_site), sep = ""))

} 

# Pipeline operator for stabe dose
stable_dose = function(simulation_out, force_def = 22) {
  s_def = get_stable_def(force_def)

  Map( function(avatar) {
         flattened_sim_out =
          Map( function(sim_out) {
                  avatar$sim_out = as.data.frame(sim_out)
                  avatar
               }
             , avatar$sim_out) 

				if(force_def == 22) {
				 TD_name = "td"
				} else {
				 TD_name = paste("td", force_def, sep="-")
				}
         avatar$outcome[[TD_name]] = unlist(Map(s_def, flattened_sim_out))
         avatar
       }
     , simulation_out )
}
