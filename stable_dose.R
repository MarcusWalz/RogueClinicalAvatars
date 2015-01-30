
  
# 
# ARGS:
#  definiton: Integer between one through twenty
# RETURNS:
#  a function that takes the following three arguments:
#  avatar
#  inr:   Vector containing INR data
#  dose:  Vector containing dose data
#  check: Vector containing check data
  


# Definition of Stable Dose of Warfarin for Each Research Group

# 1 The dose (unchanged for 6 days) that yielded an INR within 0.5 of the
# target INR.
stable_def_1 = function (avatar, simulation_results) { 

}

# 2 Average weekly dose (irrespective of achieved INR) that the patient received
# during the observation period, excluding the first 28 days after warfarin initiation.
stable_def_2 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 3 The chronic (> 30 days) warfarin dose that led to an INR in the
# therapeutic target range on several occasions.
stable_def_3 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 4 Warfarin therapeutic dose was defined as dose given when patients reach
# stable therapeutic INR. Stable therapeutic INR was defined as at least two
# consecutive INR measurements between 1.7-3 on the same warfarin daily or
# weekly dose measured at least one week apart.
stable_def_4 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 5 Warfarin Therapeutic Dose was defined as a single mean weekly warfarin dose
# that was calculated by averaging the warfarin dose at each of the 3
# consecutive clinic visits. A Stable Weekly Maintenance Dose of Warfarin was
# defined as a dose that did not vary by more than 10% between clinic visits.
# In addition, the INR at each of the 3 visits had to be in the patient's specific
# goal INR range. 
stable_def_5 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 6 The warfarin dose that led to an INR in the therapeutic
# range (2-3) on at least 3 consecutive clinic visits over a minimum period of 3
# months.
stable_def_6 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}


# 7 Two consecutive INRs in the target range of 2-3.5 while on a constant dose
# where INR measures are taken at least 3 days but less than 8 days apart.
stable_def_7 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 8 Stable dose in our data set is defined as the dose of 3 consecutive clinic
# visits, within therapeutic range of INR, the same daily dose over 3 months
# based on Higashi et al., JAMA 2002 (PMID 11926893).
stable_def_8 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 9 INR between 2 and 3 for a period >1 month.
stable_def_9 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 10 The definition of stable dose reported here is as follows: Dose at which
# INR was within therapeutic range (+/- 0.2 INR units) on 3 consecutive visits, with <90
# days between subsequent visits.
stable_def_10 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 11 The same warfarin dose for at least 3 consecutive clinic visits. No INR criteria to
# define stable dose was used because it was assumed that either the INR was in
# range at each visit or was not sufficiently out-of-range to elicit a change in dose.
stable_def_11 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}


# 12 A cross-sectional study of patients treated with warfarin for at least 2
# months and with relatively stable anticoagulation.
stable_def_12 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 13 Patients whose warfarin dose requirement was 1.5 mg per day or less and
# had a stable warfarin dose requirement for at least 3 consecutive clinic
# visits with a target International Normalised Ratio (INR) of 2.0 to 4.0 and
# no apparent cause for low dose requirement such as drug interactions or
# liver disease.
stable_def_13 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 14 Dose that lead to stable INR over 3 visits.
stable_def_14 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 15 Warfarin therapeutic dose was defined as warfarin dose at stable anticoagulation.
# Stable anticoagulation was defined when 2 consecutive INR measurements done
# at least 7 days apart were within the desired therapeutic range (i.e. 2-3),
# while warfarin dose was not changed.
stable_def_15 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 16 Warfarin dose that lead to the target INR, usually 2-3 on 1 or more
# occasions over a minimum of 30 days.
stable_def_16 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 17 The dose of warfarin that led to an INR in the target range (2-3 for
#all indications except valve prosthesis; for valve prosthesis the range is
#2.5 - 3.5) on 3 consecutive measurements.
stable_def_17 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 18 INR within therapeutic range of between 2-3 months for at least 3 months.
stable_def_18 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 19 Same dose on 3 or more consecutive clinic visits, with INR within therapeutic
# range.
stable_def_19 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 20 Subjects were required to be on warfarin and managed in the warfarin clinic for a
# minimum of 3 months before entry into the study, and thus dosing had
# stabilized by the time data collection for the study began. Values reported
# for warfarin dose are the average over the entire time of participation in
# the study (average = 20.6 months). 
stable_def_20 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

# 21 Stable dose defined as the dose that leads to a stable INR over three
# consecutive visits following initiation of the drug, with these INR
# measurements encompassing a period of at least 2 weeks, with a maximum
# difference between the mean daily dosages of 10%.
stable_def_21 = function (avatar, inr, dose, check) { 
  FALSE #TODO
}

getStableDefinition = function (group_number) {

  get(paste("stable_def_", as.character(group_number), sep = ""))

} 
