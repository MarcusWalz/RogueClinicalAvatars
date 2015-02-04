

# Calculate if ttr is in theraputic range 
# using min/max function.
theraputic_range = function(min, max) {
  function(ttr) {
    (ttr >= min  * ttr <= max) == 0 
  } 
}

# Calculate the theraptuic range using avatar's target INR (TINR).
theraputic_range_tinr = function(avatar, plus_minus=0.5) {
  theraputic_range(avatar$TINR - plus_minus, avatar$TINR + plus_minus)
}
