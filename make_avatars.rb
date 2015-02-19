prob_table = 
  { "for" => "WEIGHT" \
    , "on_fields" => {"AGE" => "5", "GENDER" => "M"} \
    , "freqs" => {"*" => 30, "1" => 20, "2" => 10}
} 
  prob_table2= 
    { "for" => "WEIGHT" \
      , "on_fields" => {"AGE" => "5", "GENDER" => "M", "SOMETHING" => "A"} \
      , "freqs" => {"*" => 10, "1" => 30, "2" => 20}
  } 

prob_tables=eval(File.open("prob_table.rb").read)


#TODO use frequencies!!!!
#TODO not tested 
def prob_table_to_rand_value(prob_table, drop_missing=false)
  if prob_table == nil then return "No p-table" end
  l = []
  prob_table["probs"].each do |value,freq| 
    unless value == "*" and drop_missing then
      l += [value] * (freq * 100).floor
    end
    end

    if l.length >= 1 then l.sample() else "No non-missing" end

  end

def matches_avatar(prob_table,avatar) 
  if(prob_table == {}) then return true end
  prob_table["on_fields"].each do |key,value|
    if avatar[key] != value then return false end
  end

  true
end

  def intersect_avatars(a,b) 
    a.reject { |k,v| v != b[k] }
  end

  def calc_specificity(prob_table, avatar) 
    intersect_avatars(prob_table["on_fields"],avatar).length
  end

  #O(n)
  def lookup_prob_table(prob_tables, avatar) 
    # keep tables that match avatar


    prob_tables = prob_tables.select { |p_table| matches_avatar(p_table,avatar)}

    best_table = nil
    specificity = -1


    prob_tables.each do |prob_table| 
      my_spec = calc_specificity(prob_table, avatar)

      if(my_spec > specificity)
        specificity = my_spec
        best_table = prob_table
      end
    end

    best_table
  end

def set_value(prob_tables, avatar) 
  best_table = lookup_prob_table(prob_tables, avatar)

  avatar[prob_tables.first["for"]] = prob_table_to_rand_value(best_table)
  return avatar
end

def make_avatars(prob_tables, n) 
  n.times do 
    avatar = {}
    prob_tables.each do |p_tables|
      avatar = set_value(p_tables, avatar)
    end
    puts avatar.values.join("\t")
  end
end

puts prob_tables.length
puts calc_specificity(prob_tables[0][0],{})
# puts prob_tables[0]
#puts spec(prob_tables[0],{})

make_avatars(prob_tables,100)

#puts lookup_prob_table(prob_tables, {"AGE" => "5", "GENDER" => "M"})
#
