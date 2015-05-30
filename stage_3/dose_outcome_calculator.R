# Calculate Summary Statistics Based On Dose
# Statify genotype and race on outcome list

# Input Processed Avatars with outcome metrics applied

# Output summary tables of outcomes. Just mean/s.d. for 
# now!

library(warfkit)



# Turn this table into a predicates


calc_summary = function(avatars, groups, metric) {

	# convert the groups data frame to a list of predicates
	groups_to_preds = function(groupings) {
		apply(groupings, 1, function(vals) {
			vals # delete me and I'll break everything
			return(function(av) {
				all(sapply(colnames(groupings), function(colname) {	
					av$avatar[[colname]] == vals[[colname]]
				}
			))}
		)})
	}

	preds = groups_to_preds(groups)

  N = c()
	outcome=(sapply( preds, function(pred) {
		
		filtered_avs = Filter(pred, avatars)

		N <<- append(N, length(filtered_avs))
		
		# create an array of outcomes 
		outcome_matrix = Reduce(rbind, Map(function(av) av$outcome , filtered_avs))
#		print(outcome_matrix)

		apply(outcome_matrix, 2, metric)
		
	}))

	cbind(groups, cbind(N, t(outcome)))
}

# Creates a table with all factors
groups = expand.grid(VKORC1G=factor(c("A/A", "A/G", "G/G")), RACE=RACE) 

avatars = readRDS("all_avs.RDS")
metric = function(nums) { mean(nums, na.rm=T) }

print(calc_summary(avatars, groups, metric))
