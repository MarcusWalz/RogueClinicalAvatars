
parts/% : avatars.txt settings.R
	mkdir parts
	Rscript split_df.R 

output/%.out : parts/%.txt settings.R
	# salloc -c 8 -J avatar srun 
	Rscript ../../run_sim.R settings.R $< $@
