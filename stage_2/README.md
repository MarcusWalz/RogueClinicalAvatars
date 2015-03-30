INR and Dose Simulation 
---------------------------

Bind Simulation Params and Data Sets, split data set.

We entangle the simulation paramaters to the avatars
themselves. This way running the computation heavy simulation
is essentially stateless.

First include warfkit and read in the data frame:
```
library(warfkit)
my_avatars = read.delim("iwpc_avatars.txt")
```

Then filter the things we don't want, in our case things with
the `CY2CP9` field set to `Missing`:
```
my_avatars = my_avatars[my_avatars$CY2CP9 != "Missing",]
```

Specify the simulation params, at minimum it should contain:

```
my_simulation =
  list ( days = 90                         # number of days to run sim
       , max_dose = 100                    # maximum dose in mg
       , max_time = 24                     # TODO ???
       , protocol = "ahc_clinical"         # protocol being used
       , initial_dose  = "pginitial_IWPC"  # initial dose formula
       , replicates = 1                    # number of replicates per avatar
       )

```

Then use `preprocess_avatars` to bind simulation paramaters to each 
avatar:

```
simulation_input = preprocess_avatars(my_avatars, my_sim) 
```

Then run the simulation by mapping the function `process_avatar` over `simulation_input`:

```
simulation_output = Map(process_avatar, simulation_input)
```


Or on a parallel HPC enviroment, such as `SLURM` split
`simulation_output` and then map over the output using:

```
split_avatars(simulation_input, prefix="iwpc_1", chunk_size=500)
```

This above snippet splits simulation_input into chunks of 500 avatars. 

Now use a script like `slurm_submit.sh` ot run the script stupidly
parallel on a cluster:


```
sbatch --array 1-250 slurm_submit.sh iwpc_1
```

Then merge the output back together:
```
simulation_output = combine_avatars(prefix="iwpc_1")
```


Don't forget to save `simulation_output`:

```
write(simulation_output, file="iwpc_1.Rdata")
```
