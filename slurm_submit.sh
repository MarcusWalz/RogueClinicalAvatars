# arg1 = prefix
prefix=$1

Rscript simulator.R $prefix.$SLURM_ARRAY_TASK_ID.avatars.Rdata \
                    $prefix.$SLURM_ARRAY_TASK_ID.avatars_simed.Rdata

