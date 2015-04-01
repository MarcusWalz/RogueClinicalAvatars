#!/bin/bash

# tab delimited txt file
training_data=iwpc.txt

# DAG as a tab delim text file
dag=iwpc_dag.txt

for i in {1..5}
do

  # bootstrap avatars
  bootstrap_avatars < $training_data > bootstrapped.$i.txt

  # sample. Unsampled scripts get sent to stderr
  sample_avatars --percent 0.8 --inverse < bootstrapped.$i.txt \
    > sampled.$i.txt 2> sampled_rej.$i.txt

  # train avatars
  train_avatars $dag sample.$i.txt cpt.$i.Rdata

  # generate 200,000 avatars using the cpt
  make_avatars cpt.$i.Rdata 200000 > avatars.$i.txt
done

# merge together avatars
merge_avatars avatars.{1..5}.txt > avatars_merged.txt

# train using merged data
train_avatars $dag $avatars_merged merged_cpt.Rdata

# generated final avatars
make_avatars merged_cpt.Rdata 2200000
