#!/bin/bash

# Convert prob table to R script
Rscript make_avatars.R $1 > $1.rb
ruby make_avatars.rb $1.rb $2
