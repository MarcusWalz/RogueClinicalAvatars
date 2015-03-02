
1. Bootstrap dataset:

  ``` 
  Rscript bootstrap.R < avatars.txt > bs_avatars.txt
  ```

2. Subset avatars 

  ```
  Rscript sample.R --percent 10 < bs_avatars > sub_avatars
  ```

  Sample 10% of the avatars randomly.

  **or**

  ```
  Rscript sample.R --avatars 100 < bs_avatars > sub_avatars
  ```

  Sample 100 avatars.

  Use the `--inverse` flag to stream unsampled avatars to 'stderr' e.g.
  in bash:

  ```
  Rscript sample.R --avatars 100 < my_avatars > sampled_avatars 2> unsampled_avatars
  ```


3. Generate BNM and Produce Avatars 

  ```
  ./make_avatars.sh 1000 < sub_avatars > sub_avatars.gen
  ```

4. Merge tables into one:

  ```
  Rscript merge.R sub_avatars.*.gen > avatars.txt 
  ```

  Merges everything matching the glob into `output_avatars.txt`

Putting it all together:

```
./bootstrap.R < avatars | ./sample.R --percent .80 | ./train_avatars | ./make_avatars 100 \
> my_avatars

```
