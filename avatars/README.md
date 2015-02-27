
1. Bootstrap dataset:

  ``` 
  Rscript bootstrap.R < avatars.txt > bs_avatars.txt
  ```

2. Subset avatars 

  ```
  Rscript sub_avatars sub_avatars 5 10 < bs_avatars
  ```

  Creates 5 samplings with 10 random avatars each.

  **Outputs `sub_avatars.[1-5]` and
  `sub_avatars.[1-5].discard`**

  where `sub_avatars.n + sub_avatars.n.discard` = bs_avatars


3. Generate BNM and Produce Avatars 

  ```
  ./make_avatars.sh 1000 sub_avatars.1.keep
  ```

  - Outputs BNM prob table used to generate the avatars. In the file:
  `sub_avatars.1.ptable.rb` and 
  `sub_avatars.1.ptable.Rdata`

  - Creates 1000 avatars using BNM. In the file `sub_avatars.1.bnm `

4. Merge tables into one:

  ```
  Rscript merge.R sub_avatars.*.bnm > output_table.txt 
  ```

  merges everything matching the glob into `output_avatars.txt`
