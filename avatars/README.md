
1. Bootstrap dataset:

  ``` 
  Rscript bootstrap.R avatars.txt
  ```
  **Outputs avatars.txt.bs**

2. Subset avatars 

  ```
  Rscript subset.R avatars.txt.bs 5 10
  ```

  **Outputs `avatars.txt.bs.[1-5].subset` and
  `avatars.txt.bs.[1-5].discard`**

  Creates 5 samplings with 10 random avatars each.

3. Generate BNM and Produce Avatars 

  ```
  ./make_avatars.sh avatars.bs.1.keep 1000
  ```

  - Outputs BNM prob table used to generate the avatars. In the file:
  `avatars.txt.bootsrapped.1.ptable.rb` and 
  `avatars.txt.bs.1.ptable.Rdata`

  - Creates 1000 avatars using BNM. In the file `avatars.txt.bs.1.bnm `



4. Merge tables into one:

  ```
  Rscript merge.R output_avatars.txt avatars.txt.bs.*.bnm
  ```

  merges everything matching the glob into `output_avatars.txt`
