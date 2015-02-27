
1. Bootstrap dataset:
  ``` 
  Rscript bootstrap.R avatars.txt
  ```
  **Outputs avatars.txt.bootstrapped**

2. Subset avatars 

  ```
  Rscript subset.R avatars.txt.bootstrapped 5 10
  ```

  **Outputs `avatars.txt.bootstrapped.[1-5].subset` and
  `avatars.txt.bootstrapped.[1-5].discard`**

  Creates 5 samplings with 10 random avatars each.

3. Generate BNM and Produce Avatars 

  ```
  ./make_avatars.sh avatars.bootstrapped.1.subset 1000
  ```

  - Creates 1000 avatars. In the file `avatars.txt.boostrapped.1.bnm `

  - Outputs prob table used to generate the avatars. In the file:
  `avatars.txt.bootsrapped.1.ptable.rb` and 
  `avatars.txt.bootstrapped.1.ptable.Rdata`


4. Merge tables into one:
  ```
  Rscript merge.R output_avatars.txt avatars.txt.bootstrapped.*.bnm
  ```

  merges everything matching the glob into `output_avatars.txt`
