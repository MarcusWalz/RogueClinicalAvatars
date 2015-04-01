ClinicalAvatars + Warfarin Simulation 
=====================================

Avatars
------------------

To install avatars source the following script in shell:

```
source install_avatars
```

Then the avatar related scripts should be in your PATH.


Warfkit
------------------

Warfkit is our Rpackage where are anticogulatent specific code lives.
Code in `stage_2` and `stage_3` both depend on it.

To install Warfkit, in terminal and from this very directory,
run the following command to build and install the Rpackage:

```
./install_warfkit
```


Then you should be able to import it in the `R` console like so:

```
library(warfkit)
```

Warfkit includes dosing protocols, outcome metrics, and the Hamberg 2007
Warfarin prediction model. It's great.

For a guide on how to preprocess avatars and run simulations see
`stage_1/`'s README.

And for a guide on how to run outcome metrics on simulation output,
see `stage_2`'s README.
