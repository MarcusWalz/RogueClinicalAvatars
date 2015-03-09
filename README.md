ClinicalAvatars
===============


Files
--------------------

- `avatars` Bayesian Network Model Tool for generating avatars (to be merged to master)
- `data/avatars.R` Contains avatar validator based on spec described in `initial_dosing.R`
- `test/`  
  - `factories/` factories for generating random simulation input and output
  - `unit/` unit tests for various scripts (currently just `stable_dose.R`)
- `simulator.R` the actual simulator
- `initial_dose.R` initial dosing formulas
- `maintenance_protocols.R` dosing protocols
- `run_sim.R` simulation execution script.


TODO
----------------------

- Finalize the minimum def of an avatar and double check that the code complies with this def.
  My proposol is in `data/avatar.R`

- Validate avatars prior to simulation execution.

- Pass simulation paramaters as an R list, e.g.:

  ```
  simulation =
    list( initial_dose = "my_initial_dose"  # or a number for a preset initial dose
        , protocol     = "my_protocol"
        , max_dose     = "15"
        , days         = 100
        , seed         = 4321
        )
  ```

- Modify `simulator.R` so `process_avatar` handles 1 avatar at a time and
  `process_avatars` maps process_avatars over many.
- Modify `maintenance_protocols.R` s.t. each protocol so that:

  All protocols end in `_protocol` so we can safely fetch a protocol using
  `ls()` and `get()`.

  ```
  my_protocol = function(avatar, simulation) { 
    # if a protocol requires auxilary fields.
    # check the avatar before returning protocol. 

    # if the avatar is invalid, write an error message and terminate.

    function(INR, dose, day) {
      ...
    }
  }
  ```

  This way we can avoid that huge if/else statement in `simulator.R`
  and keep all protocol specific logic inside `maintenance_protocols.R`.

- Do something similar for `initial_dose.R`.
