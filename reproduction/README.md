# Reproduction README

<!-- TODO: Remove this warning once filled out README -->
**Please note: This is a template README and has not yet been completed**

<!-- TODO: Fill out the README -->
## Model summary

TBC

## Scope of the reproduction

TBC

## Reproducing these results

### Repository overview

TBC

### Step 1. Set up environment

TBC

### Step 2. Running the model

TBC

## Reproduction specs and runtime

Within this reproduction, due to long run times, the model was run on a remote machine. This was an Intel Core i9-13900K with 81GB RAM running Pop!_OS 22.04 Linux. We also reduced the number of patients in the simulation from 10 million to 1 million, to improve run times. In total, it took **6 hours 53 minutes** to run all the model scenarios. This included:

* 65 year old scenario 0 - 3 minutes 32 seconds (212 seconds)
* 65 year old scenario 1 - 29 minutes 14 seconds (1754 seconds)
* 65 year old scenario 2 - 19 minutes 7 seconds (1147 seconds)
* Surveillance scenario 0 - 4 minutes 28 seconds (268 seconds)
* Surveillance scenario 1 - 1 hour 10 minutes 11 seconds (4211 seconds)
* Surveillance scenario 2 - 1 hour 39 minutes 39 seconds (5979 seconds)
* Surveillance scenario 3 - 1 hour 21 minutes 40 seconds (4900 seconds)
* Surveillance scenario 4a - 35 minutes 23 seconds (2123 seconds)
* Surveillance scenario 4b - 34 minutes 57 seconds (2097 seconds)
* Surveillance scenario 4c - 35 minutes 14 seconds (2114 seconds)


<!-- 212 + 1754 + 1147 + 268 + 4211 + 5979 + 4900 + 2123 + 2097 + 2114 = 24805 seconds = 413 minutes 25 seconds = 6 hours, 53 minutes, 26 seconds

Excluded these which I had run but which ended up not actually being needed for the paper:
* Surveillance scenario 4d - 35 minutes 5 seconds (2105 seconds)
* Surveillance scenario 4e - 34 minutes 56 seconds (2096 seconds) -->

## Citation

TBC

## License

TBC