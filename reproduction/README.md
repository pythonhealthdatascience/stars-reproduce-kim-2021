# Reproduction README

## Model summary

This study adapts a previously developed discrete-event simulation model for abdominal aortic aneurysm (AAA) screening of men in England. It aims to explore different approaches to resuming screening and surgical repair for AAA, as these survives were paused or substantially reduced during COVID-19 due to concerns about virus transmission.

## Scope of the reproduction

In this assessment, we attempted to reproduce 10 items: 6 figures, 3 tables and 1 in-text result.

## Reproducing these results

### Repository overview

```
├── docker
│   └──  ...
├── functions
│   └──  ...
├── input
│   └──  ...
├── models
│   └──  ...
├── output
│   └──  ...
├── process_results
│   └──  ...
├── renv
│   └──  ...
├── tests
│   └──  ...
├── .Rprofile
├── DESCRIPTION
├── README.md
├── kim2021.Rproj
└── renv.lock
```

* `docker/` - Instructions for creation of docker container.
* `functions/` - Discrete-event simulation model code
* `input/` - Input parameters for the model
* `models/` - Scripts to run each scenario
* `output/` - Output files from the scripts (e.g. `.csv`, `.png`)
* `process_results/`
* `renv/` - Instructions for creation of R environment
* `tests/` - Test to check that the model produces consistent results with our reproduction
* `.Rprofile` - Activates R environment
* `DESCRIPTION` - Lists packages that we installed into environment (their dependencies will have also been installed)
* `README.md` - This file!
* `kim2021.Rproj` - Project settings, which specify the Python virtual environment to use when building pages from the Quarto site that include Python. If you choose to build the Quarto site (and not just run the reproduction files in this folder), you will want to update this to a path on your machine (which you can do easily by opening this file in RStudio)
* `renv.lock` - Lists R version and all packages in the R environment

### Step 1. Set up environment

Before you can run the model, you will need to create an R environment with the correct version of R and the specified packages.

#### Option A. Renv

An `renv` environment has been provided. To create this environment locally on your machine, you should open the R project with the R environment loaded, and then run:

```
renv::restore()
```

In `renv.lock`, you will see the version of R listed. However, `renv` will not install this for you, so you will need to switch to this yourself if you wish to also use the same version of R. This reproduction has been run in R 4.4.1. If you use a different version of R, there's a chance that it might be incompatible, or that you may encounter difficulties installing the specified package versions in that version of R.

#### Option B. Build local docker image

A Dockerfile is provided, which you can use to build the Docker image. The docker image will include the correct version of R, the required packages and their versions, and an installation of RStudio which you can run from your browser. It will also include the scripts and outputs from this directory. For this option and option C, you'll need to ensure that `docker` is installed on your machine.

To create the docker image and then open up RStudio:

1. In the terminal, navigate to the parent directory of your `reproduction/` folder
2. Build the image:

```
sudo docker build --tag kim2021 . -f ./reproduction/docker/Dockerfile
```

3. Create container and open RStudio in your browser:

```
(sleep 2 && xdg-open http://localhost:8888) & sudo docker run -it -p 8888:8787 -e DISABLE_AUTH=true --name kim2021_docker kim2021
```

#### Option C. Pull pre-built docker image

A pre-built image is available on the GitHub container registry. To use it:

1. Create a Personal Access Token (Classic) for your GitHub account with `write:packages` and `delete:packages` access
2. On terminal, run the following command and then enter your sudo password (if prompted), followed by the token just generated (which acts as your GitHub password)

```
sudo docker login ghcr.io -u githubusername
```

3. Download the image:

```
sudo docker pull ghcr.io/pythonhealthdatascience/kim2021
```

4. Create container and open RStudio:

```
(sleep 2 && xdg-open http://localhost:8888) & sudo docker run -it -p 8888:8787 -e DISABLE_AUTH=true --name kim2021_docker ghcr.io/pythonhealthdatascience/kim2021:latest
```

### Step 2. Running the model

#### Option A: Execute the R scripts and markdown file

To run all the model scenarios, open and execute the provided `.R` files in `models/NAASP_COVID_modelling/`. You should run these using `source()`.

To process the model outputs and produce the tables and figures from the paper, run the file `process_results/process_results.Rmd`.

### Option B: Testthat

A small version of one the model scenarios is provided as a test within `tests/testthat`. You can run this scenario by running the following command from your R console whilst in the `reproduction/` directory:

```
testthat::test_dir("tests/testthat")
```

The test should only take about 5-10 seconds depending on your machine specs.

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

You can expect the runtime to be **notably longer** on machines with lower specs than this. For example, I ran surveillance scenario 0 on an Intel Core i7-12700H with 32GB RAM running Ubuntu 22.04.4 Linux. The runtime increased from **4 minutes 28 seconds** up to **21 minutes 59 seconds**.

<!-- 212 + 1754 + 1147 + 268 + 4211 + 5979 + 4900 + 2123 + 2097 + 2114 = 24805 seconds = 413 minutes 25 seconds = 6 hours, 53 minutes, 26 seconds

Excluded these which I had run but which ended up not actually being needed for the paper:
* Surveillance scenario 4d - 35 minutes 5 seconds (2105 seconds)
* Surveillance scenario 4e - 34 minutes 56 seconds (2096 seconds) -->

## Citation

To cite the original study, please refer to the reference above. To cite this reproduction, please refer to the CITATION.cff file in the parent folder.

## License

This repository is licensed under the GNU Lesser General Public License v3.0.