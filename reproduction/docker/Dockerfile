# Get base image from rocker
FROM rocker/rstudio:4.4.1

# Set version of packages installed outside the lockfile
ENV STRINGI_VERSION '1.8.4'

# Install system dependencies
RUN apt-get update && \
	apt-get install -y \
	libcurl4-openssl-dev \
	libssl-dev \
	libxml2-dev \
	libglpk-dev \
	libicu-dev

# Set user to rstudio, so we have access to the renv/ and files
USER rstudio

# Set project directory (otherwise files will copy into an inaccessible location,
# as the rstudio rocker image home is this path)
ENV PROJDIRECTORY "/home/rstudio/"
WORKDIR ${PROJDIRECTORY}

# Copy desired files (inc. renv.lock)
COPY --chown=rstudio ./reproduction/DESCRIPTION DESCRIPTION
COPY --chown=rstudio ./reproduction/docker docker
COPY --chown=rstudio ./reproduction/functions functions
COPY --chown=rstudio ./reproduction/input input
COPY --chown=rstudio ./reproduction/kim2021.Rproj kim2021.Rproj
COPY --chown=rstudio ./reproduction/models models
COPY --chown=rstudio ./reproduction/output output
COPY --chown=rstudio ./reproduction/process_results process_results
COPY --chown=rstudio ./reproduction/README.md README.md
COPY --chown=rstudio ./reproduction/renv.lock renv.lock
COPY --chown=rstudio ./reproduction/tests tests

# Copy renv auto-loader tools
RUN mkdir -p renv/library
COPY ./reproduction/.Rprofile .Rprofile
COPY ./reproduction/renv/activate.R renv/activate.R

# Install seperately before restore (due to issues with finding system dependencies)
# Running the `renv::` commands will automatically install renv for us
RUN R -e "renv::install('stringi@${STRINGI_VERSION}')"

# Install remaining packages
RUN R -e "renv::restore(lockfile='${PROJDIRECTORY}/renv.lock', repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest'))"

# Set user back to root
USER root