#!/bin/bash
source "${GITHUB_WORKSPACE}/.github/scripts/shutils.sh"
###############################################################################
# Install xform specific dependencies.
###############################################################################

installAptPackages r-cran-tidyverse

R -e "remotes::install_github('covid-open-data/geoutils')"

exit 0
