
<!-- README.md is generated from README.Rmd. Please edit that file -->

# djprdashdata

<!-- badges: start -->

[![R-CMD-check](https://github.com/djpr-data/djprdashdata/workflows/R-CMD-check/badge.svg)](https://github.com/djpr-data/djprdashdata/actions)
[![refresh-data](https://github.com/djpr-data/djprdashdata/workflows/refresh-data/badge.svg)](https://github.com/djpr-data/djprdashdata/actions)

<!-- badges: end -->

## Overview

This R package contains functions to download, tidy, and store data from
the ABS and other public data sources.

The `data-raw/refresh_data.R` file contains code to download public data
using the package’s functions. The data itself is saved in `data-raw`.

This public data is used in the DJPR Jobs Dashboard. See:

  - The [`djprlabourdash`](https://github.com/djpr-data/djprlabourdash)
    GitHub repo for the Jobs Dashboard code; and

  - The [live dashboard](https://djpr-spp.shinyapps.io/djprlabourdash/).

## Last refresh date

The `refresh_data.R` script is executed on a schedule using GitHub
Actions.

The last time the script was run to check for new data was 2022-01-05
13:25:23 Melbourne time.

The last time new data was found was 2021-12-23 22:42:01 Melbourne time.

## Modifying the data stored in this repository

The file `abs-lfs.qs` in the `data-raw/abs-ts` sub-folder of this
repository is the data loaded by the DJPR Jobs Dashboard.

This is a subset of the publicly-available ABS labour force survey data.
The file is loaded by the dashboard using the
`djprlabourdash::load_dash_data()` command.

The `refresh_data.R` script defines which series from the labour force
survey are included in `abs-lfs.qs` and therefore available to the DJPR
Jobs Dashboard. To add time series to this file, follow these steps:

1.  Ensure you have the `djprdashdata` repository on your local machine.
    You only need to do this step once. In RStudio, click `File` -\>
    `New Project` -\> `Version Control` -\> `Git`. Then paste the GitHub
    repository URL for `djprdashdata` and choose a location for the
    project on your disk;

2.  Create a new branch on GitHub in the `djprdashdata` from the `main`
    branch;

3.  In RStudio, working in the `djprdashdata` project, click the `Git`
    pane, then `Pull`;

4.  In RStudio, in the `Git` pane, switch to your newly-created branch;

5.  Open the `refresh_data.R` file in the `data-raw` folder;

6.  At the top of the script, a vector of ABS time series IDs is
    defined. This is a complete list of all ABS time series IDs that are
    included in the dashboard data. Add any additional required IDs to
    this vector;

7.  Save the `refresh_data.R` file;

8.  With `refresh_data.R` open, click `Source` in RStudio. This should
    be at the top of the script window;

9.  R will now run the `refresh_data.R` script. If it does not encounter
    any errors, the data in the project will update. You should see
    console output that ends with something like this;
    
    ![](images/Screen%20Shot%202021-07-28%20at%2010.55.44%20am.png)

10. In the `Git` pane of RStudio, stage and commit all modified files,
    then `Push` the changes;

11. In `GitHub` initiate a pull request from your branch to `main` and
    request a review of this pull request.

Once the PR has been approved, your modifications to the data will be
available to the DJPR Jobs Dashboard.
