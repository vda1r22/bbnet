## Resubmission

This is a resubmission of the package.

- Updated the package version from 1.0.1 to 1.1.0

## R CMD check results

0 errors ✔ | 1 warning ✖ | 1 note ✖

## Comments

- Updated bbn.predict, bbn.sensitivity, bbn.visualise and bbn.timeseties to allow for zeros in the interaction matrix.
- Updated bbn.sensitivity function to print an error message if spelling of key variables is wrong.
- Updated bbn.visualise and bbn.timeseries to stop function if priors do not follow structure (Increase, Node).
- Updated bbn.predict, bbn.visualise and bbn.timeseties to stop function if the first column of priors does not contain numeric data.
- Updated bbn.predict, bbn.visualise and bbn.timeseties to warn of different ordering of nodes between priors and interaction matrix.
