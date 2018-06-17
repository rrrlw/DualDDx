## The accuracy of computer-based diagnostic tools for the identification of concurrent genetic disorders

### Purpose

Concurrent genetic disorders occur more commonly that previously thought.
This GitHub repository includes a dataset, Results.csv, and R code, Process.R, that reproduce the plots used in this project.

### How to reproduce the plots

Here, we assume a Unix environment (Mac OS or Linux), but the script below can be adapted for Windows machines.
The following software should be installed on your computer:

* git
* R (>= 3.5.0)

The following script creates a local copy of the data and code on your machine, and then runs the R script to recreate the appropriate plots.
Note that individual panels of compound plots might be stored as individual image files.

```shell
# create a local copy of data and code
git clone https://github.com/rrrlw/DualDDx.git

# change directory into newly created folder
cd DualDDx

# run R code that recreates plots
Rscript Process.R
```

Please report any issues, comments, or suggestions on the [issues page](https://github.com/rrrlw/DualDDx/issues) of this GitHub repository.
