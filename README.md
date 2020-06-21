# CopulaControl
Simulate Bivariate Copulas and Compare Control Chart Performance for them.

## Running Script
To run, open R from this directory and enter the command `source('./run.R', local = T)`.

## Contents of files

### run.R
	- wrapper to run entire script.
	- Last line, creates an RDS file to save data for later analysis. When done with defaults, this will be about 3.2GB. Comment out to not create this file.

### dependencies.R
	- checks if all required packages are installed and installs those missing via CRAN

### library.R
	- imports packages and creates helper functions.

### parameters.R
	- sets experimental parameters. Not all work from here.
	- Details on setting them documented in this file.
	- Change `registerDoMC(n)` to run using `n` threads.

### generator.R
	- Generates data and test statistics for experimental combinations

### analysis.R
	- Computes the UCL and ARL of experimental combinations
	- This version is the automated, but incorrectly calibrated one. Best I have been able to do is manually adjust them, which I include in my copy.
