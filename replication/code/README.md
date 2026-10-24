# Code guide

Within each script, settings and input paths appear at the top. 

Run scripts from the repository root and follow the order in the main README.
The code performs:

- construction of analysis data;
- conditional-logit estimation;
- first-stage fixed-effects estimation;
- bootstrap correction;
- second-stage regressions;
- welfare calculations;
- figures and table assembly;
- the independent Monte Carlo experiment.

`code/03_bootstrap/02_single_replica_2stage_murdock.R` is an optional diagnostic
that runs one bootstrap replication. It is useful for checking the environment
before starting the full 500-by-1,000 bootstrap.
