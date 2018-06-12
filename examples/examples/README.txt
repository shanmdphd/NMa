This directory contains the files needed for  the following paper:

Implementation of dose superimposition to introduce multiple doses
for a mathematical absorption model (transit compartment model)

Jun Shen, Alison Boeckmann & Andrew Vick
Journal of Pharmacokinetics and Pharmacodynamics
ISSN 1567-567X Volume 39 Number 3
J Pharmacokinet Pharmacodyn (2012) 39:251-262 DOI 10.1007/s10928-012-9247-3

The files are:
README.txt (The file you are reading)

Special Case (SC)
The following version of the model uses abbreviated code, presently coded up to 7 doses. 
For more doses, add additional lines pertaining to dose processing, as indicated by the comments.
sumdoset.ctl, which is appendix 3 of the paper, and uses 
sumdoset.csv, the data file for sumdoset.ctl (sall6abbr.csv of the paper)
abbrtable.txt: table of results from sumdoset.ctl

General Case (GC)
The following version of the model uses abbreviated functions (FUNCA, see Guide VIII of NONMEM Users Guides), 
which handles any number of doses without requiring code changes.
sumdosetf.ctl, which is appendix 4 of the paper
sumdosetf.csv, the data file for sumdosetf.ctl (sall6.csv of the paper)
sumdoset.f90, which is appendix 5 of the paper
functable.txt: table of results from sumdosetf.ctl

One additional file is included:
sumdoset2.f90, which is an extended version of sumdoset.f90, computing second derivatives, 
               and can be used for any method, including LAPLACE.

---
Alison Boeckmann
July 2012
