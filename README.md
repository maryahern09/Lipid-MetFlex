# Lipid-MetFlex
Code used in the Lipid MetFlex Analysis

**Dataset Needed
1. Lipid MetFlex data set with anthropometrics, metabolites, clinical measures, time points, and categorical variables like diet group and fat burning group.
2. Menopausal status dataset
3. Cluster components of the metabolites seperated by week 

**Contains
1. Table 1 LME- LME code for table 1 to get p-values for differences in relevant variables by diet group
2. Menopause analysis- Code that includes the set up for the analysis (merging the datasets mentioned above) and going through the analysis to check for the impact of menopausal status (measured in 2 different ways, via self report or confirmed at doctor visit).
3. sPLS_DA_mixomics- code for the multlevel sPLS-DA with cluster components with a multilevel component
4. sPLS_DA_model tuning- tuning for the sPLS-DA model according to specification from the mixomics package info. Didn't end up using this but wanted to keep since I think it may be helpful code in the future
5. PLSR_Function- Code that goes over how to run a PLSR. Like the tuning code, we didn't end up using this code but it may be helpful in the future
