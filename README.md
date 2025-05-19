## Adaptation of QALY Shortfall Calculator

This repository is an adaptation of the [QALY Shortfall Calculator online tool](https://shiny.york.ac.uk/shortfall), forked from [bitowaqr/shortfall](https://github.com/bitowaqr/shortfall). This adaptation includes extra options for testing alignment with Lumanity models:

* Discount rates applied at midpoint rather than start of year
* One extra decimal point for entering starting age and % female

A previous development version of this adaptation tested options for turning off the half cycle correction and using a weekly rather than annual cycle length.

The reference list has also been updated to clarify that the [2017-2019 England and Wales life table data](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandandwalesreferencetables/current) in the app are from the version released on 23 September 2021 (not the latest release).

## Original QALY Shortfall Calculator repository notes

This repository contains the R source code for the publication:

### McNamara S, Schneider PP, Love-Koh J, Doran T, Gutacker N. **Quality-Adjusted Life Expectancy Norms for the English Population**. Value in Health. 2022 Aug 12. [https://doi.org/10.1016/j.jval.2022.07.005](https://doi.org/10.1016/j.jval.2022.07.005)

**Link to the: [QALY Shortfall calculator web tool](https://shiny.york.ac.uk/shortfall)**

*****

#### Folder structure:

```
./app/               # source code for the shiny app
./src manuscript/    # analysis for the results reported in the manuscript
```

*****

#### Abstract

##### **Objective**

The National Institute for Health and Care Excellence in England has proposed severity-of-disease modifiers that give greater weight to health benefits accruing to patients who experience a larger shortfall in quality-adjusted life years (QALYs) under current standard of care compared to healthy individuals. This requires an estimate of quality-adjusted life expectancy (QALE) of the general population by age and sex. Previous QALE population norms are based on nearly 30-year old assessments of HRQoL in the general population. This study provides updated QALE estimates for the English population by age and sex.

##### **Methods**
EQ-5D-5L data for 14,412 participants from the Health Survey for England (waves 2017 and 2018) were pooled and HRQoL population norms were calculated. These norms were combined with official life tables from the Office for National Statistics for 2017-2019 using the Sullivan method to derive QALE estimates by age and sex. Values were discounted using 0%, 1.5% and 3.5% discount rates.

##### **Results**
QALE at birth is 68.04 QALYs for men and 68.48 QALYs for women. These values are lower than previously published QALE population norms based on older HRQoL data. 

##### **Conclusions**
This study provides new QALE population norms for England that serve to establish absolute and relative QALY shortfalls for the purpose of health technology assessments.


****

##### **Data availability** 

All the data sets that were used for the analysis are publicly available:

1. Health Survey for England:
  * University College London Department of Epidemiology and Public Health; National Centre for Social Research (NatCen). Health Survey for England, 2017. UK Data Service (2021). [link](http://doi.org/10.5255/UKDA-SN-8488-2)
  * University College London Department of Epidemiology and Public Health; National Centre for Social Research (NatCen). Health Survey for England, 2018. UK Data Service (2021). [link](http://doi.org/10.5255/UKDA-SN-8649-1)

2. Interim Scoring for the EQ-5D-5L:
  * Van Hout B, Janssen MF, Feng YS, Kohlmann T, Busschbach J, Golicki D, Lloyd A, Scalone L, Kind P, Pickard AS. Interim scoring for the EQ-5D-5L: mapping the EQ-5D-5L to EQ-5D-3L value sets. Value in health. 2012 Jul 1;15(5):708-15. [link](https://doi.org/10.1016/j.jval.2012.02.008)
  * Fraser Morton and Jagtar Singh Nijjar (2020). eq5d: Methods for Calculating 'EQ-5D' Utility Index Scores. R package version 0.7.0. [link](https://cran.r-project.org/package=eq5d)
  * Hernandez Alava, M., Pudney, S., and Wailoo, A. (2020) Estimating the relationship between EQ-5D-5L and EQ-5D-3L: results from an English Population Study. Policy Research Unit in Economic Evaluation of Health and Care Interventions. Universities of Sheffield and York. Report 063, [link](http://nicedsu.org.uk/mapping-eq-5d-5l-to-3l/)

3. National Life Tables:
  * ONS: National Life Tables, England, 1980-1982 to 2017-2019. (2021). [link](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandreferencetables)

****

If you have comments, questions, or concerns, please [contact Us](mailto:p.schneider@sheffield.ac.uk)

