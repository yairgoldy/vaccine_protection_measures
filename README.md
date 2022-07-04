---
title: "README"
output:
  html_document:
    df_print: paged
---




## Measuring vaccine protection when the population is mostly not immune-naive  

### Data

The data are taken from the publicly-available data of the paper ["Protection of BNT162b2 Vaccine Booster against Covid-19 in Israel"](https://www.nejm.org/doi/full/10.1056/nejmoa2114255). The data are given as a table of the number of risk days and number of severe illness during the period August 10 to 26, 2021, for individuals who are Israeli residents, ages 60 and above, who receive at least two BNT162b2 doses. The data are grouped according to gender, sector, age group,vaccination period, and date.

### Reading the Data
To read the data into the R Statistical Software, first load the libraries `tidyverse`, `pander` and `broom` and then use the following code. Note that cells that have less than 5 individuals are omitted from the analysis.
```
dat_severe <- read_csv("SevereDataAug10-26.csv",
                       show_col_types = FALSE) %>%  
  filter(N_person	!= "<5") %>%
  mutate(`Vacc Period` = factor(`Vacc Period`,levels = c("JanB","FebA","FebB")), 
         Cohort = if_else(Cohort=="12+","Booster",paste("2nd Dose:",`Vacc Period`)),
         Cohort = factor(Cohort,levels = c("Booster", "2nd Dose: JanB", "2nd Dose: FebA", "2nd Dose: FebB")),
         `Epi Day` = factor(`Epi Day`),
         Sector = factor(Sector, levels = c("General Jewish", "Arab","ultra-Orthodox Jews")),
         N_person = as.numeric(N_person),
         Severe =  Rate_per_1K*N_person/1000)
```

### Analysis

The analyses compare four different cohorts that were eligible for the booster, all 60 years and above:
1. doubly-vaccinated by the second part of January (JanB),
1. doubly-vaccinated by the first part of February (FebA),
1. doubly-vaccinated by the second part of February (FebB),
1. booster dose
The analyses for both confirmed infections and severe illness use Poisson regression and adjust for age group, gender, epidemiological day (`Epi Day`), and sector.

Note that these analyses are different from those used in the original paper as they compare between the different cohorts and not between the joint cohort of the 2nd dose and booster. 

```
formula_severe <- as.formula("Severe ~  Age +
                         Gender + 
                         `Epi Day` +
                         Sector +
                         Cohort +  
                         offset(log(N_person))") 

analysis_severe <- glm(formula_severe, family="poisson", data=dat_severe)

```


### Measures of protection conferred by the vaccine
The following table presents adjusted rates for severe illness as well as different measures of protection that can be calculated using the adjusted rates. 95% confidence intervals appear in parentheses.  

```
table_severe <- calcualte_measures(dat = dat_severe,analysis = analysis_severe)
pander::pander(table_severe,keep.line.breaks = TRUE,justify = 'center')
```
