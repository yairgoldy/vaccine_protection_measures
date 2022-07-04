


## Data and code for the example analysis in the paper "Measuring vaccine protection when the population is mostly not immune-naive" 

### Data

The example discussed in the Supplementary Appendix is based on publicly-available data regarding the booster dose protection that accompanied the paper ["Protection of BNT162b2 Vaccine Booster against Covid-19 in Israel"](https://www.nejm.org/doi/full/10.1056/nejmoa2114255). 

The data are given as a table which includes the number of risk days and the number of severe illness cases during the period August 10 to 26, 2021, for individuals who are Israeli residents, ages 60 and above, who received at least two BNT162b2 doses. The data are grouped according to gender, sector, age group, vaccination period, and date.

### Reading the Data
To read the data into the R Statistical Software, first load the libraries `tidyverse`, `pander` and `broom` and then use the following code. Note that cells that have fewer than 5 individuals are omitted from the analysis.
```
dat_severe <- read_csv("SevereDataAug10-26.csv",
                       show_col_types = FALSE) %>%  
  filter(N_person	!= "<5") %>%
  mutate(`Vacc Period` = factor(`Vacc Period`,levels = c("JanB","FebA","FebB")), 
         Cohort = case_when(
           Cohort=="12+" ~ "Booster",
           `Vacc Period`=="JanB" ~ "2-dose 6 months",
           `Vacc Period`=="FebA" ~ "2-dose 5.5 months",
           `Vacc Period`=="FebB" ~ "2-dose 5 months"),
         Cohort = factor(Cohort,levels = 
            c("Booster", "2-dose 6 months", 
              "2-dose 5.5 months", "2-dose 5 months")),
         `Epi Day` = factor(`Epi Day`),
         Sector = factor(Sector, levels = c("General Jewish", "Arab","ultra-Orthodox Jews")),
         N_person = as.numeric(N_person),
         Severe =  Rate_per_1K*N_person/1000)



```

### Analysis

The analyses compare four different cohorts of individuals that were eligible for the booster, all 60 years and above:

1. doubly-vaccinated 6 months (vaccinated by the second part of January), denoted "2-dose 6 months",
1. doubly-vaccinated 5.5 months (vaccinated at the first part of February), denoted "2-dose 5.5 months,
1. doubly-vaccinated 5 months (vaccinated at the second part of February), denoted "2-dose 5 months,
1. booster dose, denoted "Booster"

The analysis for severe illness uses Poisson regression and adjusts for age group, gender, epidemiological day (Epi Day), and sector.

Note that this analysis is different from that used in the original paper as it compares between the different cohorts and not between the joint cohort of the 2nd dose and booster. 

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
