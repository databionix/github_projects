# Analysis of Hospital Mortality with Clinical Data


## Description

This project analyzes hospital patient data to explore clinical patterns
associated with mortality using R. The data were obtained from
PhysioNet’s public MIMIC-IV Clinical Database Demo \[1\].

This pipeline includes the following stages:

\- **Preprocessing** data: cleaning, creating derived variables ,and
including them in the patients’ file (patients_mod1).

\- **Exploratory data analysis (EDA)**: Visualization with ggplot to
study the relationship among variables (categorical/continuous) and
mortality.

\- **Non-supervised analysis**: Performing clustering analysis (k-means
and hierarchical with Gower distance) to explore clinical patterns
related with mortality.

## Preprocessing

Load packages

``` r
library(tidyverse)
library(knitr)
```

Import R files

``` r
admissions <- read_csv("data/admissions.csv") |> mutate(subject_id = as.character(subject_id))
patients <- read_csv("data/patients.csv") |>  mutate(subject_id = as.character(subject_id))
```

Calculate the number of admissions

``` r
unique_admissions <- count(admissions, subject_id)
patients_mod1 <- full_join(patients, unique_admissions, by = "subject_id") |> rename(number_admissions = n) 
```

``` r
kable(head(patients_mod1, n = 10))
```

| subject_id | gender | anchor_age | anchor_year | anchor_year_group | dod | number_admissions |
|:---|:---|---:|---:|:---|:---|---:|
| 10014729 | F | 21 | 2125 | 2011 - 2013 | NA | 2 |
| 10003400 | F | 72 | 2134 | 2011 - 2013 | 2137-09-02 | 7 |
| 10002428 | F | 80 | 2155 | 2011 - 2013 | NA | 7 |
| 10032725 | F | 38 | 2143 | 2011 - 2013 | 2143-03-30 | 2 |
| 10027445 | F | 48 | 2142 | 2011 - 2013 | 2146-02-09 | 3 |
| 10037928 | F | 78 | 2175 | 2011 - 2013 | NA | 10 |
| 10001725 | F | 46 | 2110 | 2011 - 2013 | NA | 1 |
| 10040025 | F | 64 | 2143 | 2011 - 2013 | 2148-02-07 | 10 |
| 10008454 | F | 26 | 2110 | 2011 - 2013 | NA | 1 |
| 10020640 | F | 91 | 2153 | 2011 - 2013 | 2154-02-04 | 1 |

Promedio del tiempo de permanencia y tiempo total de permanencia por
paciente (cálculo en horas)

``` r
admissions_mod <- mutate(admissions, length_of_stay = dischtime -  admittime, .after= dischtime) 
```

``` r
stay <- group_by(admissions_mod, subject_id) |> 
  summarise(total_stay_h = as.numeric(sum(length_of_stay)), avg_stay_h= as.numeric(mean(length_of_stay))) 
```

``` r
patients_mod1 <- full_join(patients_mod1, stay, by= "subject_id")
```

Convert mortality to 0 and 1

``` r
patients_mod1 <- mutate(patients_mod1, patients_death = ifelse(is.na(dod), 0, 1), .after = dod)
```

Obtain the most frequent admission type. Group by ID and count the
admission type.

``` r
most_freq_admission <- count(admissions,subject_id, admission_type)
kable(head(most_freq_admission))
```

| subject_id | admission_type |   n |
|:-----------|:---------------|----:|
| 10000032   | EW EMER.       |   3 |
| 10000032   | URGENT         |   1 |
| 10001217   | DIRECT EMER.   |   1 |
| 10001217   | EW EMER.       |   1 |
| 10001725   | EW EMER.       |   1 |
| 10002428   | EU OBSERVATION |   3 |

Reorganize the table

``` r
most_freq_admission <- arrange(most_freq_admission, subject_id, desc(n))
```

Group by subject_id and extract the first row in each group.

``` r
adm <- group_by(most_freq_admission, subject_id) |> slice_head(n = 1) 
adm <- rename(adm,admissions_n = n)
kable(head(adm))
```

| subject_id | admission_type | admissions_n |
|:-----------|:---------------|-------------:|
| 10000032   | EW EMER.       |            3 |
| 10001217   | DIRECT EMER.   |            1 |
| 10001725   | EW EMER.       |            1 |
| 10002428   | EU OBSERVATION |            3 |
| 10002495   | URGENT         |            1 |
| 10002930   | EU OBSERVATION |            7 |

Modify patients_mod1 including admission and n as well.

``` r
patients_mod1 <- full_join(patients_mod1, adm, by = "subject_id")
```

Calculate the most frequent insurance per patient.

``` r
subject_insurance_count <- count(admissions,subject_id, insurance) |> 
  rename(insurance_count = n)
```

If there is a tie, I keep only one subject_id -\> with_ties = FALSE

``` r
mfi_per_patient <- group_by(subject_insurance_count, subject_id) |>
  slice_max(order_by = insurance_count, n = 1, with_ties = FALSE)
```

``` r
patients_mod1 <- full_join(patients_mod1,mfi_per_patient, by="subject_id")
```

Which patients were readmitted within the first 30 days after discharge?

``` r
adm_relag <- arrange(admissions, subject_id, admittime) |> 
  group_by(subject_id) |> 
  mutate(discharge_time = lag(dischtime), .after = dischtime)
```

``` r
adm_mod3 <- mutate(adm_relag, days_between = as.numeric(admittime - discharge_time), .after = discharge_time) |> 
  filter(days_between <= 30)
kable(head(adm_mod3)) 
```

| subject_id | hadm_id | admittime | dischtime | discharge_time | days_between | deathtime | admission_type | admit_provider_id | admission_location | discharge_location | insurance | language | marital_status | race | edregtime | edouttime | hospital_expire_flag |
|:---|---:|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|
| 10000032 | 29079034 | 2180-07-23 12:35:00 | 2180-07-25 17:55:00 | 2180-06-27 18:49:00 | 25.740278 | NA | EW EMER. | P30KEH | EMERGENCY ROOM | HOME | Medicaid | ENGLISH | WIDOWED | WHITE | 2180-07-23 05:54:00 | 2180-07-23 14:00:00 | 0 |
| 10000032 | 25742920 | 2180-08-05 23:44:00 | 2180-08-07 17:50:00 | 2180-07-25 17:55:00 | 11.242361 | NA | EW EMER. | P60CC5 | EMERGENCY ROOM | HOSPICE | Medicaid | ENGLISH | WIDOWED | WHITE | 2180-08-05 20:58:00 | 2180-08-06 01:44:00 | 0 |
| 10001217 | 27703517 | 2157-12-18 16:58:00 | 2157-12-24 14:55:00 | 2157-11-25 18:00:00 | 22.956944 | NA | DIRECT EMER. | P99698 | PHYSICIAN REFERRAL | HOME HEALTH CARE | Other | ? | MARRIED | WHITE | NA | NA | 0 |
| 10002428 | 20321825 | 2156-04-30 20:35:00 | 2156-05-03 16:36:00 | 2156-04-29 16:26:00 | 1.172917 | NA | EW EMER. | P825SO | EMERGENCY ROOM | CHRONIC/LONG TERM ACUTE CARE | Medicare | ENGLISH | WIDOWED | WHITE | 2156-04-30 18:30:00 | 2156-04-30 21:53:00 | 0 |
| 10002428 | 23473524 | 2156-05-11 14:49:00 | 2156-05-22 14:16:00 | 2156-05-03 16:36:00 | 7.925694 | NA | EW EMER. | P3529J | EMERGENCY ROOM | CHRONIC/LONG TERM ACUTE CARE | Medicare | ENGLISH | WIDOWED | WHITE | 2156-05-11 11:29:00 | 2156-05-11 16:53:00 | 0 |
| 10002930 | 28301173 | 2197-04-08 19:37:00 | 2197-04-15 12:01:00 | 2197-04-08 19:37:00 | 0.000000 | NA | URGENT | P3763P | INTERNAL TRANSFER TO OR FROM PSYCH | HOME | Medicare | ENGLISH | SINGLE | BLACK/AFRICAN AMERICAN | NA | NA | 0 |

How many times were the readmitted ?

``` r
readm_number = count(adm_mod3, subject_id) |> 
  rename(n_readm_30_days = n)

patients_mod1 <- full_join(patients_mod1,readm_number, by = "subject_id")

patients_mod1 <- mutate(patients_mod1, n_readm_30_days = ifelse(is.na(n_readm_30_days), 0,n_readm_30_days))

patients_mod1 <- mutate(patients_mod1, readm_30_days = ifelse(n_readm_30_days == 0, 0, 1))
```

I introduce the variable ethnicity into patients_mod1

``` r
ethnicity_count <-count(admissions, subject_id, race)
kable(head(ethnicity_count))
```

| subject_id | race                   |   n |
|:-----------|:-----------------------|----:|
| 10000032   | WHITE                  |   4 |
| 10001217   | WHITE                  |   2 |
| 10001725   | WHITE                  |   1 |
| 10002428   | WHITE                  |   7 |
| 10002495   | UNKNOWN                |   1 |
| 10002930   | BLACK/AFRICAN AMERICAN |  12 |

The resulting table has a dimension of 104 x 3, this is because for the
same patient there are different classifications. Example:

``` r
kable(head(filter(ethnicity_count, subject_id ==10004457)))
```

| subject_id | race  |   n |
|:-----------|:------|----:|
| 10004457   | OTHER |   1 |
| 10004457   | WHITE |   5 |

I create a new logical column to identify which values are duplicated.

``` r
ethnicity_count <-mutate(ethnicity_count, duplicated_var = duplicated(ethnicity_count$subject_id))
kable(head(ethnicity_count))
```

| subject_id | race                   |   n | duplicated_var |
|:-----------|:-----------------------|----:|:---------------|
| 10000032   | WHITE                  |   4 | FALSE          |
| 10001217   | WHITE                  |   2 | FALSE          |
| 10001725   | WHITE                  |   1 | FALSE          |
| 10002428   | WHITE                  |   7 | FALSE          |
| 10002495   | UNKNOWN                |   1 | FALSE          |
| 10002930   | BLACK/AFRICAN AMERICAN |  12 | FALSE          |

I filter those that are TRUE.

``` r
ethnicity_repeated <- filter(ethnicity_count, duplicated_var == TRUE)
kable(head(ethnicity_repeated))
```

| subject_id | race                    |   n | duplicated_var |
|:-----------|:------------------------|----:|:---------------|
| 10004457   | WHITE                   |   5 | TRUE           |
| 10035631   | WHITE                   |   6 | TRUE           |
| 10037928   | HISPANIC/LATINO - CUBAN |   9 | TRUE           |
| 10038992   | WHITE                   |   1 | TRUE           |

Now I filter out the FALSE values and keep the table of 100 rows, which
I will join to patients_mod1.

``` r
ethnicity_count <- filter(ethnicity_count, duplicated_var != TRUE)
kable(head(ethnicity_count))
```

| subject_id | race                   |   n | duplicated_var |
|:-----------|:-----------------------|----:|:---------------|
| 10000032   | WHITE                  |   4 | FALSE          |
| 10001217   | WHITE                  |   2 | FALSE          |
| 10001725   | WHITE                  |   1 | FALSE          |
| 10002428   | WHITE                  |   7 | FALSE          |
| 10002495   | UNKNOWN                |   1 | FALSE          |
| 10002930   | BLACK/AFRICAN AMERICAN |  12 | FALSE          |

Before joining the tables, I modify the data of the subject_id present
in ethnicity_count with those obtained from ethnicity_repeated.

``` r
ethnicity_count <- mutate(ethnicity_count, race = case_when(subject_id == 10004457 ~ "WHITE",
  subject_id == 10035631 ~ "WHITE",
  subject_id == 10037928 ~ "HISPANIC/LATINO - CUBAN",
  subject_id == 10038992 ~ "WHITE", 
  .default = race))
kable(head(ethnicity_count))
```

| subject_id | race                   |   n | duplicated_var |
|:-----------|:-----------------------|----:|:---------------|
| 10000032   | WHITE                  |   4 | FALSE          |
| 10001217   | WHITE                  |   2 | FALSE          |
| 10001725   | WHITE                  |   1 | FALSE          |
| 10002428   | WHITE                  |   7 | FALSE          |
| 10002495   | UNKNOWN                |   1 | FALSE          |
| 10002930   | BLACK/AFRICAN AMERICAN |  12 | FALSE          |

I check that it has been replaced correctly.

``` r
kable(filter(ethnicity_count, subject_id == 10004457 | subject_id == 10035631 | subject_id == 10037928 | subject_id == 10038992))
```

| subject_id | race                    |   n | duplicated_var |
|:-----------|:------------------------|----:|:---------------|
| 10004457   | WHITE                   |   1 | FALSE          |
| 10035631   | WHITE                   |   3 | FALSE          |
| 10037928   | HISPANIC/LATINO - CUBAN |   1 | FALSE          |
| 10038992   | WHITE                   |   1 | FALSE          |

n Join with the patients_mod1 table.

``` r
patients_mod1 <- select(ethnicity_count, subject_id, race) |> 
  full_join(patients_mod1, ethnicity_count, by = "subject_id")
kable(head(patients_mod1))
```

| subject_id | race | gender | anchor_age | anchor_year | anchor_year_group | dod | patients_death | number_admissions | total_stay_h | avg_stay_h | admission_type | admissions_n | insurance | insurance_count | n_readm_30_days | readm_30_days |
|:---|:---|:---|---:|---:|:---|:---|---:|---:|---:|---:|:---|---:|:---|---:|---:|---:|
| 10000032 | WHITE | F | 52 | 2180 | 2014 - 2016 | 2180-09-09 | 1 | 4 | 138.66667 | 34.66667 | EW EMER. | 3 | Medicaid | 3 | 2 | 1 |
| 10001217 | WHITE | F | 55 | 2157 | 2011 - 2013 | NA | 0 | 2 | 305.01667 | 152.50833 | DIRECT EMER. | 1 | Other | 2 | 1 | 1 |
| 10001725 | WHITE | F | 46 | 2110 | 2011 - 2013 | NA | 0 | 1 | 71.86667 | 71.86667 | EW EMER. | 1 | Other | 1 | 0 | 0 |
| 10002428 | WHITE | F | 80 | 2155 | 2011 - 2013 | NA | 0 | 7 | 944.36667 | 134.90952 | EU OBSERVATION | 3 | Medicare | 7 | 2 | 1 |
| 10002495 | UNKNOWN | M | 81 | 2141 | 2014 - 2016 | NA | 0 | 1 | 165.40000 | 165.40000 | URGENT | 1 | Medicare | 1 | 0 | 0 |
| 10002930 | BLACK/AFRICAN AMERICAN | F | 48 | 2193 | 2011 - 2013 | 2201-12-24 | 1 | 12 | 958.43333 | 79.86944 | EU OBSERVATION | 7 | Medicare | 12 | 1 | 1 |

## Exploratory Data Analysis (EDA)

### Categorical Variables vs. Death Outcome.

**Proportion of deaths by insurance.**

``` r
ggplot(patients_mod1, mapping = aes(y = factor(patients_death), fill = insurance)) +
  geom_bar() +
  scale_fill_manual(values = c("Medicaid" = "steelblue",
 "Medicare" = "darkorange", 
 "Other" = "#76B7B2"),
name = "0 Alive 
1 Death") +
  theme_light() +
  labs(title = "Counts of deaths by insurance") +
  scale_x_continuous(breaks =seq(0,70, by=5))+
  coord_flip()
```

![](README_files/figure-commonmark/unnamed-chunk-27-1.png)

**Proportion of deaths by gender.**

``` r
ggplot(patients_mod1, aes(y=factor(patients_death), fill = gender)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("F" = "lightpink", "M"="lightblue")) +
  labs(title = "Count of deaths by gender",
y = "") +
  scale_x_continuous(breaks = seq(0,50, by= 5)) +
  theme_light() +
  coord_flip() 
```

![](README_files/figure-commonmark/unnamed-chunk-28-1.png)

Different names are used to refer to the same variable values. For
example: “UNKNOWN, UNABLE TO OBTAIN, OTHER…” I recoded the variable race
to group similar categories under a single label.

``` r
patients_mod1 <- mutate(patients_mod1, race = case_when(race %in% c("HISPANIC OR LATINO", "HISPANIC/LATINO - CUBAN",  "HISPANIC/LATINO - PUERTO RICAN", "HISPANIC/LATINO - SALVADORAN") ~ "LATINO",  
race %in% c("BLACK/AFRICAN AMERICAN","BLACK/CAPE VERDEAN", "BLACK") ~ "BLACK",
race %in% c("WHITE - BRAZILIAN", "WHITE - OTHER EUROPEAN", "WHITE", "PORTUGUESE")~ "WHITE",
race %in% c("UNKNOWN","UNABLE TO OBTAIN","OTHER", "PATIENT DECLINED TO ANSWER") ~ "OTHER", .default= race))
```

**Proportion of deaths by race.**

``` r
ggplot(patients_mod1,aes(y= factor(patients_death), fill = race)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values=c("BLACK" = "turquoise", "OTHER" = "lightgray", "WHITE" = "skyblue", "LATINO"="salmon"),
name = "0 Alive 1 Death
Race") +
  theme_light() +
  scale_linetype_binned() +
  scale_x_continuous(breaks = seq(0, 70, by=5)) +
  coord_flip() +
  labs(title = "Count of deaths according to race",
y = "")
```

![](README_files/figure-commonmark/unnamed-chunk-30-1.png)

**Proportion of deaths by admission type.**

``` r
ggplot(patients_mod1, aes(y=factor(patients_death), fill =admission_type)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  theme_classic() +
  theme(legend.text= element_text(size = 5),
legend.title= element_text(size = 8)) +
  scale_x_continuous(breaks = seq(0,30, by=2)) +
  labs(title = "Death vs adm.type") +
   scale_fill_brewer(palette="Set3") 
```

![](README_files/figure-commonmark/unnamed-chunk-31-1.png)

### Quantitative Variables vs Death Outcome.

**Average length of stay (hours) by death outcome**

``` r
deaths_vs_stay <- group_by(patients_mod1, patients_death) |> 
  summarise(media = mean(avg_stay_h),desv = sd(avg_stay_h, na.rm = TRUE), 
n = n())
```

``` r
deaths_vs_stay <- mutate(deaths_vs_stay, est_error = desv/sqrt(n),
upper_bound_ci = media + qt(0.975, df = n - 1) * est_error, 
lower_bound_ci = media - qt(0.975, df = n - 1) * est_error)
```

``` r
ggplot(deaths_vs_stay, aes(x = factor(patients_death), y=media)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin= media - desv, ymax= media + desv), width = 0.2) +
  xlab("Death outcome") +
  ylab("Mean") +
  theme_minimal() 
```

![](README_files/figure-commonmark/unnamed-chunk-34-1.png)

**Number of readmissions within the first 30 days by death outcome**

``` r
ggplot(patients_mod1, aes(y = factor(patients_death), fill = factor(n_readm_30_days))) +
  geom_bar(position = "dodge") +
  labs(y="", fill = "Number of readmissions") +
  theme_minimal() +
  ggtitle("Number of readmissions within first 30 days") +
  scale_x_continuous(breaks = seq(0,100,by= 5)) +
  theme(legend.text= element_text(size = 9), legend.position="bottom", 
legend.title.position = "top") +
  guides(fill=guide_legend(nrow = 1)) +
  scale_fill_manual(values = c("red", "blue", "green", "grey", "pink", "purple"))
```

![](README_files/figure-commonmark/unnamed-chunk-35-1.png)

**Average stay (hours) and number of admissions by death outcome**

``` r
ggplot(patients_mod1, aes(x= avg_stay_h, y =number_admissions, color=factor(patients_death))) +
  geom_point() +
  theme_light() +
  ylab("Number of admissions") +
  xlab("Average of stay (hours)") +
  ggtitle("Average Hospital Stay vs. Number of Admissions, by Death Outcome") +
  scale_x_continuous(breaks = seq(0,800, by= 100)) +
  scale_y_continuous(breaks= seq(0,25, by= 2)) +
  scale_color_manual(name = "Status", values = c("0" = "grey", "1"="turquoise"), labels = c("0" = "0 Alive", "1" = "1 Death"))
```

![](README_files/figure-commonmark/unnamed-chunk-36-1.png)

**Readmissions within 30 days vs number of admissions by death outcome**

Selection: 1

``` r
ggplot(patients_mod1, aes(y=n_readm_30_days, x =number_admissions, color = factor(patients_death))) +
  geom_point()+
  ylab("Readmissions within 30 d.") +
  xlab("Number of admissions") +
  theme_minimal() +
  scale_color_manual(values=c("0" = "salmon", "1" = "lightgrey")) +
  scale_y_continuous(breaks = seq(-30, 30, by= 5)) +
  ggtitle("Relationship Between Admissions 
and 30-Day Readmissions (Alive vs Dead)")
```

![](README_files/figure-commonmark/unnamed-chunk-37-1.png)

**Hospital stay vs 30-day readmissions by survival**

``` r
ggplot(patients_mod1, aes(y = total_stay_h, x = n_readm_30_days ,color = factor(patients_death))) +
  geom_point() +
  labs(title = "Hospital Stay vs. 30-Day Readmissions 
by Survival Status", x = "Readmissions within 30 days", y = "Total stay in hours", color = "Patients death") +
  theme_minimal() 
```

![](README_files/figure-commonmark/unnamed-chunk-38-1.png)

**Number of admissions by admission type and death outcome**

``` r
ggplot(patients_mod1, aes(x = admissions_n, fill = admission_type)) +
  geom_bar()+
  facet_wrap(~patients_death) +
  scale_fill_brewer(palette= "Set3") +
  theme_minimal() +
  labs(x = "Number of admissions", title = "Admissions by Type and Outcome", fill = "") +
   theme(legend.text= element_text(size = 5))
```

![](README_files/figure-commonmark/unnamed-chunk-39-1.png)

## **Non-supervised analysis.**

### Clustering quantitative variables.

Patient clustering: selection of relevant variables. Eliminate
categorical (character) variables.

``` r
patients_mod2 <- select(patients_mod1, -c(subject_id,insurance,gender,anchor_year ,anchor_year_group, dod,
patients_death, race, readm_30_days, admission_type))
```

Visualize the data

``` r
summary(patients_mod2)
```

       anchor_age    number_admissions  total_stay_h       avg_stay_h    
     Min.   :21.00   Min.   : 1.00     Min.   :  41.68   Min.   : 34.67  
     1st Qu.:51.75   1st Qu.: 1.00     1st Qu.: 124.33   1st Qu.:102.09  
     Median :63.00   Median : 1.00     Median : 244.72   Median :140.34  
     Mean   :61.75   Mean   : 2.75     Mean   : 453.79   Mean   :172.94  
     3rd Qu.:72.00   3rd Qu.: 3.00     3rd Qu.: 540.56   3rd Qu.:216.75  
     Max.   :91.00   Max.   :20.00     Max.   :2859.72   Max.   :751.38  
      admissions_n  insurance_count n_readm_30_days
     Min.   :1.00   Min.   : 1.0    Min.   :0.00   
     1st Qu.:1.00   1st Qu.: 1.0    1st Qu.:0.00   
     Median :1.00   Median : 1.0    Median :0.00   
     Mean   :1.67   Mean   : 2.7    Mean   :0.49   
     3rd Qu.:2.00   3rd Qu.: 3.0    3rd Qu.:1.00   
     Max.   :7.00   Max.   :20.0    Max.   :8.00   

Standardization

``` r
patients_scale <- scale(patients_mod2)
summary(patients_scale)
```

       anchor_age      number_admissions   total_stay_h       avg_stay_h     
     Min.   :-2.5201   Min.   :-0.55042   Min.   :-0.7630   Min.   :-1.2278  
     1st Qu.:-0.6184   1st Qu.:-0.55042   1st Qu.:-0.6100   1st Qu.:-0.6291  
     Median : 0.0773   Median :-0.55042   Median :-0.3871   Median :-0.2895  
     Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000  
     3rd Qu.: 0.6339   3rd Qu.: 0.07863   3rd Qu.: 0.1607   3rd Qu.: 0.3890  
     Max.   : 1.8089   Max.   : 5.42555   Max.   : 4.4544   Max.   : 5.1362  
      admissions_n     insurance_count    n_readm_30_days  
     Min.   :-0.4913   Min.   :-0.53840   Min.   :-0.4228  
     1st Qu.:-0.4913   1st Qu.:-0.53840   1st Qu.:-0.4228  
     Median :-0.4913   Median :-0.53840   Median :-0.4228  
     Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000  
     3rd Qu.: 0.2420   3rd Qu.: 0.09501   3rd Qu.: 0.4400  
     Max.   : 3.9085   Max.   : 5.47905   Max.   : 6.4796  

Set the optimal number of clusters

``` r
library(factoextra)
```

``` r
fviz_nbclust(patients_scale, FUNcluster = kmeans, method = "wss")
```

![](README_files/figure-commonmark/unnamed-chunk-44-1.png)

Optimal k = 3

``` r
cluster_patients <- kmeans(patients_scale, centers = 3)
vector_cluster <- cluster_patients$cluster
patients_mod1 <- mutate(patients_mod1, clusters_patients = vector_cluster)
```

Patient clusters by number of admissions and average hospital stay

``` r
ggplot(patients_mod1, aes(y=avg_stay_h, x =number_admissions, color = factor(clusters_patients))) +
  geom_point() +
  xlab("Number of admissions") +
  ylab("Average of stay (hours) ") +
  ggtitle("Patient clustering by admissions 
and 30-day readmissions") +
  theme_minimal() +
  scale_x_continuous(breaks= seq(0,40, by=5)) +
  scale_y_continuous(breaks = seq(0,1000, by = 50))
```

![](README_files/figure-commonmark/unnamed-chunk-46-1.png)

Visualization of patient clusters

``` r
fviz_cluster(cluster_patients,patients_mod2, axes = c(1,2))
```

![](README_files/figure-commonmark/unnamed-chunk-47-1.png)

### Clustering including categorical variables.

``` r
library(cluster)
```

I cloned patients_mod1 to patients_mod3 in order to make some changes to
the table

``` r
patients_mod1 <- mutate(patients_mod1, prop_insurance_count = insurance_count / number_admissions, .after = insurance_count)


patients_mod3 <- patients_mod1
```

In patients_mod3, convert character columns to factors to avoid errors
when using daisy, and remove irrelevant categorical variables.

``` r
patients_mod3 <- mutate(patients_mod3,subject_id = as.factor(subject_id), 
race = as.factor(race),
anchor_year = as.factor(anchor_year),
anchor_year_group = as.factor(anchor_year_group),
gender = as.factor(gender),
dod = as.factor(dod),
admission_type = as.factor(admission_type),
insurance = as.factor(insurance),
patients_death = as.factor(patients_death),
readm_30_days = as.factor(readm_30_days)) |>  select(-c(subject_id, anchor_year,dod, insurance_count, clusters_patients))
```

Gower distance matrix for patients

``` r
mat <- daisy(patients_mod3, metric = "gower")
```

Hierarchical clustering of patients (complete linkage)

``` r
resultado <- hclust(mat, method = "complete")
```

Dendrogram of patients (complete-linkage clustering)

``` r
plot(resultado, cex = 0.3)
```

![](README_files/figure-commonmark/unnamed-chunk-53-1.png)

I use factoextra to determine the optimal number of clusters (k = 4).

``` r
fviz_nbclust(patients_mod3, FUN = hcut, diss = mat, method = "wss")
```

![](README_files/figure-commonmark/unnamed-chunk-54-1.png)

Dendrogram of patients with 4 clusters highlighted.

``` r
clust <- cutree(resultado, k = 4) 
plot (resultado, cex = 0.3)
rect.hclust(resultado, k = 4, border = 1:3)
```

![](README_files/figure-commonmark/unnamed-chunk-55-1.png)

``` r
patients_mod1 <- mutate(patients_mod1, clusters_chategorical = clust)
```
