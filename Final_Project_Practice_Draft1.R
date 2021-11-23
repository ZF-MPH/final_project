
*__!!!!EVERYTHING BELOW THIS IS OLD DRAFT OR PRACTICE CHUNKS!!!!__*

##Practice Chunk GGPLOT from ESME
Zach added line of best fit and eliminated not ascertained responses __probably ready to be moved into finalized section__
```{r}
teen_w_bmi_1 %>% 
  ggplot(aes(bmi, total_pa_wk_min)) +
  geom_point(aes(color = TSEX)) +
  geom_smooth(method = lm) +
  facet_wrap(~TETHRAC_RC.y) +
  theme_minimal() +
  labs(y = "Total Physical Activity per Week",
       x = "Body Mass Index",
       title = "Physical Activity and Body Mass Index",
       subtitle = "by Race/Ethnicity")
```

##Practice Chunk GGPLOT ZACH
this one shows counts of students age by grade
```{r}
teen_w_bmi_1 %>% 
  ggplot(aes(x = TGRADE, fill = TAGE.y)) +
  geom_bar(position = "stack") +
  scale_y_continuous(breaks = seq(0, 300, by = 25)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

##Practice Chunk GGPLOT ZACH NUMBER 2
this one shows counts of students age by grade faceted by race/ethnicity
```{r}
teen_w_bmi_1 %>% 
  ggplot(aes(x = TGRADE, fill = TAGE.y)) +
  geom_bar(position = "stack") +
  scale_y_continuous(breaks = seq(0, 300, by = 25)) +
  facet_wrap(~TETHRAC_RC.y) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

## Practice chunk GGPLOT ESME
```{r another-option-for-viz}
teen_w_bmi_1 %>% 
  ggplot(aes(total_pa_wk_min, TSCHLTYPE)) +
  geom_density_ridges(aes(fill = factor(TSEX)),
                      alpha = 0.7) +
  scale_fill_viridis_d(option = "magma", name = "Sex") +
  theme_ridges() +
  labs(title = "Total Physical Activity by School Type",
       x = "School Type",
       y = "Total Weekly Physical Activity (min)",
       tag = "Figure 2")

### labs() not centered...
```


## Join Data
Datasets were **joined** (demographics dataset and physical activity dataset)
```{r}
library(tidyverse)
library(rio)
library(here)
library(janitor)
demographics <- import(here("data", "Teen-Demographic-SPSS(1) copy.zip"))

phys_act <- import(here("data", "Teen-PA-SPSS(1) copy.zip"))

# joined demographics and PA-questionnaire by participant ID "PID"
joined_set <- demographics %>% full_join(phys_act, by = "PID")

#try left_join and decide which to use, based on what data/variables we want

joined_set_revise <- demographics %>% left_join(phys_act, by = "PID")
#suggestion from Prof Nese
joined_set_revise %>% 
  as_tibble() %>%
  select(TSEX, TSCHLTYPE, TETHRAC_RC) %>% 
  characterize(c(TSEX, TSCHLTYPE, TETHRAC_RC))
#select so we only see what we're characterizing 

# selecting only variables that we may use
selected_variables <- joined_set %>% 
  select(TAGE, TGRADE, TSEX, TSCHLTYPE, XTHEIGHTCM_RC, XTWEIGHTKG_RC, TETHRAC_RC, TPMPABAD, TPMPAWANT, TPMPAUPST, TPMPAIMPT, XTPREDWEEKS, XTPREDWEEKOFS, XTPREDWEEKW)

selected_variables %>% as_tibble() %>%
  characterize(c("TSEX", "TSCHLTYPE", "TETHRAC_RC"))  

#characterize race, sex, and school type 
```

## Filter and Mutate
We **filtered** to only show _high school respondents_ & then used **mutate()**
  to create a column for BMI (calculated from height/weight
                              ```{r}
                              highschool_only <- selected_variables %>% 
                                filter(TAGE >= 14 | TAGE < 18) %>% 
                                filter(!is.na(TAGE))
                              
                              HS_W_BMI <- highschool_only %>% 
                                mutate(Height_M = XTHEIGHTCM_RC / 100) %>% 
                                mutate(BMI = XTWEIGHTKG_RC/(Height_M^2)) %>%
                                mutate(total_PA = XTPREDWEEKS + XTPREDWEEKOFS + XTPREDWEEKW)
                              ```
                              
                              ## Commit 3
                              ```{r}
                              HS_W_BMI %>%
                                group_by(TSCHLTYPE, TSEX) %>%
                                summarize(mean_age = mean(TAGE), mean_total_PA = mean(total_PA, na.rm= TRUE))
                              
                              
                              
                              
                              longer_teen_bmi_1%>%
                                ggplot(aes(minutes, TSCHLTYPE)) +
                                geom_point(aes(color= MVPA_when))
                              
                              
                              labs(title = "Physical Activty By Location and by School Type",
                                   x = "When Participants Complete Physical Activity",
                                   y = "Total Weekly Physical Activity (min)",
                                   tag = "Figure 4")
                              
                              #compute mean and sd then pivot wider # 
                              
                              
                              
                              teen_w_bmi_1 %>%
                                summarize(mean_bmi= mean(bmi), TGRADE)%>%
                                pivot_wider(
                                  names_from=TGRADE, 
                                  values_from=mean_bmi
                                )
                              #i want to show all the duplicates
                              
                              teen_w_bmi_1 %>%
                                select(TGRADE,total_pa_wk_min) %>%
                                pivot_wider(
                                  names_from = TGRADE, 
                                  values_from= total_pa_wk_min
                                )
                              
                              #same as above but to get the mean PA per grade
                              summarize(mean_total_pa= mean(total_pa_wk_min), TGRADE) %>%
                                pivot_wider(
                                  names_from =TGRADE,
                                  values_from =mean_total_pa
                                )
                              #how to show every value for each grade 
                              
                              teen_w_bmi_1 %>% 
                                summarize(mean_bmi = mean(bmi), TGRADE) %>%
                                pivot_wider(
                                  names_from = TGRADE, 
                                  values_from = mean_bmi)
                              
                              
                              
                              
                              
                              
                              
                              summarize(mean_bmi= mean(bmi)) %>%
                                pivot_wider(
                                  names_from = TGRADE, 
                                  values_from = mean_bmi
                                )
                              
                              
                              
                              
                              wider_hs <- highschool_only %>%
                                pivot_wider(
                                  names_from = c(TGRADE,TSEX), 
                                  values_from = XTPREDWEEKS,
                                  values_fn = list(XTPREDWEEKS = mean))
                              #doesn't make sense so will change, but it does make it wider! 
                              
                              
                              ```
                              
                              ZACH TEST (Turns all "-9" into "NA") (This worked)
                              ```{r}
                              library(naniar)
                              NAs_HS_W_BMI <- HS_W_BMI %>% 
                                replace_with_na_all(condition = ~.x == -9)
                              ```
                              
                              ZACH TEST to get rid of all rows containing NA (this worked)
                              ```{r}
                              omit_HS_W_BMI <- na.omit(NAs_HS_W_BMI)
                              
                              # TRIED to use characterize here, and it did nothing
                              ```
                              ## Esme Visual Plot- In Progress..
                              ```{r}
                              omit_HS_W_BMI %>% 
                                ggplot(aes(BMI, total_PA)) +
                                geom_point(aes(color = TSEX)) +
                                facet_wrap(~TETHRAC_RC) +
                                theme_minimal() +
                                labs(y = "Total Physical Activity per Week",
                                     x = "Body Mass Index",
                                     title = "Physical Activity and Body Mass Index")
                              
                              # may look for something other than BMI
                              ```
                              
                              ## ZACH trying to fix characterize issues
                              ```{r}
                              join1 <- joined_set %>% 
                                as_tibble() %>%
                                select(PID, TSEX, TSCHLTYPE, TETHRAC_RC, TGRADE) %>% 
                                characterize(c(TSEX, TSCHLTYPE, TETHRAC_RC, TGRADE))
                              
                              join2 <- joined_set %>% 
                                select(PID,XTHEIGHTCM_RC, XTWEIGHTKG_RC, TETHRAC_RC, TPMPABAD, TPMPAWANT, TPMPAUPST, TPMPAIMPT, XTPREDWEEKS, XTPREDWEEKOFS, XTPREDWEEKW)
                              
                              try_joined_set <- join2 %>% full_join(join1, by = "PID")
                              ```
                              **try_joined_set** worked!!!!!
                                