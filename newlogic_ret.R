# A. Benjamin Diaz
# June 2021
# University of La Verne

rm(list=ls())
setwd('/Users/adiaz3/Desktop/Student Success Research/ROC ret grad')

library(devtools)
library(foreign)
library(ggplot2)
library(dplyr)
library(tidyverse)
library("remotes")
# install.packages("texreg")
# vignette("texreg")
library(texreg)
library(xtable)
library(magrittr)
library(tibble)
library(readxl)
# install.packages("writexl")
library(writexl)
# install.packages("openxlsx")
library(openxlsx)

df  <- read_excel('fall reg reports 2014-2020_ROC_06.18.21.xlsx', sheet="fall reg reports 2014-2020_ROC")

#create new variable for part-time and full-time
df<- df %>%
  mutate(FTPT = if_else(TOTAL_CREDITS <= 12, "PT","FT"))


#filter for every student
df_small <-df %>% filter(ACADEMIC_PERIOD>= 201450 & STUDENT_TYPE %in% c("G","F") & TOTAL_BILLING >=1 &!`ROC Grouping`=="NA" |
                           ACADEMIC_PERIOD >= 201550 & STUDENT_TYPE %in% c("C","Z") & TOTAL_BILLING >=1  &!`ROC Grouping`=="NA" ) 

#filter for NEW students ONLY
df.newstu <-df %>% filter(ACADEMIC_PERIOD>= 201450 & STUDENT_TYPE %in% c("G","F") & TOTAL_BILLING >=1 &!`ROC Grouping`=="NA" )

#filter for CONTINUING students ONLY
df.contstu <- df %>% filter(ACADEMIC_PERIOD >= 201550 & STUDENT_TYPE %in% c("C","Z") & TOTAL_BILLING >=1  &!`ROC Grouping`=="NA") 



#subset to double check against JH's manual work
# df_highdes <- df_small %>% filter(ACADEMIC_PERIOD == 201450 & `ROC Grouping` =="High Desert Victorville Campus")

#extract only the student IDs for comp
# stu.ID <- df_highdes[,c("PERSO","ID")] 



# duplicated(df_small)
# copies <-df_small[duplicated(df_small),]
# rm(copies)
# undupe_df <-df_small[!duplicated(df_small),]
# rm(undupe_df)

# rm(df)
# rm(stu.ID)
# rm(df_highdes)



# df_smaller <- df[,c("PERSO","ID" ,"PROGRAM","STUDENT_TYPE","enr_2014_max","enr_2015_max",
                  # "enr_2016_max","enr_2017_max","enr_2018_max","enr_2019_max","enr_2020_max",
                  # "ACADEMIC_YEAR","ROC Grouping")]
#build logic

# new_students = c("G", "F")

# ret.new <-df %>%
#   mutate(retained = c("No", "Yes")[(STUDENT_TYPE %in% c("G","F") &
#                                      enr_2014 == 1 &
#                                      enr_2015 == 1)+1] )

# ret.new <-df.newstu %>%
#   mutate(retained = c("No", "Yes")[(enr_2014_max == 1 & enr_2015_max == 1 |
#                                      enr_2015_max=1 &enr_2016_max=1 |
#                                      enr_2016_max=1 & enr_2017_max |
#                                      enr_2017_max == 1 & enr_2018_max == 1 |
#                                      enr_2018_max == 1 & enr_2019_max == 1 |
#                                      enr_2019_max == 1 & enr_2020_max == 1)+1])
ret.new <-df.newstu %>%
  mutate(retained = if_else(enr_2014_max == 1 & enr_2015_max == 1 |
                              enr_2015_max==1 & enr_2016_max==1 |
                              enr_2016_max==1 & enr_2017_max==1 |
                              enr_2017_max == 1 & enr_2018_max == 1 |
                              enr_2018_max == 1 & enr_2019_max == 1 |
                              enr_2019_max == 1 & enr_2020_max == 1, "Y","N"))

#creates cohorts, doubled check against academic year, which confirms our findings
ret.test <- ret.new %>%
  mutate(cohort = if_else(enr_2014_max == 1 & enr_2015_max == 1, "2015 Cohort",
                  if_else(enr_2014_max == 0 & enr_2015_max==1 & enr_2016_max==1, "2016 Cohort",
                  if_else(enr_2014_max == 0 & enr_2015_max==0 & enr_2016_max==1 & enr_2017_max==1,"2017 Cohort",
                  if_else(enr_2014_max == 0 & enr_2015_max==0 & enr_2016_max==0 & enr_2017_max==1 & enr_2018_max == 1, "2018 Cohort",
                  if_else(enr_2014_max == 0 & enr_2015_max==0 & enr_2016_max==0 & enr_2017_max==0 & enr_2018_max == 1 & enr_2019_max == 1, "2019 Cohort",
                  if_else(enr_2014_max == 0 & enr_2015_max==0 & enr_2016_max==0 & enr_2017_max==0 & enr_2018_max == 0 & enr_2019_max == 1 & enr_2020_max == 1, "2020 Cohort", "2021 Cohort")))))))

# test.retention <- ret.new[, c("TOTAL_CREDITS", "FTPT")]

ret.cont <-df.contstu %>%
  mutate(retained = if_else(enr_2014_max == 1 & enr_2015_max == 1 |
                              enr_2015_max==1 & enr_2016_max==1 |
                              enr_2016_max==1 & enr_2017_max==1 |
                              enr_2017_max == 1 & enr_2018_max == 1 |
                              enr_2018_max == 1 & enr_2019_max == 1 |
                              enr_2019_max == 1 & enr_2020_max == 1, "Y","N"))

#let's get them into one DF
ret.all <- df_small %>%
  mutate(if_else(enr_2014_max == 1 & enr_2015_max == 1 |
                              enr_2015_max==1 & enr_2016_max==1 |
                              enr_2016_max==1 & enr_2017_max==1 |
                              enr_2017_max == 1 & enr_2018_max == 1 |
                              enr_2018_max == 1 & enr_2019_max == 1 |
                              enr_2019_max == 1 & enr_2020_max == 1, "Y","N"))

# ret.contcopy <-data.frame(ret.cont)
# ret.contcopy <- ret.contcopy[,c("enr_2014_max","enr_2015_max","enr_2016_max",
#                               "enr_2017_max","enr_2018_max","enr_2019_max",
#                               "enr_2020_max","retained")]


# df_ret <- df %>% 
#   mutate(retained = if_else(
#     STUDENT_TYPE %in% new_students && enr_2014 =1 && enr_2015 =1 || enr_2015=1 && enr_2016=1 ||
#       enr_2016=1 && enr_2017=1 || enr_2017=1 && enr_2018=1 || enr_2018=1 && enr_2019=1 ||
#       enr_2019=1 && enr_2020=1,
#     1,0
#   ))





# df2 <- df[,c("first_year_retain","ACADEMIC_YEAR","ROC Grouping")]
# df3 <- df[,c("GRADUATED_IND","ACADEMIC_YEAR","ROC Grouping")]


#########################

#ret.test for reprex, using ret.cont

ret.test <- ret.cont[,c("PERSO","ID","ACADEMIC_PERIOD", "retained")]
new.test <-head(ret.test, n=30L)
datapasta::dpasta(new.test)



tibble::tribble(
                    ~PERSO,      ~ID, ~ACADEMIC_PERIOD, ~retained,
                  10001685, 10109887,         "201750",       "Y",
                  10001685, 10109887,         "201850",       "Y",
                  10001685, 10109887,         "201950",       "Y",
                  10005733, 10162571,         "201550",       "Y",
                  10005787, 10112896,         "201550",       "Y",
                  10005795, 10112901,         "201550",       "Y",
                  10005795, 10112901,         "201650",       "Y",
                  10005795, 10112901,         "201750",       "Y",
                  10020043, 10156305,         "202050",       "Y",
                  10020165, 10122910,         "201750",       "Y",
                  10020165, 10122910,         "201850",       "Y",
                  10020649, 10123585,         "201550",       "N",
                  10028842, 10128545,         "201750",       "Y",
                  10028842, 10128545,         "201850",       "Y",
                  10039633, 10140556,         "201550",       "Y",
                  10039633, 10140556,         "201650",       "Y",
                  10041342, 10113556,         "201850",       "Y",
                  10041342, 10113556,         "201950",       "Y",
                  10041786, 10163034,         "201750",       "Y",
                  10041786, 10163034,         "201850",       "Y",
                  10043780, 10166871,         "201550",       "Y",
                  10043780, 10166871,         "201650",       "Y",
                  10044544, 10167631,         "201550",       "Y",
                  52300090, 10147580,         "201850",       "N",
                  52300740, 10149860,         "201650",       "N",
                  52300749, 10135925,         "201750",       "Y",
                  52300749, 10135925,         "201850",       "Y",
                  52300917, 10140173,         "201650",       "Y",
                  52300917, 10140173,         "201750",       "Y",
                  52300917, 10140173,         "201850",       "Y"
                  )
tibble::tribble(
                    ~PERSONUM,      ~ID, ~ORDER_PERIOD, ~retained,
                  10001685, 10109887,         "201750",       "Y",
                  10001685, 10109887,         "201850",       "Y",
                  10001685, 10109887,         "201950",       "Y",
                  10005733, 10162571,         "201550",       "Y",
                  10005787, 10112896,         "201550",       "Y",
                  10005795, 10112901,         "201550",       "Y",
                  10005795, 10112901,         "201650",       "Y",
                  10005795, 10112901,         "201750",       "Y",
                  10020043, 10156305,         "202050",       "Y",
                  10020165, 10122910,         "201750",       "Y",
                  10020165, 10122910,         "201850",       "Y",
                  10020649, 10123585,         "201550",       "N",
                  10028842, 10128545,         "201750",       "Y",
                  52300090, 10147580,         "201850",       "N",
                  52300740, 10149860,         "201650",       "N",
                  52300749, 10135925,         "201750",       "Y",
                  52300749, 10135925,         "201850",       "Y",
                  52300917, 10140173,         "201650",       "Y",
                  52300917, 10140173,         "201750",       "Y",
                  52300917, 10140173,         "201850",       "Y"
                  )


df_cleaned <- new.test %>% distinct(PERSO, retained,ACADEMIC_PERIOD,.keep_all=TRUE)
df_cleaned
#remove the test data
rm(df_cleaned,ret.test,new.test)
######################## test over



#removed old continuing students (non-distinct student IDs)
ret.cont <- ret.cont %>% distinct(PERSO, retained,.keep_all=TRUE)


#filter for NEW UG
newUG.df <- ret.new %>% filter(STUDENT_CLASSIFICATION <= 4)

#filter for NEW GRDs
newGRD.df <- ret.new %>% filter(STUDENT_CLASSIFICATION > 4)

#filter for CONT UG
contUG.df <- ret.cont %>% filter(STUDENT_CLASSIFICATION <= 4)

#filter for CONT GRDs
contGRD.df <- ret.cont %>% filter(STUDENT_CLASSIFICATION > 4)



# ret_cont_check <- ret_cont[, c("PERSO", "ID", "retained")]

##all checks out!

#lets remove these df so they don't clutter our workspace
rm(ret_cont_check)

##overall new
new2piv <-ret.new %>%
  group_by(ACADEMIC_YEAR,CAMPUS_DESC,retained) %>%
  summarise(n=n()) %>%
  mutate('Percentage of Retained Students' = paste0(round(100*n/sum(n),0),'%'))
new2piv

#overall cont
cont2piv <-ret.cont %>%
  group_by(ACADEMIC_YEAR,`ROC Grouping`,retained) %>%
  summarise(n=n()) %>%
  mutate('Percentage of Retained Students' = paste0(round(100*n/sum(n),0),'%'))
cont2piv

#################

#new UG
newUG2piv <-newUG.df %>%
  group_by(ACADEMIC_YEAR,CAMPUS_DESC,retained) %>%
  summarise(n=n()) %>%
  mutate('Percentage of Retained Students' = paste0(round(100*n/sum(n),0),'%'))


#cont UG
contUG2piv <-contUG.df %>%
  group_by(ACADEMIC_YEAR,`ROC Grouping`,retained) %>%
  summarise(n=n()) %>%
  mutate('Percentage of Retained Students' = paste0(round(100*n/sum(n),0),'%'))


#new GRD
newGRD2piv <-newGRD.df %>%
  group_by(ACADEMIC_YEAR,CAMPUS_DESC,retained) %>%
  summarise(n=n()) %>%
  mutate('Percentage of Retained Students' = paste0(round(100*n/sum(n),0),'%'))


#cont GRD
contGRD2piv <-contGRD.df %>%
  group_by(ACADEMIC_YEAR,`ROC Grouping`,retained) %>%
  summarise(n=n()) %>%
  mutate('Percentage of Retained Students' = paste0(round(100*n/sum(n),0),'%'))

#[, !(names(df) %in% c("n"))] keeps all rows and drops cols with the name "n" from
#our summarise function

#PT/FT
time2piv <-newUG.df %>%
  group_by(ACADEMIC_YEAR,CAMPUS_DESC,FTPT, retained) %>%
  summarise(n=n()) %>%
  mutate('Percentage of Retained Students' = paste0(round(100*n/sum(n),0),'%'))


#########################

#TEST FOR wide pivot format for reprex, using cont2piv


# piv.test <-head(cont2piv, n=10L)
# datapasta::dpasta(piv.test)
# 
# 
# 
# df <-tibble::tribble(
#                   ~YEAR,                    ~Volunteers, ~retained,   ~n, ~Rel.Percentage,
#                             2016,                            "LA",       "N",  51,                             "7%",
#                             2016,                            "LA",       "Y", 685,                            "93%",
#                             2017,                   "Victorville",       "N",  12,                            "16%",
#                             2017,                   "Victorville",       "Y",  66,                            "84%",
#                             2018,                 "Inland Empire",       "N",  33,                            "13%",
#                             2018,                 "Inland Empire",       "Y", 227,                            "87%",
#                             2019,                   "Kern County",       "N",   5,                             "7%",
#                             2019,                   "Kern County",       "Y",  69,                            "93%",
#                             2020,                       "Military",       "N",  61,                            "20%",
#                             2020,                       "Military",       "Y", 243,                            "80%",
#                             2017,                            "LA",       "N",  59,                             "7%",
#                             2017,                            "LA",       "Y", 645,                            "93%",
#                             2016,                   "Victorville",       "N",  15,                            "16%",
#                             2016,                   "Victorville",       "Y",  64,                            "84%",
#                             2019,                 "Inland Empire",       "N",  32,                            "13%",
#                             2019,                 "Inland Empire",       "Y", 221,                            "87%",
#                             2017,                   "Kern County",       "N",   7,                             "7%",
#                             2017,                   "Kern County",       "Y",  73,                            "93%",
#                             2016,                       "Military",       "N",  63,                            "20%",
#                             2016,                       "Military",       "Y", 241,                            "80%"
#                   )
# 
# 
# 
# # df$new1 <- paste0(df$n, " (", df$Rel.Percentage, ")")
# df$neat <- paste0(df$n, " (", df$Rel.Percentage, ")")
# df <- df[, !(names(df) %in% c("n","Rel.Percentage", "new","new1"))]
# df
# wide.test <-df%>%
#   pivot_wider(names_from = YEAR, values_from = neat) 
# wide.test
# 
# rm(piv.test)
# rm(wide.test)


#remove the test data
rm(df_cleaned,ret.test,new.test,new.trim)
######################## test over


#TABLE 1
#overall continuing student retention

cont2piv$fixed <- paste0(cont2piv$`Percentage of Retained Students`, " (", cont2piv$n, ")")
cont2piv.wide <-cont2piv[, !(names(cont2piv) %in% c("n","Percentage of Retained Students"))] %>%
  pivot_wider(names_from = ACADEMIC_YEAR, values_from =fixed )

names(cont2piv.wide)[1] <- "ROC CAMPUS"
#LaTeX
print(xtable(cont2piv.wide, type = "latex"), include.rownames=FALSE, file = "ROC_continuing_Overall_ret.tex")


#TABLE 2
#overall NEW student retention
new2piv$fixed <- paste0(new2piv$`Percentage of Retained Students`, " (", new2piv$n, ")")
new2piv.wide <-new2piv[, !(names(new2piv) %in% c("n","Percentage of Retained Students"))] %>%
  pivot_wider(names_from = ACADEMIC_YEAR, values_from =fixed )

names(new2piv.wide)[1] <- "ROC CAMPUS"
#LaTeX
print(xtable(new2piv.wide, type = "latex"),include.rownames=FALSE, file = "ROC_New_Overall_ret.tex")
# 
# write.xl
# write_xlsx(new2piv.wide, "New_students_all_levels.xlsx")


#TABLE 3
#NEW UG student retention

newUG2piv$fixed <- paste0(newUG2piv$`Percentage of Retained Students`, " (", newUG2piv$n, ")")
newUG2piv.wide <-newUG2piv[, !(names(newUG2piv) %in% c("n","Percentage of Retained Students"))] %>%
  pivot_wider(names_from = ACADEMIC_YEAR, values_from =fixed )

names(newUG2piv.wide)[1] <- "ROC CAMPUS"
#LaTeX
print(xtable(newUG2piv.wide, type = "latex"),include.rownames=FALSE, file = "ROC_new_UG_ret.tex")



#TABLE 4
#CONT UG student rentention
contUG2piv$fixed <- paste0(contUG2piv$`Percentage of Retained Students`, " (", contUG2piv$n, ")")
contUG2piv.wide <-contUG2piv[, !(names(contUG2piv) %in% c("n","Percentage of Retained Students"))] %>%
  pivot_wider(names_from = ACADEMIC_YEAR, values_from =fixed )

names(contUG2piv.wide)[1] <- "ROC CAMPUS"
#LaTeX
print(xtable(contUG2piv.wide, type = "latex"), include.rownames=FALSE, file = "ROC_cont_UG_ret.tex")


#TABLE 5
#CONT GRD student rentention
contGRD2piv$fixed <- paste0(contGRD2piv$`Percentage of Retained Students`, " (", contGRD2piv$n, ")")
contGRD2piv.wide <-contGRD2piv[, !(names(contGRD2piv) %in% c("n","Percentage of Retained Students"))] %>%
  pivot_wider(names_from = ACADEMIC_YEAR, values_from =fixed )

names(contGRD2piv.wide)[1] <- "ROC CAMPUS"
#LaTeX
print(xtable(contGRD2piv.wide, type = "latex"),include.rownames=FALSE, file = "ROC_cont_GRD_ret.tex")



#TABLE 6
#NEW GRD student rentention
newGRD2piv$fixed <- paste0(newGRD2piv$`Percentage of Retained Students`, " (", newGRD2piv$n, ")")
newGRD2piv.wide <-newGRD2piv[, !(names(newGRD2piv) %in% c("n","Percentage of Retained Students"))] %>%
  pivot_wider(names_from = ACADEMIC_YEAR, values_from =fixed )

names(newGRD2piv.wide)[1] <- "ROC CAMPUS"
#LaTeX
print(xtable(newGRD2piv.wide, type = "latex"), include.rownames=FALSE, file = "ROC_new_GRD_ret.tex")
#with numbers not percentages


#TABLE 7 - FT/PT Rates


time2piv$fixed <- paste0(time2piv$`Percentage of Retained Students`, " (", time2piv$n, ")")
time2piv.wide <-time2piv[, !(names(time2piv) %in% c("n","Percentage of Retained Students"))] %>%
  pivot_wider(names_from = ACADEMIC_YEAR, values_from =fixed )

names(time2piv.wide)[1] <- "ROC CAMPUS"
#LaTeX
# print(xtable(time2piv.wide, type = "latex"), include.rownames=FALSE, file = "ROC_continuing_Overall_ret.tex")



#excel for DTB

list_of_datasets <- list("All Levels" = new2piv.wide,
                         "Undergrads" = newUG2piv.wide,
                         "Graduates" = newGRD2piv.wide,
                         "FTPT Total"= time2piv.wide)
# write.xlsx(list_of_datasets, file = "ROC_Retention.xlsx", overwrite = TRUE)
write.xlsx(list_of_datasets, file = "ROC_Retention_v2.xlsx", overwrite = TRUE)

# 
# # cont2piv.wide_num <-cont2piv[, !(names(cont2piv) %in% c("n","Percentage of Retained Students"))]%>%
# #   pivot_wider(names_from = ACADEMIC_YEAR, values_from = n)
# tibble::tribble(
#                                                                 ~PERSO,      ~ID, ~ACADEMIC_PERIOD, ~retained,
#                                                               10001685, 10109887,         "201750",       "Y",
#                                                               10001685, 10109887,         "201850",       "Y",
#                                                               10001685, 10109887,         "201950",       "Y",
#                                                               10005733, 10162571,         "201550",       "Y",
#                                                               10005787, 10112896,         "201550",       "Y",
#                                                               10005795, 10112901,         "201550",       "Y",
#                                                               10005795, 10112901,         "201650",       "Y",
#                                                               10005795, 10112901,         "201750",       "Y",
#                                                               10020043, 10156305,         "202050",       "Y",
#                                                               10020165, 10122910,         "201750",       "Y",
#                                                               10020165, 10122910,         "201850",       "Y",
#                                                               10020649, 10123585,         "201550",       "N",
#                                                               10028842, 10128545,         "201750",       "Y",
#                                                               10028842, 10128545,         "201850",       "Y",
#                                                               10039633, 10140556,         "201550",       "Y",
#                                                               10039633, 10140556,         "201650",       "Y",
#                                                               10041342, 10113556,         "201850",       "Y",
#                                                               10041342, 10113556,         "201950",       "Y",
#                                                               10041786, 10163034,         "201750",       "Y",
#                                                               10041786, 10163034,         "201850",       "Y",
#                                                               10043780, 10166871,         "201550",       "Y",
#                                                               10043780, 10166871,         "201650",       "Y",
#                                                               10044544, 10167631,         "201550",       "Y",
#                                                               52300090, 10147580,         "201850",       "N",
#                                                               52300740, 10149860,         "201650",       "N",
#                                                               52300749, 10135925,         "201750",       "Y",
#                                                               52300749, 10135925,         "201850",       "Y",
#                                                               52300917, 10140173,         "201650",       "Y",
#                                                               52300917, 10140173,         "201750",       "Y",
#                                                               52300917, 10140173,         "201850",       "Y"
#                                                               )
# # cont2piv.wide_num
# # 
# # #overall 
# # n2p.wide <-new2piv[, !(names(new2piv) %in% c("n"))] %>%
# #   pivot_wider(names_from = ACADEMIC_YEAR, values_from = `Percentage of Retained Students`)
# tibble::tribble(
#                                                                                               ~ACADEMIC_YEAR,                    ~ROC.Grouping, ~retained,   ~n, ~Percentage.of.Retained.Students,
#                                                                                                         2016,                           "CAPA",       "N",  51L,                             "7%",
#                                                                                                         2016,                           "CAPA",       "Y", 685L,                            "93%",
#                                                                                                         2016, "High Desert Victorville Campus",       "N",  12L,                            "16%",
#                                                                                                         2016, "High Desert Victorville Campus",       "Y",  64L,                            "84%",
#                                                                                                         2016,           "Inland Empire Campus",       "N",  33L,                            "13%",
#                                                                                                         2016,           "Inland Empire Campus",       "Y", 227L,                            "87%",
#                                                                                                         2016,             "Kern County Campus",       "N",   5L,                             "7%",
#                                                                                                         2016,             "Kern County Campus",       "Y",  69L,                            "93%",
#                                                                                                         2016,                       "Military",       "N",  61L,                            "20%",
#                                                                                                         2016,                       "Military",       "Y", 243L,                            "80%"
#                                                                                               )
# n2p.wide

# tracemem(copy_p)==tracemem(pivot2)




# copy_n2p <-data.frame(new2piv)
# copy_n2p <-copy_n2p[, !(names(copy_n2p) %in% c("n"))]
# 
# 
# 
# copy_c2p <-data.frame(cont2piv)
# copy_c2p <-copy_c2p[, !(names(copy_c2p) %in% c("n"))]

# c2p.wide <-copy_c2p %>%
#   pivot_wider(names_from = ACADEMIC_YEAR, values_from = Percentage.of.Retained.Students)
# c2p.wide
# 
# 
# n2p.wide <-copy_n2p %>%
#   pivot_wider(names_from = ACADEMIC_YEAR, values_from = Percentage.of.Retained.Students)
# n2p.wide

# df.wide <-df.wide[,-c(2)]
# df.wide
# 
# print(xtable(c2p.wide, type = "latex"), file = "ROC_continuing_ret.tex")
# 
# print(xtable(n2p.wide, type = "latex"), file = "ROC_new_ret.tex")

