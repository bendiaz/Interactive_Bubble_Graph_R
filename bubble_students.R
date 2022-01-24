# A. Benjamin Diaz
# June 2021
# University of La Verne

rm(list=ls())
setwd('/Users/adiaz3/Desktop/Student Success Research/project 7')
#development tools if we choose to use it
library(devtools)
#for graphing
library(plotly)
library(ggplot2)
#data cleaning and summary tables
library(dplyr)
library(tidyverse)
#regex
library(texreg)
#for creating cross tables
library(xtable)
#for pipe operations
library(magrittr)
#creating tibbles
library(tibble)
#for manipulating excel
library(readxl)
library(writexl)
library(openxlsx)
#for web apps
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)




df  <- read_excel('Student Course 18_19_20 101520.xlsx', sheet="Export Worksheet")
#create factors for graph
df$STUDENT_CAMPUS_DESC <- as.factor(df$STUDENT_CAMPUS_DESC)
df$COURSE_CAMPUS_DESC <- as.factor(df$COURSE_CAMPUS_DESC)
df$MAJOR_DESC <- as.factor(df$MAJOR_DESC)
df$PROGRAM_DESC <- as.factor(df$PROGRAM_DESC)
df$COURSE_DEPARTMENT <- as.factor(df$COURSE_DEPARTMENT)
df$SUBJECT <-as.factor(df$SUBJECT)

df$COURSE_BILLING_CREDITS
#remove letters
df$COURSE_NUMBER <-gsub("[^0-9.-]", "",df$COURSE_NUMBER )


#plotly figure
plotlyfigure <- plot_ly(df, x= ~STUDENT_CAMPUS_DESC, y = ~ COURSE_CAMPUS_DESC,
                        text = ~sum(COURSE_BILLING_CREDITS) ,type= 'scattergl', mode= 'markers',
                        marker = list(size = ~COURSE_BILLING_CREDITS, opacity = 0.5, sizemode='diameter'))
plotlyfigure <- plotlyfigure %>% layout(title = 'Course Campus vs Student Campus by Billing Hours',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE),
                      showlegend = FALSE)
plotlyfigure


bubble_students <- Dash$new()
bubble_students$layout(
  htmlDiv(
    list(
      dccGraph(figure = plotlyfigure)
    )
  )
)
app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)

 # df_new<-df %>%
 #  group_by(ACADEMIC_YEAR,CAMPUS_DESC,retained) %>%
 #  summarise(n=n()) %>%
 #  mutate('Percentage of Retained Students' = paste0(round(100*n/sum(n),0),'%'))




