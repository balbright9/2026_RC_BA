####Set up####


if (!require("tidyverse")) install.packages("tidyverse")
if (!require("palmerpenguins")) install.packages("palmerpenguins")
if (!require("nycflights13")) install.packages("nycflights13")
if (!require("rstudioapi")) install.packages("rstudioapi")
if (!require("readxl")) install.packages("readxl")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("readxl")) install.packages("readxl")
if (!require("babynames")) install.packages("babynames")
if (!require("words")) install.packages("words")

#library loadings
library(tidyverse)
library(palmerpenguins)
library(rstudioapi)
library(nycflights13)
library(readxl)
library(ggthemes)
library(babynames)
library(words)

#setting a working directory
#following command assumes you have rstudioapi installed/loaded and sets working directory to script directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

####Code####

#load google sheet data
library(googlesheets4)

read_sheet('https://docs.google.com/spreadsheets/d/1237CQh0_OLojQDOFQ211m5mpYNDEyXRiOpKbqjyzxIk/edit?usp=sharing')
df<-read_sheet('https://docs.google.com/spreadsheets/d/1237CQh0_OLojQDOFQ211m5mpYNDEyXRiOpKbqjyzxIk/edit?usp=sharing')

#wrong axes standard curve lol
ggplot(table212, aes(x=ug_oil_standards_dup, y=standards_dups_abs )) +
  geom_point()+ 
  geom_line()+
  labs(title = "Standard Curve",
       x = "ug_oil",
       y = "Absorbance")


#2/12/26 vanillin
read_sheet('https://docs.google.com/spreadsheets/d/1237CQh0_OLojQDOFQ211m5mpYNDEyXRiOpKbqjyzxIk/edit?usp=sharing')
table212 <- read_sheet('https://docs.google.com/spreadsheets/d/1237CQh0_OLojQDOFQ211m5mpYNDEyXRiOpKbqjyzxIk/edit?usp=sharing')

#correct condensed standard curve plot
ggplot(table212, aes(x=(ug_oil_standards_dup), y=standards_dups_abs))+
    geom_point()+
    geom_smooth(method = "lm")+
    annotate("point", x=0.0478, y=0.090, color="red")

#object of standard curve for values
standard_curve<-lm( standards_dups_abs ~ ug_oil_standards_dup, data=table212)

#command to produce slope and intercept
coefficients(standard_curve)

#r squared value
summary(standard_curve)$r.squared

#2/26/26 vanillin with 50 uL H2SO4 instead
read_sheet('https://docs.google.com/spreadsheets/d/11fpAMSPV71ban53GVycqCJZfV5CO5MFPMc32icr_qz8/edit?usp=sharing')
table226 <- read_sheet('https://docs.google.com/spreadsheets/d/11fpAMSPV71ban53GVycqCJZfV5CO5MFPMc32icr_qz8/edit?usp=sharing')

#standard curve
ggplot(table226, aes(x=(Ug_oil_standards), y=Standards_Dups))+
  geom_point()+
  geom_smooth(method = "lm")

#plotting samples on the standard curve
ggplot(table226, aes(x=(Ug_oil_standards), y=Standards_Dups))+
  geom_point()+
  geom_point(aes(x=(Ug_oil_standards), y=(Samples), color = Samples))

  
#object of standard curve for values
standard_curve<-lm( Standards_Dups ~ Ug_oil_standards, data=table226)

#command to produce slope and intercept
coefficients(standard_curve)

#r squared value
summary(standard_curve)$r.squared


#getting fat amounts for real samples from 5/13/26 assay
read_sheet('https://docs.google.com/spreadsheets/d/1vsN_TfIDOMNZ5XoZhZQXl85cQ-jBGmAAA6zNoIMIwVI/edit?usp=sharing')
table513 <- read_sheet('https://docs.google.com/spreadsheets/d/1vsN_TfIDOMNZ5XoZhZQXl85cQ-jBGmAAA6zNoIMIwVI/edit?usp=sharing')

#standard curve
ggplot(table513, aes(x=(ug_oil_standards), y=Abs_oil_standards_dup))+
  geom_point()+
  geom_smooth(method = "lm")

#plotting samples on the standard curve
ggplot(table226, aes(x=(Ug_oil_standards), y=Standards_Dups))+
  geom_point()+
  geom_point(aes(x=(Ug_oil_standards), y=(Samples), color = Samples))


#object of standard curve for values
standard_curve<-lm( Standards_Dups ~ Ug_oil_standards, data=table226)

#command to produce slope and intercept
coefficients(standard_curve)

#r squared value
summary(standard_curve)$r.squared
