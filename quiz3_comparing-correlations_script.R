library(apaTables)
library(tidyverse)
library(cocor)
library(predictionInterval)

bfi_data <- read_csv("bfi2.csv") # load data
A1 <- bfi_data$A1 
E1 <- bfi_data$E1
O1 <- bfi_data$O1
N1 <- bfi_data$N1
C1 <- bfi_data$C1
gender <- bfi_data$gender
bfi_data <- cbind(A1,E1,O1,N1,C1,gender)

apa.cor.table(bfi_data)
#create correlation table 

#Q1: compare A1-C1 to E1-O1 (no overlap)
cocor(~A1+C1|E1+O1, data=as.data.frame(bfi_data))
#$\Delta r$ = -.01, 95% CI [-.11,.09]
#retained null hypothesis 

#Q2: compare A1-C1 to A1-E1 (overlap)
cocor(~C1+A1|E1+A1, data=as.data.frame(bfi_data))
#$\Delta r$ = -.08, 95% CI [-.18,.02]
#retained null hypothesis 

#Q3: compare A1-E1 for men and A1-E1 for women 
#1 for males, 2 for females 
bfi_men <- bfi_data %>% filter(gender==1) %>% select (-gender)
bfi_women <- bfi_data %>% filter(gender==2) %>% select (-gender)

bfi_men <- as.data.frame(bfi_men)
bfi_women <- as.data.frame(bfi_women)

cocor(~A1+E1|A1+E1, data=list(bfi_men,bfi_women))
#$\Delta r$ = .02, 95% CI [-.13,.17]
#retained null hypothesis 

#Q4: rating-raises vs. rating-critical 
cocor.dep.groups.overlap(r.jk=.59,r.jh=.16,r.kh=.38,n=30) #using correlation values instead of individual items 
#$\Delta r$ = .43, 95% CI [.07,.79]
#rejected null hypothesis 

#Q5: rating-raises vs. complaints-critical 
cocor.dep.groups.nonoverlap(r.jk=.59,r.hm=.19,r.jh=.83,r.jm=.16,r.kh=.67,r.km=.38,n=30) #using correlation values instead of individual items 
#$\Delta r$ = .40, 95% CI [.02,.78]
#rejected null hypothesis 

#Q6: study 1- rating-raises vs. study 2- rating-raises  
cocor.indep.groups(r1.jk = .59,r2.hm =.03, n1=30, n2=3000)
#$\Delta r$ = .56, 95% CI [.26,.76], p.0008
#Null hypothesis rejected 

#Q7: We can confidently estimate that from different populations. 
