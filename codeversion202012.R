#import data#
library(haven)
#recode data#
library(dplyr)
#recode data with label#
library(labelled)
#logistic regression#
library(MASS)
#for tidy: manipulate model output easier#
library(broom)
#draw figure#
library(ggplot2)
#dot and whisker digure#
library(dotwhisker)

###import data, change the direction to your local field###
cfps2010adult_202008 <- read_dta("D:/git/version202011/cfps2010adult_202008.dta")
ecfps2014adult_201906 <- read_dta("D:/git/version202011/ecfps2014adult_201906.dta")
ecfps2016adult_201906 <- read_dta("D:/git/version202011/ecfps2016adult_201906.dta")
cfps2018person_201911 <- read_dta("D:/git/version202011/cfps2018person_201911.dta")

###recode data###
data2010 <- cfps2010adult_202008 %>% mutate(
                      myincome=recode(qm401, '-8'=NA_real_),
                      age=qa1age,
                      partymember=recode(qa7_s_1,'1'=1, '-2'=NA_real_, '-1'=NA_real_, .default = 0),
                      evaluation=recode(qn4,'1'=5,'2'=4 ,'3'=3,'4'=2,'5'=1, .default = NA_real_),
                      male=gender,
                      educ=recode(cfps2010edu_best,'-1'=1),
                      marriage=recode(qe1, '2'=1, '3'=1, '-1'=NA_real_, '-2'=NA_real_, '-8'=NA_real_,'1'=0,'4'=0, '5'=0),
                      internet=recode(ku2, '-1'=0, '-8'=0),
                      province=provcd
                                           )
data2014 <- ecfps2014adult_201906 %>% mutate(
                      myincome=recode(qn8011, '-2'= NA_real_, '-1'= NA_real_, '79'= NA_real_ ),
                      age=recode(cfps2014_age, '-8'= NA_real_), 
                      partymember=recode(qn401_s_1, '1'=1, '-9'=NA_real_, .default = 0),
                      evaluation=recode(qn1101, '1'=5,'2'=4 ,'3'=3,'4'=2,'5'=1, .default = NA_real_),
                      male=cfps_gender,
                      educ=recode(cfps2014edu,'-1'=1,'9'=1, '-9'=NA_real_),
                      urban=recode(urban14, '-9'=NA_real_),
                      marriage=recode(qea0, '2'=1, '3'=1, '-1'=0,'0'=0,'1'=0,'4'=0, '5'=0),
                      internet=recode(ku2, '-1'=0),
                      province=provcd14,
                      environment=recode(qn6012,'-8'=NA_real_,'-2'=NA_real_, '-1'=NA_real_),
                      corruption=recode(qn6011, '-8'=NA_real_,'-2'=NA_real_, '-1'=NA_real_)
                                            )
data2016 <- ecfps2016adult_201906 %>% mutate(
                      myincome=recode(qn8011, '-1'= NA_real_, '79'= NA_real_),
                      age =recode(cfps_age, '-8'= NA_real_),
                      partymember=recode(qn4001, '-1'=0, '1'=1,'-9'=NA_real_),
                      evaluation=recode(qn1101, '1'=5,'2'=4 ,'3'=3,'4'=2,'5'=1, .default = NA_real_),
                      male=recode(cfps_gender,'-8'=NA_real_),
                      educ=cfps2016edu,
                      urban=recode(urban16, '-9'=NA_real_),
                      marriage=recode(qea0,'2'=1, '3'=1, '-1'=0,'0'=0,'1'=0,'4'=0, '5'=0, '-2'=NA_real_),
                      internet=case_when(ku201==1|ku202==1 ~ 1,
                                         is.na(ku201)&is.na(ku202) ~ NA_real_,
                                         TRUE ~ 0),
                      province=recode(provcd16, '-9'=NA_real_),
                      environment=recode(qn6012,'-9'=NA_real_,'-2'=NA_real_, '-1'=NA_real_),
                      corruption=recode(qn6011, '-9'=NA_real_,'-2'=NA_real_, '-1'=NA_real_)
                                            )
data2018 <-  cfps2018person_201911 %>% mutate(
                      myincome=recode(qn8011, '-1'= NA_real_,'-2'=NA_real_, '79'= NA_real_),
                      partymember=recode(party,'-8'=0),
                      evaluation=recode(qn1101, '1'=5,'2'=4 ,'3'=3,'4'=2,'5'=1, .default = NA_real_),
                      male=gender,
                      educ=cfps2018edu,
                      urban=recode(urban18,'-9'=NA_real_),
                      marriage=recode(qea0,'2'=1, '3'=1, '1'=0,'4'=0, '5'=0, .default= NA_real_),
                      internet=case_when(qu201==1|qu202==1 ~ 1,
                                         is.na(qu201)&is.na(qu202) ~ NA_real_,
                                         TRUE ~ 0),
                      province=provcd18,
                      environment=recode(qn6012,'-9'=NA_real_,'-2'=NA_real_, '-1'=NA_real_),
                      corruption=recode(qn6011, '-9'=NA_real_,'-2'=NA_real_, '-1'=NA_real_)
                                               )

###only keep data I use###
variables <- c('pid', 'myincome', 'age', 'partymember', 'evaluation','male','educ','urban','marriage','internet','province','environment','corruption')

data2010 <- data2010[variables[-c(12,13)]]
data2014 <- data2014[variables]
data2016 <- data2016[variables]
data2018 <- data2018[variables]

###check the result of recoding###
f <- function(a,c){
  attach(a)
  for(i in 1:length(c)){
    c[[i]]%>%print()
    c[[i]]%>%get()%>%table()%>%print()
  }
  rm(i)
  detach(a)
}

c <- list('myincome', 'age', 'partymember', 'evaluation','male','educ','urban','marriage','internet','province','environment','corruption')

f(data2014,c)
f(data2016,c)
f(data2018,c)


###release memory###
rm(cfps2010adult_202008)
rm(cfps2018person_201911)
rm(ecfps2014adult_201906)
rm(ecfps2016adult_201906)

###model for figure 1###
model1.1 <- polr(formula = factor(evaluation) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2010, Hess = TRUE )
model1.2 <- polr(formula = factor(evaluation) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2014, Hess = TRUE )
model1.3 <- polr(formula = factor(evaluation) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2016, Hess = TRUE )
model1.4 <- polr(formula = factor(evaluation) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2018, Hess = TRUE )

###figure1###
model1.1.1 <- tidy(model1.1) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2010")
model1.2.1 <- tidy(model1.2) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2014")
model1.3.1 <- tidy(model1.3) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2016")
model1.4.1 <- tidy(model1.4) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2018")
draw1 <- rbind(model1.1.1,model1.2.1,model1.3.1,model1.4.1)

figure1 <- dwplot(draw1, 
                  vline = geom_vline(xintercept = 0, 
                                     colour = "grey60", 
                                     linetype = 2)
                  ) %>% relabel_predictors(
  c(
    internet = "Internet use",
    age = "Age",
    male = "Male",
    myincome = "Income",
    educ = "Education Level",
    urban = "Urban Resident",
    marriage = "Marriage",
    partymember = "Party Member"
   )
                                          )+ 
  theme_bw() +
  xlab("Coefficient Estimate") +
  ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("The Evaluation of Government") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.7, 0.75),
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"), legend.title = element_blank()
        )


##create the subset of traced respondents##
data201014 <- inner_join(data2010,data2014, by="pid")
data201016 <- inner_join(data201014,data2016, by="pid")
data201018 <- inner_join(data201016,data2018, by="pid")

data201014 <- rename(data201014,
                     c(myincome=myincome.y, 
                       age=age.y, 
                       partymember=partymember.y, 
                       evaluation=evaluation.y,
                       male=male.y,educ=educ.y,
                       urban=urban.y,
                       marriage=marriage.y,
                       internet=internet.y,
                       province=province.y
                       )
                     )

data201018 <- rename(data201018,
                     c(myincome=myincome.y.y, 
                       age=age.y.y, 
                       partymember=partymember.y.y, 
                       evaluation=evaluation.y.y,
                       male=male.y,educ=educ.y.y,
                       urban=urban.y.y,
                       marriage=marriage.y.y,
                       internet=internet.y.y,
                       province=province.y.y
                     )
                    )

##model for figure2: respondents whose internet availability has not changed from the last survey. 
model2.1 <- polr(formula = factor(evaluation) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2010, Hess = TRUE )
model2.2 <- polr(formula = factor(evaluation) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), 
                 data = subset(data201014, (data201014$internet.x==1&data201014$internet==1)|(data201014$internet.x==0&data201014$internet==0)), Hess = TRUE )
model2.3 <- polr(formula = factor(evaluation) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), 
                 data = subset(data201016, 
                              (data201016$internet==1&data201016$internet.y==1)|(data201016$internet==0&data201016$internet.y==0)),
                 Hess = TRUE )
model2.4 <- polr(formula = factor(evaluation) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), 
                data = subset(data201018, 
                             (data201018$internet==1&data201018$internet.x.x==1)|(data201018$internet==0&data201018$internet.x.x==0)),
                Hess = TRUE )


###figure2###
model2.1.1 <- tidy(model2.1) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2010")
model2.2.1 <- tidy(model2.2) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2014")
model2.3.1 <- tidy(model2.3) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2016")
model2.4.1 <- tidy(model2.4) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2018")
draw2 <- rbind(model2.1.1,model2.2.1,model2.3.1,model2.4.1)

figure2 <- dwplot(draw2, 
                  vline = geom_vline(xintercept = 0, 
                                     colour = "grey60", 
                                     linetype = 2)
) %>% relabel_predictors(
  c(
    internet = "Internet use",
    age = "Age",
    male = "Male",
    myincome = "Income",
    educ = "Education Level",
    urban = "Urban Resident",
    marriage = "Marriage",
    partymember = "Party Member"
  )
)+ 
  theme_bw() +
  xlab("Coefficient Estimate") +
  ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("The Evaluation of Government (without new users)") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.7, 0.75),
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"), legend.title = element_blank()
  )

##difference in difference test##
##specify the variables from different panels##
datadid <- rename(data201018,
                     c(evaluation2018=evaluation,
                       internet2018=internet,
                       evaluation2016=evaluation.x.x,
                       evaluation2014=evaluation.y,
                       evaluation2010=evaluation.x,
                       internet2016=internet.x.x,
                       internet2014=internet.y,
                       internet2010=internet.x
                      )
                  )

##creat the control group and the treatment group## 
##control group: who have not use the internet from 2010
##treatmeant group: who did not use the interent before 2014 but begin using the internet after 2014##
control <- subset(datadid, datadid$internet2010==0&datadid$internet2014==0&datadid$internet2016==0&datadid$internet2018==0)
treatment <- subset(datadid, datadid$internet2010==0&datadid$internet2014==0&datadid$internet2016==1&datadid$internet2018==1)

##the difference of the change of evaluation from 2010 to 2016 between control group and treatment group##
dicontrol2016 <- control$evaluation2016-control$evaluation2010
ditreatment2016 <- treatment$evaluation2016-treatment$evaluation2010
t.test(dicontrol2016,ditreatment2016)

##the difference of the change of evaluation from 2010 to 2018 between control group and treatment group##
dicontrol2018 <- control$evaluation2018-control$evaluation2010
ditreatment2018 <- treatment$evaluation2018-treatment$evaluation2010
t.test(dicontrol2018,ditreatment2018)

##model for figure 3. the panel 2010 did not ask question about corruption and environmental pollution##
model3.2 <- polr(formula = factor(corruption) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2014, Hess = TRUE )
model3.3 <- polr(formula = factor(corruption) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2016, Hess = TRUE )
model3.4 <- polr(formula = factor(corruption) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2018, Hess = TRUE )

##model for figure 4. the panel 2010 did not ask question about corruption and environmental pollution##
model4.2 <- polr(formula = factor(environment) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2014, Hess = TRUE )
model4.3 <- polr(formula = factor(environment) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2016, Hess = TRUE )
model4.4 <- polr(formula = factor(environment) ~ internet + age + male + myincome + educ + urban + marriage + partymember + factor(province), data = data2018, Hess = TRUE )

##figure 3##
model3.2.1 <- tidy(model3.2) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2014")
model3.3.1 <- tidy(model3.3) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2016")
model3.4.1 <- tidy(model3.4) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2018")
draw3 <- rbind(model3.2.1,model3.3.1,model3.4.1)

figure3 <- dwplot(draw3, 
                  vline = geom_vline(xintercept = 0, 
                                     colour = "grey60", 
                                     linetype = 2)
) %>% relabel_predictors(
  c(
    internet = "Internet use",
    age = "Age",
    male = "Male",
    myincome = "Income",
    educ = "Education Level",
    urban = "Urban Resident",
    marriage = "Marriage",
    partymember = "Party Member"
  )
)+ 
  theme_bw() +
  xlab("Coefficient Estimate") +
  ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("The Severity of Corruption") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.05, 0.75),
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"), legend.title = element_blank()
  )

##figure4##

model4.2.1 <- tidy(model4.2) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2014")
model4.3.1 <- tidy(model4.3) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2016")
model4.4.1 <- tidy(model4.4) %>% filter(!grepl('province*', term)) %>% mutate(model = "Survey 2018")
draw4 <- rbind(model4.2.1,model4.3.1,model4.4.1)

figure4 <- dwplot(draw4, 
                  vline = geom_vline(xintercept = 0, 
                                     colour = "grey60", 
                                     linetype = 2)
) %>% relabel_predictors(
  c(
    internet = "Internet use",
    age = "Age",
    male = "Male",
    myincome = "Income",
    educ = "Education Level",
    urban = "Urban Resident",
    marriage = "Marriage",
    partymember = "Party Member"
  )
)+ 
  theme_bw() +
  xlab("Coefficient Estimate") +
  ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("The Severity of Environment Pollution") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.7, 0.05),
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"), legend.title = element_blank()
  )

