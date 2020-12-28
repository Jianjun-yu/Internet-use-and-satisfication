//read survey 2014 data
use "ecfps2014adult_201906.dta", clear

//I recode the missing and don't know to . and change the variable name.
recode qn8011 -2/-1=. 79=. , gen(myincome2014)
recode cfps2014_age -8=.,gen(age2014)
recode qn401_s_1 1=1 -8/-1=0 2/100=0, gen(partymember2014)
recode qn1101 -9/-1=. 1=5 2=4 3=3 4=2 5=1, gen(evaluation2014) 
gen male2014=cfps_gender
recode cfps2014edu -1=1 -9=. 9=1 , gen(educ2014)
recode urban14 -9=. , gen(urban2014)
recode qea0 2/3=1 -1/1=0 4/5=0, gen(marriage2014)
recode ku2  -1=0, gen(internet2014)
gen province2014=provcd14
recode qn6012 -9/-1=. ,gen(environment2014)
recode qn6011 -9/-1=. ,gen(corruption2014)

//only remind the variables I need
keep pid myincome2014 partymember2014 evaluation2014 male2014 age2014 urban2014 educ2014 marriage2014 internet2014 province2014 environment2014 corruption2014

//save the new data
save "2014data.dta", replace

//read survey 2016 data
use "ecfps2016adult_201906.dta", clear

//recode the missing and do not know to . and change variable name
recode cfps_age -8=.,gen(age2016)
recode qn8011 -1=. 79=. , gen(myincome2016)
recode qn4001 -9=. -1=0 , gen(partymember2016)
recode qn1101 -9/-1=. 79=., gen(evaluation2016)
recode evaluation2016 1=5 2=4 3=3 4=2 5=1
gen male2016=cfps_gender
recode male2016 -8=.
gen educ2016=cfps2016edu 
recode urban16 -9=. , gen(urban2016)
recode qea0 2/3=1 -1/1=0 4/5=0, gen(marriage2016)
gen internet2016=0
replace internet2016=1 if ku201==1 | ku202==1
recode provcd16  -9=. , gen(province2016)
recode qn6012 -9/-1=. ,gen(environment2016)
recode qn6011 -9/-1=. ,gen(corruption2016)

//only keep the variables I need 
keep pid myincome2016 partymember2016 evaluation2016 male2016 age2016 urban2016 educ2016 marriage2016 internet2016 province2016 environment2016 corruption2016

//save new data 
save "2016data.dta", replace

//read 2018 survey data
 use "cfps2018person_201911.dta", clear 

 //recode the missing and do not know to . and change the name of variable 
recode qn8011 -2/-1=. 79=. , gen(myincome2018)
recode party -8=0 , gen(partymember2018)
recode qn1101 -9/-1=. , gen(evaluation2018)
recode evaluation2018 1=5 2=4 3=3 4=2 5=1
gen male2018=gender
gen educ2018=cfps2018edu 
recode urban18 -9=. , gen(urban2018)
recode qea0 2/3=1 1=0 4/5=0, gen(marriage2018)
gen internet2018=0
gen age2018 = age
replace internet2018=1 if qu201==1 | qu202==1
gen province2018=provcd18
recode qn6012 -9/-1=. ,gen(environment2018)
recode qn6011 -9/-1=. ,gen(corruption2018)

//only keep the variables I need
keep pid myincome2018 partymember2018 evaluation2018 male2018 age2018 urban2018 educ2018 marriage2018 internet2018 province2018 environment2018 corruption2018

save new data
save "2018data.dta",replace

//merge data 2018 with 2016
merge m:m pid using "2016data.dta"
 
//drop _merge generate in the last merge practice
drop _merge

//merge data 2014 with the other two data sets
merge m:m pid using "2014data.dta"
 
//the the data I use for analysis 
save "date from 2014 to 2018.dta", replace


