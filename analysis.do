
//A program to change the coefficient name of my regression. 
program coefnamechange, eclass
         matrix coename=e(b)
         matrix colnames coename = "Internet" "age" "male" "education" "urban" "partynumber" "marriage" "income" "province"
         ereturn repost b = coename, rename
 end
********************************************************************************
//Read data. Do not forget to change your working directory
use "date from 2014 to 2018.dta", replace
 
********************************************************************************
//Creat the first graph: the result of the effect of internet use on the evaluation of government performance
foreach num of numlist 2014 2016 2018{
dis `num'
ologit evaluation`num' internet`num' age`num' male`num' educ`num' urban`num' partymember`num' marriage`num' myincome`num' i.province`num'
coefnamechange
estimates store evaluation`num'
}

//Creat the graph
coefplot evaluation2014 evaluation2016 evaluation2018 , drop(*province* ) xline(0) scheme(s1color) title(The evaluation of government performance) legend(row(1) label(4 "performance2016") label(2 "performance2014") label(6 "performance2018"))
graph export "evaluation1.png", as(png) name("Graph") replace

//For people who want to review the detailed result
esttab evaluation2014 evaluation2016 evaluation2018 , drop(*province* ) title(The evaluation of government performance) mtitles(2014 2016 2018)

********************************************************************************
//Creat the second graph: the result of the effect of internet use on the evaluation of government performance without new interent users
foreach num of numlist 2016 2018{
dis `num'
local a=`num'-2
ologit evaluation`num' internet`num' age`num' male`num' educ`num' urban`num' partymember`num' marriage`num' myincome`num' i.province`num' if internet`num'!=1|internet`a'!=0
coefnamechange 
estimates store evaluation_h1`num'
}

//Creat the graph
coefplot evaluation2014 evaluation_h12016 evaluation_h12018, drop(*province* ) xline(0) scheme(s1color)  title(The evaluation of government performance) subtitle(without new users) legend(row(1) label(4 "performance2016") label(2 "performance2014") label(6 "performance2018"))
graph export "evaluation2.png", as(png) name("Graph") replace

//For people who want to review the detailed result 
esttab evaluation2014 evaluation_h12016 evaluation_h12018 , drop(*province* ) title(The evaluation of government performance(without new nusers)) mtitles(2014 2016 2018)

********************************************************************************
//Creat the third graph: the result of the effect of internet use on the evaluation of the severity of environmential pollution
foreach num of numlist 2014 2016 2018{
dis `num'
ologit environment`num' internet`num' age`num' male`num' educ`num' urban`num' partymember`num' marriage`num' myincome`num' i.province`num'
coefnamechange
estimates store environment`num'
}

//Creat the graph
coefplot environment2014 environment2016 environment2018 , drop(*province* ) xline(0) title(The evaluation of environmential pollution) legend(row(1) label(2 "pollution2014") label(4 "pollution2016") label(6 "pollution2018")) scheme(s1color)
graph export "pollution.png", as(png) name("Graph") replace

//For people who want to review the detail result
esttab environment2014 environment2016 environment2018 , drop(*province* ) title(The evaluation of environmential pollution) mtitles(2014 2016 2018)

********************************************************************************
//Creat the fourth graph: the result of the effect of internet use on the evaluation of the severity of corruption
foreach num of numlist 2014 2016 2018{
dis `num'
ologit corruption`num' internet`num' age`num' male`num' educ`num' urban`num' partymember`num' marriage`num' myincome`num' i.province`num'
coefnamechange
estimates store corruption`num'
}

//Creat the graph
coefplot corruption2014 corruption2016 corruption2018 , drop(*province* ) xline(0) title(The evaluation of corruption) scheme(s1color) legend(row(1) label(2 "corruption2014") label(4 "corruption2016") label(6 "corruption2018"))
graph export "corruption.png", as(png) name("Graph") replace
esttab corruption2014 corruption2016 corruption2018 , drop(*province* ) title(The evaluation of corruption) mtitles(2014 2016 2018)
