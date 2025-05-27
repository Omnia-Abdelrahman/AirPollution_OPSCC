/* Create a library called opscc */
libname opscc '/home/u63900618/'; 

/* Import the data into the opscc library */
proc import datafile="/home/u63900618/Book1SAS.csv" 
    out=opscc.dataset_name  
    dbms=csv
    replace;
    getnames=yes;
run;

proc logistic data=opscc.dataset_name;
    model region(event='1') = age SES smk_cat COPD;
    output out=ps_data p=propensity_score;
run;
data weighted_data;
    set ps_data;
    if region = 1 then weight = 1 / propensity_score;
    else weight = 1 / (1 - propensity_score);
run;
proc phreg data=weighted_data;
    model ttr*rec(0) = pm_cat age SES smk_cat COPD;
    weight weight;   /* Apply the weights */
    hazardratio 'Region' pm_cat;
    title1 'Long-term Recurrence: Cox Proportional Hazards Model';
run;

/* Short-term recurrence model (within 3 years) */
proc phreg data=weighted_data;
    where ttr <= 36; /* ttr is time to recurrence in months; censor after 36 months (3 years) */
    model ttr*rec(0) = pm_cat age SES smk_cat COPD;
    weight weight;
    hazardratio 'Region' pm_cat;
    title1 '3-Year Recurrence: Cox Proportional Hazards Model';
run;
/* Five-year recurrence model (within 5 years) */
proc phreg data=weighted_data;
    where ttr <= 60; /* Censor after 60 months (5 years) */
    model ttr*rec(0) = pm_cat age SES smk_cat COPD;
    weight weight;
    hazardratio 'Region' pm_cat;
    title1 '5-Year Recurrence: Cox Proportional Hazards Model';
run;

