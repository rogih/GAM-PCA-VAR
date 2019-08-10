Generalized additive model with principal component analysis: an application to time series of respiratory disease and air pollution data
J. B. de Souza, V. A. Reisen, G. C. Franco, M. Ispány, P. Bondon and J. Meri Santos
Appl. Statist., 67 (2018), 453 -- 480


Description of the data set in the file Pollutant_data
The data set contains the daily measurements of the following variables between January 1, 2005 and December 31, 2010 (n=2191)
The response variable (Admissions) was obtained from the main childrens emergency department in the Vitoria Metropolitan Area (Brazil), called Hospital Infantil Nossa Senhora da Gloria
The pollutant covariates and confounding variables (temperature and humidity) were obtained from 
Variable name : description
Admissions : number of hospital admissions for respiratory diseases (integer)
Time : timestamp (integer)
PM10 : concentration of particulate material smaller than 10 micron in microgram/cubic meter (real)
SO2	: concentration of sulphur dioxide in microgram/cubic meter (real)
NO2	: concentration of nitrogen dioxide in microgram/cubic meter (real)
CO : concentration of carbon monoxide in microgram/cubic meter (real)
O3 : concentration of ozone in microgram/cubic meter (real)
Tmax : Daily maximum temperature in C (real)
Tmaxm12 : Maximum temperature between 0-12 in C (real)
Tave : Daily average temperature in C (real)
Tmin : Daily minimum temperature in C (real)	
RH : Air relative humidity in % (real)
RHm01 : Air relative humidity in the first hour of the day % (real)
RHm12 : Air relative humidity between 0-12 in % (real)
Tuesday	: day indicator (boolean)
Wednesday : day indicator (boolean)
Thursday : day indicator (boolean)
Friday : day indicator (boolean)
Saturday : day indicator (boolean)
Sunday : day indicator (boolean)
Carn : Carnival (boolean)
Tirad : Tiradentes day (boolean)
Corpus : Corpus Christ (boolean)
Indep : Brazil's Independence day (boolean)
Penha : Our Lady of Penha (boolean)

The R codes are in the file RD_GAM_PCA_VAR with comments.
Dependency on the following R packages: gam, vegan, vars, MTS


Márton Ispány
Faculty of Informatics
University of Debrecen
Debrecen
Kassai út 26
4028 Debrecen
Hungary

E-mail: ispany.marton@inf.unideb.hu