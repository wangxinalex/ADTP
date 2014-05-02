#Read all data
bicycle = read.csv("hour3.csv", header=T)
attach(bicycle)
lm.fit_all = lm(cnt~.-registered-casual,data=bicycle)
summary(lm.fit_all)
#Show all dummy variables
contrasts(season)
#All non-linear predictors and interactive variables
lm.fit_cas = lm(casual~season+yr+mnth+hr+holiday+weekday+weathersit+(temp+hum+windspeed)^2+poly(hum,4)+I(windspeed^2),data=bicycle)
summary(lm.fit_cas)
> lm.fit_cas = lm(casual~season+yr+mnth+hr+holiday+weekday+weathersit+(temp+hum+windspeed)^2+poly(hum,4)+I(windspeed^2),data=bicycle)
> summary(lm.fit_cas)

Call:
lm(formula = casual ~ season + yr + mnth + hr + holiday + weekday + 
    weathersit + (temp + hum + windspeed)^2 + poly(hum, 4) + 
    I(windspeed^2), data = bicycle)

Residuals:
    Min      1Q  Median      3Q     Max 
-91.485 -18.393  -3.001  12.852 251.042 

Coefficients: (1 not defined because of singularities)
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)     -36.9387     4.0705  -9.075  < 2e-16 ***
seasonSpring     -1.5635     1.7591  -0.889 0.374127    
seasonSummer      9.0139     1.5203   5.929 3.10e-09 ***
seasonWinter      0.4354     1.5790   0.276 0.782745    
yry1             11.6976     0.4811  24.314  < 2e-16 ***
mnthm10          14.1620     1.9941   7.102 1.28e-12 ***
mnthm11           5.3110     1.9123   2.777 0.005487 ** 
mnthm12          -1.1146     1.5199  -0.733 0.463347    
mnthm2           -1.2003     1.1986  -1.001 0.316646    
mnthm3            9.2682     1.3535   6.847 7.77e-12 ***
mnthm4            5.0657     2.0031   2.529 0.011452 *  
mnthm5            7.9913     2.1474   3.721 0.000199 ***
mnthm6           -1.9915     2.2048  -0.903 0.366402    
mnthm7           -1.2365     2.4829  -0.498 0.618480    
mnthm8            4.6355     2.4157   1.919 0.055016 .  
mnthm9           14.6029     2.1552   6.776 1.28e-11 ***
hrh1             -2.6318     1.6318  -1.613 0.106806    
hrh10            31.0652     1.6412  18.928  < 2e-16 ***
hrh11            40.9410     1.6542  24.750  < 2e-16 ***
hrh12            47.2065     1.6703  28.262  < 2e-16 ***
hrh13            48.9847     1.6830  29.106  < 2e-16 ***
hrh14            50.6534     1.6939  29.903  < 2e-16 ***
hrh15            49.5307     1.6986  29.159  < 2e-16 ***
hrh16            48.7719     1.6943  28.786  < 2e-16 ***
hrh17            51.0527     1.6829  30.337  < 2e-16 ***
hrh18            39.7861     1.6690  23.838  < 2e-16 ***
hrh19            30.1419     1.6518  18.248  < 2e-16 ***
hrh2             -3.7206     1.6378  -2.272 0.023113 *  
hrh20            20.0313     1.6424  12.196  < 2e-16 ***
hrh21            14.0805     1.6348   8.613  < 2e-16 ***
hrh22             9.7283     1.6316   5.963 2.53e-09 ***
hrh23             4.1645     1.6298   2.555 0.010623 *  
hrh3             -5.7562     1.6497  -3.489 0.000486 ***
hrh4             -6.4031     1.6518  -3.876 0.000106 ***
hrh5             -4.9352     1.6415  -3.006 0.002647 ** 
hrh6             -1.7698     1.6374  -1.081 0.279787    
hrh7              4.1843     1.6336   2.561 0.010432 *  
hrh8             12.5246     1.6313   7.677 1.71e-14 ***
hrh9             19.0738     1.6336  11.676  < 2e-16 ***
holidayYes       22.1754     1.4904  14.879  < 2e-16 ***
weekdayw1       -31.5364     0.9084 -34.715  < 2e-16 ***
weekdayw2       -34.1844     0.8870 -38.538  < 2e-16 ***
weekdayw3       -34.4817     0.8859 -38.922  < 2e-16 ***
weekdayw4       -34.2482     0.8869 -38.616  < 2e-16 ***
weekdayw5       -26.7270     0.8824 -30.288  < 2e-16 ***
weekdayw6         5.1556     0.8794   5.863 4.64e-09 ***
weathersitMist   -3.3804     0.5892  -5.737 9.78e-09 ***
weathersitRain  -14.9966    17.9935  -0.833 0.404604    
weathersitSnow  -11.1963     1.0455 -10.709  < 2e-16 ***
temp            158.5548     5.8865  26.935  < 2e-16 ***
hum              50.0185     4.6179  10.831  < 2e-16 ***
windspeed        -5.3359    11.2598  -0.474 0.635584    
poly(hum, 4)1         NA         NA      NA       NA    
poly(hum, 4)2  -218.2046    35.0205  -6.231 4.75e-10 ***
poly(hum, 4)3    82.0113    31.8030   2.579 0.009925 ** 
poly(hum, 4)4   101.5730    32.1174   3.163 0.001567 ** 
I(windspeed^2)  -53.8757    11.2155  -4.804 1.57e-06 ***
temp:hum       -152.3517     7.4260 -20.516  < 2e-16 ***
temp:windspeed   72.3110    10.8474   6.666 2.70e-11 ***
hum:windspeed   -31.4267    10.8382  -2.900 0.003741 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 31.06 on 17320 degrees of freedom
Multiple R-squared:  0.6045,	Adjusted R-squared:  0.6032 
F-statistic: 456.5 on 58 and 17320 DF,  p-value: < 2.2e-16
#plot the residual graphs
par(mfrow = c(2,2))
plot(lm.fit_cas)
lm.fit_reg = lm(registered~season+yr+mnth+hr+holiday+weekday+weathersit+(temp+hum+windspeed)^2+poly(hum,4)+I(windspeed^2),data=bicycle)
par(mfrow = c(2,2))
plot(lm.fit_reg)
summary(lm.fit_reg)
Call:
lm(formula = registered ~ season + yr + mnth + hr + holiday + 
    weekday + weathersit + (temp + hum + windspeed)^2 + poly(hum, 
    4) + I(windspeed^2), data = bicycle)

Residuals:
    Min      1Q  Median      3Q     Max 
-346.74  -48.33   -4.84   45.05  409.22 

Coefficients: (1 not defined because of singularities)
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)    -113.39857   11.12982 -10.189  < 2e-16 ***
seasonSpring    -30.10083    4.80975  -6.258 3.98e-10 ***
seasonSummer     -0.03502    4.15677  -0.008 0.993279    
seasonWinter     34.77392    4.31732   8.055 8.49e-16 ***
yry1             72.97351    1.31547  55.473  < 2e-16 ***
mnthm10           6.04341    5.45246   1.108 0.267713    
mnthm11         -15.25252    5.22860  -2.917 0.003537 ** 
mnthm12          -8.06433    4.15576  -1.941 0.052333 .  
mnthm2            6.51994    3.27730   1.989 0.046670 *  
mnthm3            9.62441    3.70091   2.601 0.009315 ** 
mnthm4            1.84303    5.47706   0.337 0.736498    
mnthm5           16.83562    5.87155   2.867 0.004145 ** 
mnthm6            8.21192    6.02847   1.362 0.173156    
mnthm7           -7.18527    6.78877  -1.058 0.289886    
mnthm8            9.79156    6.60522   1.482 0.138253    
mnthm9           28.55734    5.89293   4.846 1.27e-06 ***
hrh1            -14.63565    4.46182  -3.280 0.001039 ** 
hrh10            74.70238    4.48747  16.647  < 2e-16 ***
hrh11            90.62046    4.52290  20.036  < 2e-16 ***
hrh12           123.78065    4.56703  27.103  < 2e-16 ***
hrh13           116.68791    4.60171  25.358  < 2e-16 ***
hrh14            98.39093    4.63165  21.243  < 2e-16 ***
hrh15           109.12845    4.64449  23.496  < 2e-16 ***
hrh16           171.67122    4.63262  37.057  < 2e-16 ***
hrh17           323.81688    4.60134  70.374  < 2e-16 ***
hrh18           303.09973    4.56343  66.419  < 2e-16 ***
hrh19           205.08482    4.51639  45.409  < 2e-16 ***
hrh2            -22.46907    4.47804  -5.018 5.28e-07 ***
hrh20           136.50566    4.49082  30.397  < 2e-16 ***
hrh21            93.59404    4.46996  20.938  < 2e-16 ***
hrh22            61.43831    4.46110  13.772  < 2e-16 ***
hrh23            28.03207    4.45639   6.290 3.24e-10 ***
hrh3            -31.14258    4.51077  -6.904 5.23e-12 ***
hrh4            -34.01962    4.51643  -7.532 5.23e-14 ***
hrh5            -19.41414    4.48833  -4.325 1.53e-05 ***
hrh6             36.17843    4.47707   8.081 6.85e-16 ***
hrh7            164.46840    4.46653  36.822  < 2e-16 ***
hrh8            296.05311    4.46048  66.372  < 2e-16 ***
hrh9            142.20404    4.46662  31.837  < 2e-16 ***
holidayYes      -46.27055    4.07505 -11.355  < 2e-16 ***
weekdayw1        39.97029    2.48387  16.092  < 2e-16 ***
weekdayw2        45.77866    2.42533  18.875  < 2e-16 ***
weekdayw3        47.59645    2.42234  19.649  < 2e-16 ***
weekdayw4        47.32708    2.42494  19.517  < 2e-16 ***
weekdayw5        42.51280    2.41275  17.620  < 2e-16 ***
weekdayw6        10.15983    2.40446   4.225 2.40e-05 ***
weathersitMist   -6.14889    1.61101  -3.817 0.000136 ***
weathersitRain  -68.10023   49.19839  -1.384 0.166316    
weathersitSnow  -49.68810    2.85866 -17.382  < 2e-16 ***
temp            221.91792   16.09518  13.788  < 2e-16 ***
hum              64.07483   12.62640   5.075 3.92e-07 ***
windspeed        90.88231   30.78709   2.952 0.003162 ** 
poly(hum, 4)1          NA         NA      NA       NA    
poly(hum, 4)2  -771.25147   95.75437  -8.054 8.50e-16 ***
poly(hum, 4)3   207.72063   86.95687   2.389 0.016915 *  
poly(hum, 4)4   172.63743   87.81673   1.966 0.049328 *  
I(windspeed^2) -161.62695   30.66584  -5.271 1.38e-07 ***
temp:hum       -188.31749   20.30450  -9.275  < 2e-16 ***
temp:windspeed  117.97659   29.65931   3.978 6.99e-05 ***
hum:windspeed  -150.07255   29.63427  -5.064 4.14e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 84.92 on 17320 degrees of freedom
Multiple R-squared:  0.6863,	Adjusted R-squared:  0.6852 
F-statistic: 653.2 on 58 and 17320 DF,  p-value: < 2.2e-16

lm.fit_fin = lm(cnt~season+yr+mnth+hr+holiday+weekday+weathersit+(temp+hum+windspeed)^2+poly(hum,4)+I(windspeed^2),data=bicycle)
par(mfrow = c(2,2))
plot(lm.fit_fin)
summary(lm.fit_fin)

Call:
lm(formula = cnt ~ season + yr + mnth + hr + holiday + weekday + 
    weathersit + (temp + hum + windspeed)^2 + poly(hum, 4) + 
    I(windspeed^2), data = bicycle)

Residuals:
    Min      1Q  Median      3Q     Max 
-366.40  -59.41   -5.70   50.19  424.33 

Coefficients: (1 not defined because of singularities)
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)    -150.337     13.187 -11.400  < 2e-16 ***
seasonSpring    -31.664      5.699  -5.556 2.80e-08 ***
seasonSummer      8.979      4.925   1.823  0.06831 .  
seasonWinter     35.209      5.115   6.883 6.06e-12 ***
yry1             84.671      1.559  54.324  < 2e-16 ***
mnthm10          20.205      6.460   3.128  0.00177 ** 
mnthm11          -9.941      6.195  -1.605  0.10857    
mnthm12          -9.179      4.924  -1.864  0.06232 .  
mnthm2            5.320      3.883   1.370  0.17072    
mnthm3           18.893      4.385   4.308 1.65e-05 ***
mnthm4            6.909      6.489   1.065  0.28707    
mnthm5           24.827      6.957   3.569  0.00036 ***
mnthm6            6.220      7.143   0.871  0.38384    
mnthm7           -8.422      8.044  -1.047  0.29511    
mnthm8           14.427      7.826   1.843  0.06528 .  
mnthm9           43.160      6.982   6.181 6.49e-10 ***
hrh1            -17.267      5.287  -3.266  0.00109 ** 
hrh10           105.768      5.317  19.893  < 2e-16 ***
hrh11           131.561      5.359  24.550  < 2e-16 ***
hrh12           170.987      5.411  31.599  < 2e-16 ***
hrh13           165.673      5.452  30.386  < 2e-16 ***
hrh14           149.044      5.488  27.159  < 2e-16 ***
hrh15           158.659      5.503  28.831  < 2e-16 ***
hrh16           220.443      5.489  40.161  < 2e-16 ***
hrh17           374.870      5.452  68.760  < 2e-16 ***
hrh18           342.886      5.407  63.416  < 2e-16 ***
hrh19           235.227      5.351  43.958  < 2e-16 ***
hrh2            -26.190      5.306  -4.936 8.04e-07 ***
hrh20           156.537      5.321  29.419  < 2e-16 ***
hrh21           107.675      5.296  20.331  < 2e-16 ***
hrh22            71.167      5.286  13.464  < 2e-16 ***
hrh23            32.197      5.280   6.098 1.10e-09 ***
hrh3            -36.899      5.345  -6.904 5.23e-12 ***
hrh4            -40.423      5.351  -7.554 4.44e-14 ***
hrh5            -24.349      5.318  -4.579 4.71e-06 ***
hrh6             34.409      5.305   6.487 9.02e-11 ***
hrh7            168.653      5.292  31.869  < 2e-16 ***
hrh8            308.578      5.285  58.388  < 2e-16 ***
hrh9            161.278      5.292  30.474  < 2e-16 ***
holidayYes      -24.095      4.828  -4.990 6.08e-07 ***
weekdayw1         8.434      2.943   2.866  0.00417 ** 
weekdayw2        11.594      2.874   4.035 5.49e-05 ***
weekdayw3        13.115      2.870   4.569 4.92e-06 ***
weekdayw4        13.079      2.873   4.552 5.35e-06 ***
weekdayw5        15.786      2.859   5.522 3.40e-08 ***
weekdayw6        15.315      2.849   5.376 7.72e-08 ***
weathersitMist   -9.529      1.909  -4.992 6.02e-07 ***
weathersitRain  -83.097     58.292  -1.426  0.15403    
weathersitSnow  -60.884      3.387 -17.976  < 2e-16 ***
temp            380.473     19.070  19.951  < 2e-16 ***
hum             114.093     14.960   7.626 2.54e-14 ***
windspeed        85.546     36.478   2.345  0.01903 *  
poly(hum, 4)1        NA         NA      NA       NA    
poly(hum, 4)2  -989.456    113.454  -8.721  < 2e-16 ***
poly(hum, 4)3   289.732    103.030   2.812  0.00493 ** 
poly(hum, 4)4   274.210    104.049   2.635  0.00841 ** 
I(windspeed^2) -215.503     36.334  -5.931 3.07e-09 ***
temp:hum       -340.669     24.058 -14.161  < 2e-16 ***
temp:windspeed  190.288     35.142   5.415 6.21e-08 ***
hum:windspeed  -181.499     35.112  -5.169 2.38e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 100.6 on 17320 degrees of freedom
Multiple R-squared:  0.6933,	Adjusted R-squared:  0.6923 
F-statistic: 675.1 on 58 and 17320 DF,  p-value: < 2.2e-16

mean(casual)
mean(bicycle$casual)
mean(bicycle$cnt)
mean(bicycle$registered)
summary(lm.fit_reg)
mean(bicycle$registered)
summary(lm.fit_reg)
confint(lm.fit_reg)
#Prediction
predict(lm.fit_fin,data.frame(hum=0.80),interval="prediction")
predict(lm.fit_fin,data.frame(season="Spring",hum=0.80),interval="prediction")
predict(lm.fit_fin,data.frame(season="Spring",yr = "0",mnth ="m2", holiday = "No", hr = "h10",weekday = "w5", weathersit="Clear",temp = 0.24, atemp = 0.2879,hum=0.80,windspeed = 0.2),interval="prediction")
predict(lm.fit_cas,data.frame(season="Spring",yr = "y0",mnth ="m2", holiday = "No", hr = "h10",weekday = "w5", weathersit="Clear",temp = 0.24, atemp = 0.2879,hum=0.80,windspeed = 0.2),interval="prediction")
predict(lm.fit_reg,data.frame(season="Spring",yr = "y0",mnth ="m2", holiday = "No", hr = "h10",weekday = "w5", weathersit="Clear",temp = 0.24, atemp = 0.2879,hum=0.80,windspeed = 0.2),interval="prediction")
predict(lm.fit_reg,data.frame(season="Spring",yr = "y0",mnth ="m2", holiday = "No", hr = "h10",weekday = "w5", weathersit="Clear",temp = 0.24, atemp = 0.2879,hum=0.80,windspeed = 0.2),interval="confidence")
#use "car" package to draw interactive scatter graphs 
library(car)
bicycle_simple = subset(bicycle,c(temp,atemp, hum, windspeed))
bicycle_simple = subset(bicycle,select=c(temp,atemp, hum, windspeed))
cor(bicycle_simple)
scatterplotMatrix(bicycle_simple, spread=FALSE, lty.smooth=2)
