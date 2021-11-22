Volume
================

  - [Required packages](#required-packages)
  - [Data import](#data-import)
  - [Histogram outcome variable](#histogram-outcome-variable)
  - [Model buidling](#model-buidling)
      - [Baseline models](#baseline-models)
      - [Model building using nested
        LMM](#model-building-using-nested-lmm)
          - [Backward approach](#backward-approach)

# Required packages

# Data import

``` r
load("../Data/FinalData.RData")
df_final <- df_final %>% 
  dplyr::filter(NeosporaResult != "Uninterpretable") %>%
  dplyr::mutate(
    scaledTime = scale(as.numeric(SampleDay)),
    SiteAnimalIdentifier = paste(Site, AnimalIdentifier, sep = "_")
  )
```

# Histogram outcome variable

![](Volume_files/figure-gfm/pressure-1.png)<!-- --> \# Discriptive
summary of the results

``` r
df_final %>% dplyr::group_by(NeosporaResult) %>%
  summarise(
    count = n(),
    numberofsire = n_distinct(AnimalIdentifier, na.rm = TRUE),
    mean = mean(Volume, na.rm = TRUE),
    var = var(Volume, na.rm = TRUE),
    stdev = sd(Volume,na.rm = TRUE),
    max = max(Volume, na.rm = TRUE),
    min  = min(Volume, na.rm = TRUE)
  )
```

    ## # A tibble: 2 x 8
    ##   NeosporaResult count numberofsire  mean   var stdev   max   min
    ##   <fct>          <int>        <int> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Negative        9537           84  6.60  6.49  2.55  16     0.5
    ## 2 Positive         941            8  6.28 10.7   3.28  14.2   0.6

# Model buidling

## Baseline models

Baseline model is needed to compare more complex models, and check
residual variance

``` r
LMM0.0 <- lmer(
                  Volume ~ 1 + (1| AnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.1 <- lmer(
                  Volume ~ 1 + (1| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.2 <- lmer(
                  Volume ~ 1 + (1| AnimalIdentifier/Site), 
                  data = df_final,
                  REML = TRUE,
                  )

LMM0.3<- lmer(
                  Volume ~ 1 + (1 +  scaledTime| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE,
                  )


anova(LMM0.0,LMM0.1, LMM0.2, LMM0.3)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## LMM0.0: Volume ~ 1 + (1 | AnimalIdentifier)
    ## LMM0.1: Volume ~ 1 + (1 | SiteAnimalIdentifier)
    ## LMM0.2: Volume ~ 1 + (1 | AnimalIdentifier/Site)
    ## LMM0.3: Volume ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance    Chisq Df Pr(>Chisq)    
    ## LMM0.0    3 42748 42770 -21371    42742                           
    ## LMM0.1    3 42690 42712 -21342    42684  57.6624  0               
    ## LMM0.2    4 42685 42714 -21338    42677   7.5863  1   0.005881 ** 
    ## LMM0.3    5 42391 42427 -21191    42381 295.3335  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Model building using nested LMM

``` r
LMM1.0 <- lmer(
              Volume ~ SampleSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

LMM1.1 <- lmer(
              Volume ~ ProductionSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

LMM1.2 <- lmer(
               Volume ~ Site + (1 +  scaledTime| SiteAnimalIdentifier),
               data = df_final
               )
LMM1.3 <- lmer(
            Volume ~ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.4 <- lmer(
            Volume ~ SampleMaxTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.5 <- lmer(
            Volume ~ ProductionMeanTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.6 <- lmer(
              Volume ~ NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

anova(LMM0.3, LMM1.0,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Volume ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.0: Volume ~ SampleSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 42391 42427 -21191    42381                         
    ## LMM1.0    8 42290 42348 -21137    42274 107.07  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.1,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Volume ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.1: Volume ~ ProductionSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 42391 42427 -21191    42381                         
    ## LMM1.1    8 42235 42293 -21109    42219 162.42  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.2,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Volume ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.2: Volume ~ Site + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## LMM0.3    5 42391 42427 -21191    42381                     
    ## LMM1.2    7 42395 42446 -21190    42381 0.4478  2     0.7994

``` r
anova(LMM0.3, LMM1.3,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Volume ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.3: Volume ~ Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 42391 42427 -21191    42381                         
    ## LMM1.3    6 42379 42423 -21184    42367 13.995  1  0.0001833 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.4,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Volume ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.4: Volume ~ SampleMaxTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 42391 42427 -21191    42381                         
    ## LMM1.4    6 42344 42388 -21166    42332 48.671  1  3.027e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.5,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Volume ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.5: Volume ~ ProductionMeanTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 42391 42427 -21191    42381                         
    ## LMM1.5    6 42354 42398 -21171    42342 39.182  1  3.861e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.6,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Volume ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.6: Volume ~ NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)
    ## LMM0.3    5 42391 42427 -21191    42381                    
    ## LMM1.6    6 42393 42437 -21191    42381 2e-04  1     0.9888

### Backward approach

#### Full model with significant fixed effects

``` r
FULL <- lmer( Volume ~ SampleMaxTHI + ProductionMeanTHI + SampleSeason + ProductionSeason + NeosporaResult+Age+ (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
vif.mer (FULL)
```

    ##           SampleMaxTHI      ProductionMeanTHI     SampleSeasonSpring 
    ##               3.301194               5.525246               4.107432 
    ##     SampleSeasonSummer     SampleSeasonWinter ProductionSeasonSummer 
    ##               2.700385               3.850076               2.968018 
    ## ProductionSeasonWinter   ProductionSeasonFall NeosporaResultPositive 
    ##               3.161980               3.917168               1.000312 
    ##                    Age 
    ##               1.769328

Multicollinearity between season and THI.

``` r
FULL1 <- lmer(Volume ~ ProductionMeanTHI + Age + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier), data = df_final)

summary (FULL1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Volume ~ ProductionMeanTHI + Age + NeosporaResult + (1 + scaledTime |  
    ##     SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 42363.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.0748 -0.5758 -0.0229  0.5559  4.6013 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.1591   1.7774       
    ##                       scaledTime  0.4104   0.6406   0.03
    ##  Residual                         3.1348   1.7705       
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error t value
    ## (Intercept)            5.3729422  0.3527690  15.231
    ## ProductionMeanTHI      0.0139320  0.0025464   5.471
    ## Age                    0.0004299  0.0001983   2.168
    ## NeosporaResultPositive 0.0383682  0.6625483   0.058
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI Age   
    ## PrdctnMnTHI -0.182              
    ## Age         -0.757 -0.263       
    ## NsprRsltPst -0.156 -0.001  0.014

``` r
drop1(FULL1, test = "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Volume ~ ProductionMeanTHI + Age + NeosporaResult + (1 + scaledTime | 
    ##     SiteAnimalIdentifier)
    ##                   npar   AIC     LRT   Pr(Chi)    
    ## <none>                 42353                      
    ## ProductionMeanTHI    1 42381 29.7265 4.975e-08 ***
    ## Age                  1 42356  4.5434   0.03305 *  
    ## NeosporaResult       1 42351  0.0035   0.95253    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
FULL2 <- lmer(Volume ~ SampleMaxTHI + Age + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
summary(FULL2)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Volume ~ SampleMaxTHI + Age + NeosporaResult + (1 + scaledTime |  
    ##     SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 42348.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.0000 -0.5810 -0.0219  0.5558  4.6130 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.0972   1.7599       
    ##                       scaledTime  0.3999   0.6323   0.03
    ##  Residual                         3.1313   1.7696       
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error t value
    ## (Intercept)            5.1449667  0.3545307  14.512
    ## SampleMaxTHI           0.0126728  0.0018843   6.726
    ## Age                    0.0006247  0.0001906   3.277
    ## NeosporaResultPositive 0.0310829  0.6561122   0.047
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmMTHI Age   
    ## SampleMxTHI -0.242              
    ## Age         -0.804 -0.073       
    ## NsprRsltPst -0.153 -0.003  0.014

``` r
drop1(FULL2, test= "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Volume ~ SampleMaxTHI + Age + NeosporaResult + (1 + scaledTime | 
    ##     SiteAnimalIdentifier)
    ##                npar   AIC    LRT   Pr(Chi)    
    ## <none>              42338                     
    ## SampleMaxTHI      1 42381 45.088 1.884e-11 ***
    ## Age               1 42346 10.416  0.001249 ** 
    ## NeosporaResult    1 42336  0.002  0.961113    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
FULL3 <- lmer(Volume ~ ProductionSeason + Age + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
summary(FULL3)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Volume ~ ProductionSeason + Age + NeosporaResult + (1 + scaledTime |  
    ##     SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 42229.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.0425 -0.5852 -0.0190  0.5650  4.7039 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.0962   1.7596       
    ##                       scaledTime  0.3916   0.6257   0.03
    ##  Residual                         3.0959   1.7595       
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)             5.5854237  0.4218148  13.241
    ## ProductionSeasonSummer -0.4221243  0.0573064  -7.366
    ## ProductionSeasonWinter -0.5186214  0.0555187  -9.341
    ## ProductionSeasonFall   -0.6365901  0.0738929  -8.615
    ## Age                     0.0010615  0.0002516   4.219
    ## NeosporaResultPositive  0.0003052  0.6559403   0.000
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrdcSS PrdcSW PrdcSF Age   
    ## PrdctnSsnSm  0.263                            
    ## PrdctnSsnWn -0.383  0.132                     
    ## PrdctnSsnFl  0.448  0.651 -0.033              
    ## Age         -0.899 -0.354  0.371 -0.551       
    ## NsprRsltPst -0.124  0.007 -0.002  0.009  0.005

``` r
drop1(FULL3, test ="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Volume ~ ProductionSeason + Age + NeosporaResult + (1 + scaledTime | 
    ##     SiteAnimalIdentifier)
    ##                  npar   AIC     LRT   Pr(Chi)    
    ## <none>                42221                      
    ## ProductionSeason    3 42381 165.804 < 2.2e-16 ***
    ## Age                 1 42237  17.384 3.054e-05 ***
    ## NeosporaResult      1 42219   0.000    0.9989    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
FULL4 <- lmer(Volume ~ SampleSeason + Age + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
summary(FULL4)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Volume ~ SampleSeason + Age + NeosporaResult + (1 + scaledTime |  
    ##     SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 42280.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.0224 -0.5799 -0.0172  0.5617  4.7427 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.0669   1.7512       
    ##                       scaledTime  0.3885   0.6233   0.03
    ##  Residual                         3.1118   1.7640       
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)             5.2159798  0.3718911  14.026
    ## SampleSeasonSpring      0.4068417  0.0624829   6.511
    ## SampleSeasonSummer      0.2650244  0.0534232   4.961
    ## SampleSeasonWinter     -0.0967671  0.0608184  -1.591
    ## Age                     0.0009516  0.0002016   4.721
    ## NeosporaResultPositive  0.0156239  0.6529514   0.024
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmplSsnSp SmplSsnSm SmplSW Age   
    ## SmplSsnSprn -0.368                                  
    ## SmplSsnSmmr -0.239  0.602                           
    ## SmplSsnWntr -0.337  0.654     0.564                 
    ## Age         -0.868  0.327     0.183     0.291       
    ## NsprRsltPst -0.144 -0.005    -0.004    -0.003  0.011

``` r
drop1(FULL4, test ="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Volume ~ SampleSeason + Age + NeosporaResult + (1 + scaledTime | 
    ##     SiteAnimalIdentifier)
    ##                npar   AIC     LRT   Pr(Chi)    
    ## <none>              42272                      
    ## SampleSeason      3 42381 114.846 < 2.2e-16 ***
    ## Age               1 42292  21.780 3.058e-06 ***
    ## NeosporaResult    1 42270   0.001    0.9802    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(FULL1, FULL2, FULL3, FULL4)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## FULL1: Volume ~ ProductionMeanTHI + Age + NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL2: Volume ~ SampleMaxTHI + Age + NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL3: Volume ~ ProductionSeason + Age + NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL4: Volume ~ SampleSeason + Age + NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##       npar   AIC   BIC logLik deviance   Chisq Df Pr(>Chisq)    
    ## FULL1    8 42353 42411 -21169    42337                          
    ## FULL2    8 42338 42396 -21161    42322  15.361  0               
    ## FULL3   10 42221 42294 -21101    42201 120.716  2  < 2.2e-16 ***
    ## FULL4   10 42272 42345 -21126    42252   0.000  0               
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Final Model

``` r
FINAL <- lmer(Volume ~ ProductionMeanTHI + Age + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
summary (FINAL)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Volume ~ ProductionMeanTHI + Age + NeosporaResult + (1 + scaledTime |  
    ##     SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 42363.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.0748 -0.5758 -0.0229  0.5559  4.6013 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.1591   1.7774       
    ##                       scaledTime  0.4104   0.6406   0.03
    ##  Residual                         3.1348   1.7705       
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error t value
    ## (Intercept)            5.3729422  0.3527690  15.231
    ## ProductionMeanTHI      0.0139320  0.0025464   5.471
    ## Age                    0.0004299  0.0001983   2.168
    ## NeosporaResultPositive 0.0383682  0.6625483   0.058
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI Age   
    ## PrdctnMnTHI -0.182              
    ## Age         -0.757 -0.263       
    ## NsprRsltPst -0.156 -0.001  0.014

``` r
drop1(FINAL, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Volume ~ ProductionMeanTHI + Age + NeosporaResult + (1 + scaledTime | 
    ##     SiteAnimalIdentifier)
    ##                   npar   AIC     LRT   Pr(Chi)    
    ## <none>                 42353                      
    ## ProductionMeanTHI    1 42381 29.7265 4.975e-08 ***
    ## Age                  1 42356  4.5434   0.03305 *  
    ## NeosporaResult       1 42351  0.0035   0.95253    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif.mer(FINAL)
```

    ##      ProductionMeanTHI                    Age NeosporaResultPositive 
    ##               1.074225               1.074419               1.000188

#### Final Model with interaction

Interaction Age\*ProductionMeanTHI

``` r
FINAL1 <- lmer(Volume ~ Age*ProductionMeanTHI + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final)
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(FINAL1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Volume ~ Age * ProductionMeanTHI + NeosporaResult + (1 + scaledTime |  
    ##     SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 42381.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1013 -0.5785 -0.0248  0.5535  4.6110 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.1803   1.783        
    ##                       scaledTime  0.4032   0.635    0.03
    ##  Residual                         3.1337   1.770        
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)             4.701e+00  4.609e-01  10.200
    ## Age                     8.588e-04  2.740e-04   3.135
    ## ProductionMeanTHI       2.704e-02  6.281e-03   4.306
    ## NeosporaResultPositive  3.071e-02  6.647e-01   0.046
    ## Age:ProductionMeanTHI  -8.303e-06  3.635e-06  -2.284
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Age    PrMTHI NsprRP
    ## Age         -0.863                     
    ## PrdctnMnTHI -0.644  0.553              
    ## NsprRsltPst -0.116  0.006 -0.005       
    ## Ag:PrdcMTHI  0.643 -0.689 -0.914  0.005
    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
drop1(FINAL1, test= "Chisq")
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

    ## Single term deletions
    ## 
    ## Model:
    ## Volume ~ Age * ProductionMeanTHI + NeosporaResult + (1 + scaledTime | 
    ##     SiteAnimalIdentifier)
    ##                       npar   AIC    LRT Pr(Chi)  
    ## <none>                     42350                 
    ## NeosporaResult           1 42348 0.0023  0.9619  
    ## Age:ProductionMeanTHI    1 42353 5.1914  0.0227 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
plot(FINAL1)
```

![](Volume_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
res=resid(FINAL1)
plot_model(FINAL1, type = "pred", terms = c("Age","ProductionMeanTHI"))
```

![](Volume_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
hist (res)
```

![](Volume_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
qqPlot (res)
```

![](Volume_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

    ## [1] 1264 1011

``` r
Anova(FINAL1)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Volume
    ##                         Chisq Df Pr(>Chisq)    
    ## Age                    4.6392  1    0.03125 *  
    ## ProductionMeanTHI     29.9463  1  4.442e-08 ***
    ## NeosporaResult         0.0021  1    0.96315    
    ## Age:ProductionMeanTHI  5.2170  1    0.02237 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
