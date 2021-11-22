ProgressiveMotilityThawed
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
  dplyr::filter(!is.na(MotilityPercentageThawedProg) & !is.na(MotilityPercentageThawedTotal) & !is.na(MorfThawed)) %>%
  dplyr::mutate(
    scaledTime = scale(as.numeric(SampleDay)),
    SiteAnimalIdentifier = paste(Site, AnimalIdentifier, sep = "_")
  )
```

# Histogram outcome variable

![](MotilityThawedProg_files/figure-gfm/pressure-1.png)<!-- --> \#
Discriptive summary of the results

``` r
df_final %>% dplyr::group_by(NeosporaResult) %>%
  summarise(
    count = n(),
    numberofsire = n_distinct(AnimalIdentifier, na.rm = TRUE),
    mean = mean(MotilityPercentageThawedProg, na.rm = TRUE),
    var = var(MotilityPercentageThawedProg, na.rm = TRUE),
    stdev = sd(MotilityPercentageThawedProg,na.rm = TRUE),
    max = max(MotilityPercentageThawedProg, na.rm = TRUE),
   min = min(MotilityPercentageThawedProg,na.rm = TRUE)
  )
```

    ## # A tibble: 2 x 8
    ##   NeosporaResult count numberofsire  mean   var stdev   max   min
    ##   <fct>          <int>        <int> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Negative        7018           84  26.3  66.8  8.17    57     3
    ## 2 Positive         667            8  27.1  77.7  8.81    55     2

# Model buidling

## Baseline models

Baseline model is needed to compare more complex models, and check
residual variance

``` r
LMM0.0 <- lmer(
                  MotilityPercentageThawedProg ~ 1 + (1| AnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.1 <- lmer(
                  MotilityPercentageThawedProg ~ 1 + (1| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.2 <- lmer(
                  MotilityPercentageThawedProg ~ 1 + (1| AnimalIdentifier/Site), 
                  data = df_final,
                  REML = TRUE,
                  )

LMM0.3<- lmer(
                  MotilityPercentageThawedProg ~ 1 + (1 +  scaledTime| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE,
                  )


anova(LMM0.0,LMM0.1, LMM0.2, LMM0.3)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## LMM0.0: MotilityPercentageThawedProg ~ 1 + (1 | AnimalIdentifier)
    ## LMM0.1: MotilityPercentageThawedProg ~ 1 + (1 | SiteAnimalIdentifier)
    ## LMM0.2: MotilityPercentageThawedProg ~ 1 + (1 | AnimalIdentifier/Site)
    ## LMM0.3: MotilityPercentageThawedProg ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance    Chisq Df Pr(>Chisq)    
    ## LMM0.0    3 51574 51595 -25784    51568                           
    ## LMM0.1    3 51548 51569 -25771    51542  25.7172  0               
    ## LMM0.2    4 51545 51573 -25768    51537   5.6056  1     0.0179 *  
    ## LMM0.3    5 51280 51315 -25635    51270 266.5958  1     <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Model building using nested LMM

``` r
LMM1.0 <- lmer(
              MotilityPercentageThawedProg ~ SampleSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.1 <- lmer(
              MotilityPercentageThawedProg ~ ProductionSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.2 <- lmer(
              MotilityPercentageThawedProg ~ Site + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.3 <- lmer(
            MotilityPercentageThawedProg ~ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.4 <- lmer(
            MotilityPercentageThawedProg ~ SampleMaxTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.5 <- lmer(
            MotilityPercentageThawedProg ~ ProductionMeanTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.6 <- lmer(
            MotilityPercentageThawedProg ~ NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
anova(LMM0.3, LMM1.0, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedProg ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.0: MotilityPercentageThawedProg ~ SampleSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 51280 51315 -25635    51270                         
    ## LMM1.0    8 51259 51314 -25621    51243 27.578  3  4.454e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.1, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedProg ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.1: MotilityPercentageThawedProg ~ ProductionSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 51280 51315 -25635    51270                         
    ## LMM1.1    8 51226 51282 -25605    51210 60.157  3  5.442e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.2, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedProg ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.2: MotilityPercentageThawedProg ~ Site + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## LMM0.3    5 51280 51315 -25635    51270                     
    ## LMM1.2    7 51283 51332 -25635    51269 1.1634  2     0.5589

``` r
anova(LMM0.3, LMM1.3, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedProg ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.3: MotilityPercentageThawedProg ~ Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)   
    ## LMM0.3    5 51280 51315 -25635    51270                        
    ## LMM1.3    6 51275 51317 -25632    51263 6.7236  1   0.009515 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.4, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedProg ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.4: MotilityPercentageThawedProg ~ SampleMaxTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 51280 51315 -25635    51270                         
    ## LMM1.4    6 51271 51313 -25630    51259 11.016  1  0.0009032 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.5, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedProg ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.5: MotilityPercentageThawedProg ~ ProductionMeanTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 51280 51315 -25635    51270                         
    ## LMM1.5    6 51221 51262 -25604    51209 61.432  1  4.582e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.6, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedProg ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.6: MotilityPercentageThawedProg ~ NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## LMM0.3    5 51280 51315 -25635    51270                     
    ## LMM1.6    6 51282 51324 -25635    51270 0.0055  1     0.9408

### Backward approach

Full model with significant fixed effects

``` r
FULL <- lmer(
            MotilityPercentageThawedProg ~ SampleMaxTHI + ProductionMeanTHI + SampleSeason + ProductionSeason + NeosporaResult+Age+ (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
vif.mer (FULL)
```

    ##           SampleMaxTHI      ProductionMeanTHI     SampleSeasonSpring 
    ##               3.326452               5.476697               4.240011 
    ##     SampleSeasonSummer     SampleSeasonWinter ProductionSeasonSummer 
    ##               2.772764               3.960634               2.733805 
    ## ProductionSeasonWinter   ProductionSeasonFall NeosporaResultPositive 
    ##               3.128815               3.422002               1.000358 
    ##                    Age 
    ##               1.318903

Multicollinearity between season and THI.

``` r
FULL1 <- lmer(
            MotilityPercentageThawedProg ~  ProductionMeanTHI + Age+ NeosporaResult +  
              (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
drop1(FULL1,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedProg ~ ProductionMeanTHI + Age + NeosporaResult + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                   npar   AIC    LRT   Pr(Chi)    
    ## <none>                 51224                     
    ## ProductionMeanTHI    1 51277 55.645 8.682e-14 ***
    ## Age                  1 51223  0.950    0.3297    
    ## NeosporaResult       1 51222  0.005    0.9459    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(FULL1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## MotilityPercentageThawedProg ~ ProductionMeanTHI + Age + NeosporaResult +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 51224.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5589 -0.6343  0.0097  0.6577  3.6069 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 22.562   4.750         
    ##                       scaledTime   8.471   2.911    -0.12
    ##  Residual                         42.701   6.535         
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            31.5839638  1.1772497  26.829
    ## ProductionMeanTHI      -0.0827944  0.0110614  -7.485
    ## Age                    -0.0006134  0.0006387  -0.960
    ## NeosporaResultPositive -0.1230188  1.7935273  -0.069
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI Age   
    ## PrdctnMnTHI -0.361              
    ## Age         -0.735 -0.209       
    ## NsprRsltPst -0.128 -0.004  0.012

``` r
FULL2 <- lmer(
            MotilityPercentageThawedProg ~  SampleMaxTHI + Age+ NeosporaResult +  
              (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
drop1(FULL2,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedProg ~ SampleMaxTHI + Age + NeosporaResult + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                npar   AIC    LRT  Pr(Chi)   
    ## <none>              51270                   
    ## SampleMaxTHI      1 51277 9.9659 0.001595 **
    ## Age               1 51273 5.6867 0.017094 * 
    ## NeosporaResult    1 51268 0.0101 0.919942   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(FULL2)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MotilityPercentageThawedProg ~ SampleMaxTHI + Age + NeosporaResult +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 51270.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5422 -0.6412  0.0089  0.6513  3.5689 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 21.765   4.665         
    ##                       scaledTime   8.649   2.941    -0.11
    ##  Residual                         42.971   6.555         
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            29.6658345  1.1557147  25.669
    ## SampleMaxTHI           -0.0257651  0.0081611  -3.157
    ## Age                    -0.0014787  0.0006201  -2.385
    ## NeosporaResultPositive -0.1793830  1.7669321  -0.102
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmMTHI Age   
    ## SampleMxTHI -0.345              
    ## Age         -0.810 -0.067       
    ## NsprRsltPst -0.128 -0.007  0.012

``` r
FULL3 <- lmer(
            MotilityPercentageThawedProg ~  ProductionSeason + Age+ NeosporaResult +  
              (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
drop1(FULL3,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedProg ~ ProductionSeason + Age + NeosporaResult + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                  npar   AIC    LRT   Pr(Chi)    
    ## <none>                51224                     
    ## ProductionSeason    3 51277 59.416 7.836e-13 ***
    ## Age                 1 51228  5.991   0.01438 *  
    ## NeosporaResult      1 51222  0.018   0.89346    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(FULL3)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## MotilityPercentageThawedProg ~ ProductionSeason + Age + NeosporaResult +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 51216.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5630 -0.6412  0.0070  0.6507  3.4953 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 21.89    4.679         
    ##                       scaledTime   8.53    2.921    -0.09
    ##  Residual                         42.70    6.535         
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            28.9161063  1.1960632  24.176
    ## ProductionSeasonSummer -1.3621124  0.2444186  -5.573
    ## ProductionSeasonWinter -0.2159480  0.2275350  -0.949
    ## ProductionSeasonFall    0.2601428  0.2952850   0.881
    ## Age                    -0.0017421  0.0007109  -2.451
    ## NeosporaResultPositive -0.2389549  1.7749899  -0.135
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrdcSS PrdcSW PrdcSF Age   
    ## PrdctnSsnSm  0.120                            
    ## PrdctnSsnWn -0.306  0.209                     
    ## PrdctnSsnFl  0.260  0.603  0.076              
    ## Age         -0.902 -0.235  0.260 -0.388       
    ## NsprRsltPst -0.123  0.007  0.000  0.011  0.006

``` r
FULL4 <- lmer(
            MotilityPercentageThawedProg ~  SampleSeason + Age+ NeosporaResult +  
              (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
drop1(FULL4,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedProg ~ SampleSeason + Age + NeosporaResult + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                npar   AIC     LRT   Pr(Chi)    
    ## <none>              51259                      
    ## SampleSeason      3 51277 24.4882 1.975e-05 ***
    ## Age               1 51261  3.6460    0.0562 .  
    ## NeosporaResult    1 51257  0.0104    0.9188    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(FULL4)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MotilityPercentageThawedProg ~ SampleSeason + Age + NeosporaResult +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 51252.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5254 -0.6383  0.0062  0.6474  3.6409 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 21.993   4.690         
    ##                       scaledTime   8.613   2.935    -0.12
    ##  Residual                         42.896   6.550         
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            27.7607316  1.1617936  23.895
    ## SampleSeasonSpring      0.3479474  0.2679633   1.298
    ## SampleSeasonSummer     -0.6319072  0.2379091  -2.656
    ## SampleSeasonWinter      0.3567719  0.2625353   1.359
    ## Age                    -0.0012212  0.0006407  -1.906
    ## NeosporaResultPositive -0.1823848  1.7731579  -0.103
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmplSsnSp SmplSsnSm SmplSW Age   
    ## SmplSsnSprn -0.326                                  
    ## SmplSsnSmmr -0.220  0.610                           
    ## SmplSsnWntr -0.312  0.669     0.576                 
    ## Age         -0.893  0.232     0.121     0.217       
    ## NsprRsltPst -0.129 -0.004    -0.003     0.000  0.010

``` r
anova(FULL1, FULL2, FULL3, FULL4)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## FULL1: MotilityPercentageThawedProg ~ ProductionMeanTHI + Age + NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL2: MotilityPercentageThawedProg ~ SampleMaxTHI + Age + NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL3: MotilityPercentageThawedProg ~ ProductionSeason + Age + NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL4: MotilityPercentageThawedProg ~ SampleSeason + Age + NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##       npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)    
    ## FULL1    8 51224 51279 -25604    51208                        
    ## FULL2    8 51270 51325 -25627    51254  0.00  0               
    ## FULL3   10 51224 51294 -25602    51204 49.45  2  1.829e-11 ***
    ## FULL4   10 51259 51328 -25620    51239  0.00  0               
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Final model

``` r
FINAL <- lmer(
            MotilityPercentageThawedProg ~  ProductionMeanTHI + NeosporaResult +  
              (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
summary(FINAL)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MotilityPercentageThawedProg ~ ProductionMeanTHI + NeosporaResult +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 51212.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5659 -0.6344  0.0071  0.6558  3.6073 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 22.924   4.788         
    ##                       scaledTime   8.456   2.908    -0.13
    ##  Residual                         42.694   6.534         
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error t value
    ## (Intercept)            30.75418    0.80115  38.387
    ## ProductionMeanTHI      -0.08505    0.01082  -7.863
    ## NeosporaResultPositive -0.07923    1.80459  -0.044
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI
    ## PrdctnMnTHI -0.774       
    ## NsprRsltPst -0.176 -0.002

``` r
drop1(FINAL, test= "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedProg ~ ProductionMeanTHI + NeosporaResult + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                   npar   AIC    LRT  Pr(Chi)    
    ## <none>                 51223                    
    ## ProductionMeanTHI    1 51282 61.429 4.59e-15 ***
    ## NeosporaResult       1 51221  0.002   0.9656    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
plot(FINAL)
```

![](MotilityThawedProg_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
res=resid(FINAL)
hist (res)
```

![](MotilityThawedProg_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
qqPlot (res)
```

![](MotilityThawedProg_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

    ## [1] 4731 2710

``` r
Anova(FINAL)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: MotilityPercentageThawedProg
    ##                     Chisq Df Pr(>Chisq)    
    ## ProductionMeanTHI 61.8197  1  3.764e-15 ***
    ## NeosporaResult     0.0019  1      0.965    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
