TotalMotilityThawed
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

![](MotilityThawedTotal_files/figure-gfm/pressure-1.png)<!-- --> \#
Discriptive summary of the results

``` r
df_final %>% dplyr::group_by(NeosporaResult) %>%
  summarise(
    count = n(),
    numberofsire = n_distinct(AnimalIdentifier, na.rm = TRUE),
    mean = mean(MotilityPercentageThawedTotal, na.rm = TRUE),
    var = var(MotilityPercentageThawedTotal, na.rm = TRUE),
    stdev = sd(MotilityPercentageThawedTotal,na.rm = TRUE),
    max = max(MotilityPercentageThawedTotal, na.rm = TRUE),
    min = min(MotilityPercentageThawedTotal,na.rm = TRUE),
  )
```

    ## # A tibble: 2 x 8
    ##   NeosporaResult count numberofsire  mean   var stdev   max   min
    ##   <fct>          <int>        <int> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Negative        7018           84  45.1  135.  11.6    79     5
    ## 2 Positive         667            8  44.5  156.  12.5    82     6

# Model buidling

## Baseline models

Baseline model is needed to compare more complex models, and check
residual variance

``` r
LMM0.0 <- lmer(
                  MotilityPercentageThawedTotal ~ 1 + (1| AnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.1 <- lmer(
                  MotilityPercentageThawedTotal ~ 1 + (1| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.2 <- lmer(
                  MotilityPercentageThawedTotal ~ 1 + (1| AnimalIdentifier/Site), 
                  data = df_final,
                  REML = TRUE,
                  )

LMM0.3<- lmer(
                  MotilityPercentageThawedTotal ~ 1 + (1 +  scaledTime| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE,
                  )


anova(LMM0.0,LMM0.1, LMM0.2, LMM0.3)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## LMM0.0: MotilityPercentageThawedTotal ~ 1 + (1 | AnimalIdentifier)
    ## LMM0.1: MotilityPercentageThawedTotal ~ 1 + (1 | SiteAnimalIdentifier)
    ## LMM0.2: MotilityPercentageThawedTotal ~ 1 + (1 | AnimalIdentifier/Site)
    ## LMM0.3: MotilityPercentageThawedTotal ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance   Chisq Df Pr(>Chisq)    
    ## LMM0.0    3 57080 57101 -28537    57074                          
    ## LMM0.1    3 57063 57084 -28529    57057  17.067  0               
    ## LMM0.2    4 57055 57083 -28524    57047  10.110  1   0.001475 ** 
    ## LMM0.3    5 56897 56932 -28444    56887 159.610  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Model building using nested LMM

``` r
LMM1.0 <- lmer(
              MotilityPercentageThawedTotal ~ SampleSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

LMM1.1 <- lmer(
              MotilityPercentageThawedTotal ~ ProductionSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

LMM1.2 <- lmer(
              MotilityPercentageThawedTotal ~ Site + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.3 <- lmer(
            MotilityPercentageThawedTotal ~ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.4 <- lmer(
            MotilityPercentageThawedTotal ~ SampleMaxTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.5 <- lmer(
            MotilityPercentageThawedTotal ~ ProductionMeanTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.6 <- lmer(
              MotilityPercentageThawedTotal ~ NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

anova(LMM0.3, LMM1.0, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedTotal ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.0: MotilityPercentageThawedTotal ~ SampleSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 56897 56932 -28444    56887                         
    ## LMM1.0    8 56751 56807 -28368    56735 152.09  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.1, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedTotal ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.1: MotilityPercentageThawedTotal ~ ProductionSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 56897 56932 -28444    56887                         
    ## LMM1.1    8 56634 56690 -28309    56618 269.23  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.2, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedTotal ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.2: MotilityPercentageThawedTotal ~ Site + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## LMM0.3    5 56897 56932 -28444    56887                     
    ## LMM1.2    7 56897 56946 -28442    56883 4.3839  2     0.1117

``` r
anova(LMM0.3, LMM1.3, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedTotal ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.3: MotilityPercentageThawedTotal ~ Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 56897 56932 -28444    56887                         
    ## LMM1.3    6 56883 56925 -28436    56871 16.174  1  5.779e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.4, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedTotal ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.4: MotilityPercentageThawedTotal ~ SampleMaxTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 56897 56932 -28444    56887                        
    ## LMM1.4    6 56712 56754 -28350    56700 187.4  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.5, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedTotal ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.5: MotilityPercentageThawedTotal ~ ProductionMeanTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 56897 56932 -28444    56887                         
    ## LMM1.5    6 56635 56677 -28312    56623 264.03  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.6, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageThawedTotal ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.6: MotilityPercentageThawedTotal ~ NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## LMM0.3    5 56897 56932 -28444    56887                     
    ## LMM1.6    6 56898 56939 -28443    56886 1.6976  1     0.1926

### Backward approach

Full model with significant fixed effects

``` r
FULL <- lmer(
            MotilityPercentageThawedTotal ~ SampleMaxTHI + ProductionMeanTHI + SampleSeason + ProductionSeason + NeosporaResult+Age+ (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
vif.mer (FULL)
```

    ##           SampleMaxTHI      ProductionMeanTHI     SampleSeasonSpring 
    ##               3.348460               5.604572               4.276576 
    ##     SampleSeasonSummer     SampleSeasonWinter ProductionSeasonSummer 
    ##               2.780917               3.993920               2.725780 
    ## ProductionSeasonWinter   ProductionSeasonFall NeosporaResultPositive 
    ##               3.221739               3.447104               1.000501 
    ##                    Age 
    ##               1.320889

Multicollinearity between season and THI.

``` r
FULL1 <- lmer(
            MotilityPercentageThawedTotal ~  ProductionMeanTHI + Age + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
drop1(FULL1,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedTotal ~ ProductionMeanTHI + Age + NeosporaResult + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                   npar   AIC     LRT Pr(Chi)    
    ## <none>                 56638                    
    ## ProductionMeanTHI    1 56883 247.510  <2e-16 ***
    ## Age                  1 56636   0.228  0.6331    
    ## NeosporaResult       1 56637   1.501  0.2205    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(FULL1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## MotilityPercentageThawedTotal ~ ProductionMeanTHI + Age + NeosporaResult +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 56635.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5804 -0.6245  0.0551  0.6803  3.8330 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 46.48    6.818         
    ##                       scaledTime  13.05    3.613    -0.03
    ##  Residual                         86.59    9.305         
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            59.5727179  1.6627987  35.827
    ## ProductionMeanTHI      -0.2489744  0.0156609 -15.898
    ## Age                    -0.0004077  0.0009134  -0.446
    ## NeosporaResultPositive -3.1861864  2.5900008  -1.230
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI Age   
    ## PrdctnMnTHI -0.348              
    ## Age         -0.734 -0.219       
    ## NsprRsltPst -0.130 -0.004  0.013

``` r
FULL2 <- lmer(
            MotilityPercentageThawedTotal ~ SampleMaxTHI +  NeosporaResult + Age+ (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

drop1(FULL2,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedTotal ~ SampleMaxTHI + NeosporaResult + 
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##                npar   AIC     LRT   Pr(Chi)    
    ## <none>              56704                      
    ## SampleMaxTHI      1 56883 180.811 < 2.2e-16 ***
    ## NeosporaResult    1 56704   1.740  0.187140    
    ## Age               1 56713  10.136  0.001454 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL2)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MotilityPercentageThawedTotal ~ SampleMaxTHI + NeosporaResult +  
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 56703.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6385 -0.6162  0.0523  0.6796  3.8389 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 41.53    6.444        
    ##                       scaledTime  12.08    3.475    0.02
    ##  Residual                         87.56    9.357        
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            57.9839746  1.5924623  36.412
    ## SampleMaxTHI           -0.1571448  0.0116123 -13.533
    ## NeosporaResultPositive -3.2499064  2.4630468  -1.319
    ## Age                    -0.0027523  0.0008608  -3.198
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmMTHI NsprRP
    ## SampleMxTHI -0.352              
    ## NsprRsltPst -0.129 -0.007       
    ## Age         -0.804 -0.070  0.014

``` r
FULL3 <- lmer(
            MotilityPercentageThawedTotal ~ ProductionSeason +  NeosporaResult + Age+ (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

drop1(FULL3,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedTotal ~ ProductionSeason + NeosporaResult + 
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##                  npar   AIC     LRT   Pr(Chi)    
    ## <none>                56626                      
    ## ProductionSeason    3 56883 263.378 < 2.2e-16 ***
    ## NeosporaResult      1 56626   1.863  0.172261    
    ## Age                 1 56635  10.771  0.001031 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL3)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MotilityPercentageThawedTotal ~ ProductionSeason + NeosporaResult +  
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 56614.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5626 -0.6232  0.0483  0.6813  3.8046 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 41.71    6.458        
    ##                       scaledTime  11.63    3.411    0.05
    ##  Residual                         86.65    9.309        
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            48.3854571  1.6512152  29.303
    ## ProductionSeasonSummer -0.9298341  0.3440365  -2.703
    ## ProductionSeasonWinter  3.0833793  0.3208344   9.611
    ## ProductionSeasonFall    3.2492829  0.4108838   7.908
    ## NeosporaResultPositive -3.3628056  2.4676978  -1.363
    ## Age                    -0.0032629  0.0009854  -3.311
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrdcSS PrdcSW PrdcSF NsprRP
    ## PrdctnSsnSm  0.122                            
    ## PrdctnSsnWn -0.313  0.232                     
    ## PrdctnSsnFl  0.267  0.593  0.105              
    ## NsprRsltPst -0.123  0.009 -0.001  0.014       
    ## Age         -0.902 -0.231  0.255 -0.386  0.006

``` r
FULL4 <- lmer(
            MotilityPercentageThawedTotal ~ SampleSeason +  NeosporaResult + Age+ (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

drop1(FULL4,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedTotal ~ SampleSeason + NeosporaResult + 
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##                npar   AIC     LRT   Pr(Chi)    
    ## <none>              56742                      
    ## SampleSeason      3 56883 147.155 < 2.2e-16 ***
    ## NeosporaResult    1 56742   1.838 0.1752423    
    ## Age               1 56752  11.702 0.0006242 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL4)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MotilityPercentageThawedTotal ~ SampleSeason + NeosporaResult +  
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 56731.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5139 -0.6156  0.0572  0.6815  3.8080 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 41.22    6.420        
    ##                       scaledTime  12.25    3.500    0.02
    ##  Residual                         87.97    9.379        
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            49.9632163  1.6000194  31.227
    ## SampleSeasonSpring     -0.8872619  0.3795762  -2.338
    ## SampleSeasonSummer     -2.4347836  0.3391219  -7.180
    ## SampleSeasonWinter      1.3804337  0.3722981   3.708
    ## NeosporaResultPositive -3.3258274  2.4547132  -1.355
    ## Age                    -0.0030521  0.0008875  -3.439
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmplSsnSp SmplSsnSm SmplSW NsprRP
    ## SmplSsnSprn -0.341                                  
    ## SmplSsnSmmr -0.230  0.607                           
    ## SmplSsnWntr -0.327  0.663     0.574                 
    ## NsprRsltPst -0.129 -0.007    -0.005    -0.003       
    ## Age         -0.891  0.238     0.124     0.223  0.012

``` r
anova (FULL1, FULL2, FULL3, FULL4)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## FULL1: MotilityPercentageThawedTotal ~ ProductionMeanTHI + Age + NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL2: MotilityPercentageThawedTotal ~ SampleMaxTHI + NeosporaResult + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL3: MotilityPercentageThawedTotal ~ ProductionSeason + NeosporaResult + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL4: MotilityPercentageThawedTotal ~ SampleSeason + NeosporaResult + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## FULL1    8 56638 56693 -28311    56622                         
    ## FULL2    8 56704 56760 -28344    56688  0.000  0               
    ## FULL3   10 56626 56695 -28303    56606 82.567  2  < 2.2e-16 ***
    ## FULL4   10 56742 56812 -28361    56722  0.000  0               
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Final model

``` r
FINAL <- lmer(
            MotilityPercentageThawedTotal ~  ProductionMeanTHI + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
summary(FINAL)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MotilityPercentageThawedTotal ~ ProductionMeanTHI + NeosporaResult +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 56623.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5841 -0.6246  0.0558  0.6808  3.8353 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 46.94    6.851         
    ##                       scaledTime  12.95    3.599    -0.04
    ##  Residual                         86.58    9.305         
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error t value
    ## (Intercept)            59.03336    1.13187   52.16
    ## ProductionMeanTHI      -0.25052    0.01528  -16.40
    ## NeosporaResultPositive -3.14807    2.60064   -1.21
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI
    ## PrdctnMnTHI -0.766       
    ## NsprRsltPst -0.179 -0.001

``` r
drop1(FINAL, test= "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageThawedTotal ~ ProductionMeanTHI + NeosporaResult + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                   npar   AIC     LRT Pr(Chi)    
    ## <none>                 56636                    
    ## ProductionMeanTHI    1 56898 263.779  <2e-16 ***
    ## NeosporaResult       1 56635   1.443  0.2296    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
plot(FINAL)
```

![](MotilityThawedTotal_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
res=resid(FINAL)
hist (res)
```

![](MotilityThawedTotal_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
qqPlot (res)
```

![](MotilityThawedTotal_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

    ## [1] 1514 2710

``` r
Anova(FINAL)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: MotilityPercentageThawedTotal
    ##                      Chisq Df Pr(>Chisq)    
    ## ProductionMeanTHI 268.9363  1     <2e-16 ***
    ## NeosporaResult      1.4653  1     0.2261    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
