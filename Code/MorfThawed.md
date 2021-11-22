MorfologyThawed
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

![](MorfThawed_files/figure-gfm/pressure-1.png)<!-- --> \# Discriptive
summary of the results

``` r
df_final %>% dplyr::group_by(NeosporaResult) %>%
  summarise(
    count = n(),
    numberofsire = n_distinct(AnimalIdentifier, na.rm = TRUE),
    mean = mean(MorfThawed, na.rm = TRUE),
    var = var(MorfThawed, na.rm = TRUE),
    stdev = sd(MorfThawed,na.rm = TRUE),
    max = max(MorfThawed, na.rm = TRUE),
    min = min(MorfThawed,na.rm = TRUE)
  )
```

    ## # A tibble: 2 x 8
    ##   NeosporaResult count numberofsire  mean   var stdev   max   min
    ##   <fct>          <int>        <int> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Negative        7018           84  96.0  6.38  2.53   100    62
    ## 2 Positive         667            8  95.9  5.66  2.38   100    84

# Model buidling

## Baseline models

Baseline model is needed to compare more complex models, and check
residual variance

``` r
LMM0.0 <- lmer(
                  MorfThawed ~ 1 + (1| AnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.1 <- lmer(
                  MorfThawed ~ 1 + (1| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.2 <- lmer(
                  MorfThawed ~ 1 + (1| AnimalIdentifier/Site), 
                  data = df_final,
                  REML = TRUE,
                  )

LMM0.3<- lmer(
                  MorfThawed ~ 1 + (1 +  scaledTime| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE,
                  )


anova(LMM0.0,LMM0.1, LMM0.2, LMM0.3)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## LMM0.0: MorfThawed ~ 1 + (1 | AnimalIdentifier)
    ## LMM0.1: MorfThawed ~ 1 + (1 | SiteAnimalIdentifier)
    ## LMM0.2: MorfThawed ~ 1 + (1 | AnimalIdentifier/Site)
    ## LMM0.3: MorfThawed ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance    Chisq Df Pr(>Chisq)    
    ## LMM0.0    3 34227 34248 -17111    34221                           
    ## LMM0.1    3 34222 34243 -17108    34216   5.5985  0               
    ## LMM0.2    4 34217 34245 -17104    34209   7.0108  1   0.008102 ** 
    ## LMM0.3    5 33824 33859 -16907    33814 394.6777  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Model building using nested LMM

``` r
LMM1.0 <- lmer(
              MorfThawed ~ SampleSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.1 <- lmer(
              MorfThawed ~ ProductionSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

LMM1.2 <- lmer(
              MorfThawed ~ Site + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.3 <- lmer(
              MorfThawed ~ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.4 <- lmer(
              MorfThawed ~ SampleMaxTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.5 <- lmer(
              MorfThawed ~ ProductionMeanTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.6 <- lmer(
              MorfThawed ~ NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

anova(LMM0.3, LMM1.0, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MorfThawed ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.0: MorfThawed ~ SampleSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 33824 33859 -16907    33814                         
    ## LMM1.0    8 33775 33831 -16880    33759 54.802  3  7.566e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.1, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MorfThawed ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.1: MorfThawed ~ ProductionSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 33824 33859 -16907    33814                         
    ## LMM1.1    8 33740 33795 -16862    33724 90.456  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.2, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MorfThawed ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.2: MorfThawed ~ Site + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## LMM0.3    5 33824 33859 -16907    33814                     
    ## LMM1.2    7 33828 33877 -16907    33814 0.0699  2     0.9656

``` r
anova(LMM0.3, LMM1.3, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MorfThawed ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.3: MorfThawed ~ Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 33824 33859 -16907    33814                         
    ## LMM1.3    6 33792 33834 -16890    33780 34.372  1  4.553e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.4, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MorfThawed ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.4: MorfThawed ~ SampleMaxTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 33824 33859 -16907    33814                         
    ## LMM1.4    6 33805 33847 -16897    33793 21.279  1  3.971e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.5, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MorfThawed ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.5: MorfThawed ~ ProductionMeanTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)   
    ## LMM0.3    5 33824 33859 -16907    33814                        
    ## LMM1.5    6 33816 33858 -16902    33804 10.064  1   0.001512 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.6, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MorfThawed ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.6: MorfThawed ~ NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## LMM0.3    5 33824 33859 -16907    33814                     
    ## LMM1.6    6 33826 33868 -16907    33814 0.1876  1     0.6649

### Backward approach

Full model with significant fixed effects

``` r
FULL <- lmer(
            MorfThawed ~ SampleMaxTHI + ProductionMeanTHI + SampleSeason + ProductionSeason + NeosporaResult+Age+ (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
vif.mer (FULL)
```

    ##           SampleMaxTHI      ProductionMeanTHI     SampleSeasonSpring 
    ##               3.362233               5.639291               4.292866 
    ##     SampleSeasonSummer     SampleSeasonWinter ProductionSeasonSummer 
    ##               2.784782               4.013038               2.734035 
    ## ProductionSeasonWinter   ProductionSeasonFall NeosporaResultPositive 
    ##               3.248323               3.482212               1.000270 
    ##                    Age 
    ##               1.359627

Multicollinearity between season and THI.

``` r
FULL1 <- lmer(
            MorfThawed ~   ProductionMeanTHI +  NeosporaResult + Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
drop1(FULL1,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MorfThawed ~ ProductionMeanTHI + NeosporaResult + Age + (1 + 
    ##     scaledTime | SiteAnimalIdentifier)
    ##                   npar   AIC     LRT   Pr(Chi)    
    ## <none>                 33792                      
    ## ProductionMeanTHI    1 33794  3.5294   0.06029 .  
    ## NeosporaResult       1 33790  0.1894   0.66338    
    ## Age                  1 33818 27.8838 1.288e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MorfThawed ~ ProductionMeanTHI + NeosporaResult + Age + (1 +  
    ##     scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 33801.7
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -15.9143  -0.4053   0.1202   0.5792   3.9763 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 3.2719   1.8088        
    ##                       scaledTime  0.6926   0.8322   -0.49
    ##  Residual                         4.4215   2.1027        
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error t value
    ## (Intercept)            94.014611   0.400414 234.794
    ## ProductionMeanTHI       0.006672   0.003553   1.878
    ## NeosporaResultPositive  0.261952   0.603436   0.434
    ## Age                     0.001189   0.000210   5.659
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI NsprRP
    ## PrdctnMnTHI -0.340              
    ## NsprRsltPst -0.118 -0.011       
    ## Age         -0.731 -0.236  0.000

``` r
FULL2 <- lmer(
            MorfThawed ~ SampleMaxTHI +  NeosporaResult + Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
drop1(FULL2,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MorfThawed ~ SampleMaxTHI + NeosporaResult + Age + (1 + scaledTime | 
    ##     SiteAnimalIdentifier)
    ##                npar   AIC     LRT   Pr(Chi)    
    ## <none>              33778                      
    ## SampleMaxTHI      1 33794 17.7004 2.586e-05 ***
    ## NeosporaResult    1 33776  0.1909    0.6622    
    ## Age               1 33807 30.8317 2.814e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL2)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MorfThawed ~ SampleMaxTHI + NeosporaResult + Age + (1 + scaledTime |  
    ##     SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 33788.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -15.9376  -0.4100   0.1229   0.5805   3.9217 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 3.363    1.8339        
    ##                       scaledTime  0.704    0.8391   -0.49
    ##  Residual                         4.411    2.1002        
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error t value
    ## (Intercept)            9.371e+01  4.011e-01 233.606
    ## SampleMaxTHI           1.096e-02  2.608e-03   4.204
    ## NeosporaResultPositive 2.661e-01  6.112e-01   0.435
    ## Age                    1.234e-03  2.065e-04   5.975
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmMTHI NsprRP
    ## SampleMxTHI -0.318              
    ## NsprRsltPst -0.121 -0.006       
    ## Age         -0.815 -0.076 -0.002

``` r
FULL3 <- lmer(
            MorfThawed ~   ProductionSeason +  NeosporaResult + Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
drop1(FULL3,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MorfThawed ~ ProductionSeason + NeosporaResult + Age + (1 + scaledTime | 
    ##     SiteAnimalIdentifier)
    ##                  npar   AIC    LRT   Pr(Chi)    
    ## <none>                33741                     
    ## ProductionSeason    3 33794 58.875 1.022e-12 ***
    ## NeosporaResult      1 33739  0.130   0.71805    
    ## Age                 1 33742  2.879   0.08974 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL3)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## MorfThawed ~ ProductionSeason + NeosporaResult + Age + (1 + scaledTime |  
    ##     SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 33747.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -15.8189  -0.4134   0.1156   0.5666   3.9441 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 2.3811   1.5431        
    ##                       scaledTime  0.5448   0.7381   -0.50
    ##  Residual                         4.4184   2.1020        
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            95.6129212  0.3600288 265.570
    ## ProductionSeasonSummer  0.0180885  0.0774855   0.233
    ## ProductionSeasonWinter -0.5163241  0.0723470  -7.137
    ## ProductionSeasonFall    0.2768880  0.0924942   2.994
    ## NeosporaResultPositive  0.1871123  0.5189771   0.361
    ## Age                     0.0003713  0.0002114   1.756
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrdcSS PrdcSW PrdcSF NsprRP
    ## PrdctnSsnSm  0.089                            
    ## PrdctnSsnWn -0.287  0.232                     
    ## PrdctnSsnFl  0.225  0.592  0.104              
    ## NsprRsltPst -0.121 -0.008  0.005 -0.008       
    ## Age         -0.897 -0.241  0.264 -0.399  0.003

``` r
FULL4 <- lmer(
            MorfThawed ~   SampleSeason +  NeosporaResult + Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
drop1(FULL4,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MorfThawed ~ SampleSeason + NeosporaResult + Age + (1 + scaledTime | 
    ##     SiteAnimalIdentifier)
    ##                npar   AIC    LRT   Pr(Chi)    
    ## <none>              33743                     
    ## SampleSeason      3 33794 56.421 3.416e-12 ***
    ## NeosporaResult    1 33741  0.215    0.6427    
    ## Age               1 33777 36.032 1.941e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL4)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MorfThawed ~ SampleSeason + NeosporaResult + Age + (1 + scaledTime |  
    ##     SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 33749.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -15.9219  -0.4091   0.1215   0.5860   3.9234 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 3.7721   1.9422        
    ##                       scaledTime  0.7458   0.8636   -0.48
    ##  Residual                         4.3797   2.0928        
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error t value
    ## (Intercept)            9.376e+01  4.236e-01 221.337
    ## SampleSeasonSpring     3.132e-01  8.627e-02   3.630
    ## SampleSeasonSummer     4.825e-01  7.610e-02   6.340
    ## SampleSeasonWinter     5.427e-02  8.444e-02   0.643
    ## NeosporaResultPositive 3.015e-01  6.502e-01   0.464
    ## Age                    1.483e-03  2.251e-04   6.588
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmplSsnSp SmplSsnSm SmplSW NsprRP
    ## SmplSsnSprn -0.317                                  
    ## SmplSsnSmmr -0.214  0.612                           
    ## SmplSsnWntr -0.302  0.675     0.581                 
    ## NsprRsltPst -0.127  0.009     0.006     0.010       
    ## Age         -0.894  0.276     0.148     0.258  0.001

``` r
anova(FULL1, FULL2, FULL3, FULL4)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## FULL1: MorfThawed ~ ProductionMeanTHI + NeosporaResult + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL2: MorfThawed ~ SampleMaxTHI + NeosporaResult + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL3: MorfThawed ~ ProductionSeason + NeosporaResult + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL4: MorfThawed ~ SampleSeason + NeosporaResult + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## FULL1    8 33792 33848 -16888    33776                         
    ## FULL2    8 33778 33834 -16881    33762 14.171  0               
    ## FULL3   10 33741 33810 -16860    33721 41.175  2  1.146e-09 ***
    ## FULL4   10 33743 33813 -16862    33723  0.000  0               
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Final model

``` r
FINAL <- lmer(
            MorfThawed ~   ProductionMeanTHI +  NeosporaResult + Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
summary (FINAL)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MorfThawed ~ ProductionMeanTHI + NeosporaResult + Age + (1 +  
    ##     scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 33801.7
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -15.9143  -0.4053   0.1202   0.5792   3.9763 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 3.2719   1.8088        
    ##                       scaledTime  0.6926   0.8322   -0.49
    ##  Residual                         4.4215   2.1027        
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error t value
    ## (Intercept)            94.014611   0.400414 234.794
    ## ProductionMeanTHI       0.006672   0.003553   1.878
    ## NeosporaResultPositive  0.261952   0.603436   0.434
    ## Age                     0.001189   0.000210   5.659
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI NsprRP
    ## PrdctnMnTHI -0.340              
    ## NsprRsltPst -0.118 -0.011       
    ## Age         -0.731 -0.236  0.000

``` r
drop1(FINAL,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## MorfThawed ~ ProductionMeanTHI + NeosporaResult + Age + (1 + 
    ##     scaledTime | SiteAnimalIdentifier)
    ##                   npar   AIC     LRT   Pr(Chi)    
    ## <none>                 33792                      
    ## ProductionMeanTHI    1 33794  3.5294   0.06029 .  
    ## NeosporaResult       1 33790  0.1894   0.66338    
    ## Age                  1 33818 27.8838 1.288e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif.mer(FINAL)
```

    ##      ProductionMeanTHI NeosporaResultPositive                    Age 
    ##               1.059309               1.000131               1.059177

#### Final model with interaction

Interaction Age\*ProductionMeanTHI

``` r
FINAL1 <- lmer(MorfThawed ~ProductionMeanTHI*Age + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final)
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(FINAL1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: MorfThawed ~ ProductionMeanTHI * Age + NeosporaResult + (1 +  
    ##     scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 33822
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -15.9153  -0.4085   0.1178   0.5762   3.9535 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr 
    ##  SiteAnimalIdentifier (Intercept) 3.2802   1.8111        
    ##                       scaledTime  0.7033   0.8386   -0.51
    ##  Residual                         4.4211   2.1026        
    ## Number of obs: 7685, groups:  SiteAnimalIdentifier, 104
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)             9.338e+01  5.957e-01 156.750
    ## ProductionMeanTHI       1.876e-02  8.649e-03   2.169
    ## Age                     1.598e-03  3.481e-04   4.591
    ## NeosporaResultPositive  2.636e-01  5.970e-01   0.442
    ## ProductionMeanTHI:Age  -7.509e-06  4.909e-06  -1.530
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI Age    NsprRP
    ## PrdctnMnTHI -0.772                     
    ## Age         -0.888  0.672              
    ## NsprRsltPst -0.066 -0.020 -0.013       
    ## PrdctMTHI:A  0.743 -0.912 -0.801  0.016
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
    ## MorfThawed ~ ProductionMeanTHI * Age + NeosporaResult + (1 + 
    ##     scaledTime | SiteAnimalIdentifier)
    ##                       npar   AIC     LRT Pr(Chi)
    ## <none>                     33792                
    ## NeosporaResult           1 33790 0.19623  0.6578
    ## ProductionMeanTHI:Age    1 33792 2.35573  0.1248

``` r
plot(FINAL1)
```

![](MorfThawed_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
plot_model(FINAL1, type = "pred", terms = c("Age","ProductionMeanTHI"))
```

![](MorfThawed_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
res=resid(FINAL1)
hist (res)
```

![](MorfThawed_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
qqPlot (res)
```

![](MorfThawed_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

    ## [1] 1708 3417

``` r
Anova(FINAL1)   
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: MorfThawed
    ##                         Chisq Df Pr(>Chisq)    
    ## ProductionMeanTHI      3.5499  1    0.05955 .  
    ## Age                   31.5930  1  1.901e-08 ***
    ## NeosporaResult         0.1950  1    0.65878    
    ## ProductionMeanTHI:Age  2.3401  1    0.12608    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
