Concentration
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

![](Concentration_files/figure-gfm/pressure-1.png)<!-- --> \#
Discriptive summary of the results

``` r
df_final %>% dplyr::group_by(NeosporaResult) %>%
  summarise(
    count = n(),
    numberofsire = n_distinct(AnimalIdentifier, na.rm = TRUE),
    mean = mean(Concentration, na.rm = TRUE),
    var = var(Concentration, na.rm = TRUE),
    stdev = sd(Concentration,na.rm = TRUE),
    max = max(Concentration, na.rm = TRUE), 
    min = min(Concentration, na.rm = TRUE)
  )
```

    ## # A tibble: 2 x 8
    ##   NeosporaResult count numberofsire  mean     var stdev   max   min
    ##   <fct>          <int>        <int> <dbl>   <dbl> <dbl> <dbl> <dbl>
    ## 1 Negative        9537           84 1395. 243810.  494.  3675    17
    ## 2 Positive         941            8 1472. 283460.  532.  3029   189

# Model buidling

## Baseline models

Baseline model is needed to compare more complex models, and check
residual variance

``` r
LMM0.0 <- lmer(
                  Concentration ~ 1 + (1| AnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.1 <- lmer(
                  Concentration ~ 1 + (1| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE
                  )
LMM0.2 <- lmer(
                  Concentration ~ 1 + (1| AnimalIdentifier/Site), 
                  data = df_final,
                  REML = TRUE,
                  )

LMM0.3<- lmer(
                  Concentration ~ 1 + (1 +  scaledTime| SiteAnimalIdentifier), 
                  data = df_final,
                  REML = TRUE,
                  )


anova(LMM0.0,LMM0.1, LMM0.2, LMM0.3)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## LMM0.0: Concentration ~ 1 + (1 | AnimalIdentifier)
    ## LMM0.1: Concentration ~ 1 + (1 | SiteAnimalIdentifier)
    ## LMM0.2: Concentration ~ 1 + (1 | AnimalIdentifier/Site)
    ## LMM0.3: Concentration ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC logLik deviance   Chisq Df Pr(>Chisq)    
    ## LMM0.0    3 155748 155770 -77871   155742                          
    ## LMM0.1    3 155731 155753 -77863   155725  17.235  0               
    ## LMM0.2    4 155715 155744 -77854   155707  18.066  1  2.134e-05 ***
    ## LMM0.3    5 155152 155188 -77571   155142 565.425  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Model building using nested LMM

``` r
LMM1.0 <- lmer(
              Concentration ~ SampleSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.1 <- lmer(
              Concentration ~ ProductionSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

LMM1.2 <- lmer(
              Concentration ~ Site + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.3 <- lmer(
            Concentration ~ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.4 <- lmer(
            Concentration ~ SampleMaxTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.5 <- lmer(
            Concentration ~ ProductionMeanTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
LMM1.6 <- lmer(
              Concentration ~ NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

anova(LMM0.3, LMM1.0, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Concentration ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.0: Concentration ~ SampleSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 155152 155188 -77571   155142                         
    ## LMM1.0    8 154892 154950 -77438   154876 265.31  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.1, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Concentration ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.1: Concentration ~ ProductionSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 155152 155188 -77571   155142                         
    ## LMM1.1    8 155056 155114 -77520   155040 101.98  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.2, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Concentration ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.2: Concentration ~ Site + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)  
    ## LMM0.3    5 155152 155188 -77571   155142                       
    ## LMM1.2    7 155148 155199 -77567   155134 7.2281  2    0.02694 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.3, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Concentration ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.3: Concentration ~ Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 155152 155188 -77571   155142                         
    ## LMM1.3    6 155119 155162 -77553   155107 34.798  1  3.658e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.4, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Concentration ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.4: Concentration ~ SampleMaxTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 155152 155188 -77571   155142                         
    ## LMM1.4    6 155129 155173 -77559   155117 24.684  1  6.754e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.5, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Concentration ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.5: Concentration ~ ProductionMeanTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    5 155152 155188 -77571   155142                         
    ## LMM1.5    6 155017 155060 -77502   155005 137.02  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.6, test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: Concentration ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.6: Concentration ~ NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)
    ## LMM0.3    5 155152 155188 -77571   155142                     
    ## LMM1.6    6 155153 155196 -77570   155141 0.7912  1     0.3737

### Backward approach

#### Full model with significants fixed effects

``` r
FULL <- lmer(
            Concentration ~ SampleMaxTHI + ProductionMeanTHI + SampleSeason + ProductionSeason + NeosporaResult+Site+Age+ (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

vif.mer(FULL)
```

    ##           SampleMaxTHI      ProductionMeanTHI     SampleSeasonSpring 
    ##               3.298214               5.506078               4.103157 
    ##     SampleSeasonSummer     SampleSeasonWinter ProductionSeasonSummer 
    ##               2.699207               3.844908               2.920787 
    ## ProductionSeasonWinter   ProductionSeasonFall NeosporaResultPositive 
    ##               3.119295               3.748690               1.094146 
    ##                  Site6                  Site8                    Age 
    ##               1.230584               1.265557               1.639096

Multicollinearity between season and THI.

``` r
FULL1 <- lmer(
            Concentration ~ ProductionMeanTHI + NeosporaResult+Site+ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

summary (FULL1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Concentration ~ ProductionMeanTHI + NeosporaResult + Site + Age +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 154943
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6503 -0.5981  0.0418  0.6295  3.9076 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 114378   338.2        
    ##                       scaledTime   24150   155.4    0.23
    ##  Residual                         146729   383.1        
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            2123.94037   81.15232  26.172
    ## ProductionMeanTHI        -5.89045    0.54987 -10.712
    ## NeosporaResultPositive   99.24269  129.97530   0.764
    ## Site6                  -299.30943   83.67297  -3.577
    ## Site8                   -75.65648   81.27008  -0.931
    ## Age                      -0.16778    0.04087  -4.105
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI NsprRP Site6  Site8 
    ## PrdctnMnTHI -0.177                            
    ## NsprRsltPst -0.066 -0.001                     
    ## Site6       -0.480 -0.046 -0.019              
    ## Site8       -0.342 -0.006 -0.279  0.360       
    ## Age         -0.710 -0.238  0.043  0.178 -0.018

``` r
drop1(FULL1, test = "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Concentration ~ ProductionMeanTHI + NeosporaResult + Site + Age + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                   npar    AIC     LRT   Pr(Chi)    
    ## <none>                 155000                      
    ## ProductionMeanTHI    1 155111 112.791 < 2.2e-16 ***
    ## NeosporaResult       1 154999   0.617  0.432241    
    ## Site                 2 155008  12.140  0.002312 ** 
    ## Age                  1 155014  15.510 8.206e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
FULL2 <- lmer(
           Concentration ~ SampleMaxTHI +  NeosporaResult+ Site + Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

summary(FULL2)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Concentration ~ SampleMaxTHI + NeosporaResult + Site + Age +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 155034
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6218 -0.6035  0.0424  0.6263  3.9687 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 133629   365.6        
    ##                       scaledTime   27239   165.0    0.19
    ##  Residual                         147647   384.2        
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            2073.66832   87.11688  23.803
    ## SampleMaxTHI             -1.90176    0.41010  -4.637
    ## NeosporaResultPositive   95.19405  140.95982   0.675
    ## Site6                  -336.33857   90.68084  -3.709
    ## Site8                   -74.01725   88.19020  -0.839
    ## Age                      -0.27384    0.04182  -6.547
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmMTHI NsprRP Site6  Site8 
    ## SampleMxTHI -0.211                            
    ## NsprRsltPst -0.065 -0.001                     
    ## Site6       -0.484 -0.012 -0.020              
    ## Site8       -0.343 -0.007 -0.279  0.359       
    ## Age         -0.746 -0.065  0.042  0.168 -0.020

``` r
drop1(FULL2,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Concentration ~ SampleMaxTHI + NeosporaResult + Site + Age + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                npar    AIC    LRT   Pr(Chi)    
    ## <none>              155092                     
    ## SampleMaxTHI      1 155111 21.547 3.452e-06 ***
    ## NeosporaResult    1 155090  0.488  0.484653    
    ## Site              2 155101 13.253  0.001325 ** 
    ## Age               1 155127 37.098 1.123e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
FULL3 <- lmer(
            Concentration ~ ProductionSeason + NeosporaResult+Site+ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

summary(FULL3)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Concentration ~ ProductionSeason + NeosporaResult + Site + Age +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 154973.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6032 -0.5967  0.0381  0.6313  4.0411 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 105137   324.2        
    ##                       scaledTime   22451   149.8    0.22
    ##  Residual                         147676   384.3        
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            1602.22057   90.61293  17.682
    ## ProductionSeasonSummer  -69.27785   12.34265  -5.613
    ## ProductionSeasonWinter   53.32166   11.94348   4.465
    ## ProductionSeasonFall    -90.40620   15.55082  -5.814
    ## NeosporaResultPositive   99.87582  124.99219   0.799
    ## Site6                  -245.04656   81.31044  -3.014
    ## Site8                   -92.18757   78.17070  -1.179
    ## Age                      -0.04138    0.04840  -0.855
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrdcSS PrdcSW PrdcSF NsprRP Site6  Site8 
    ## PrdctnSsnSm  0.216                                          
    ## PrdctnSsnWn -0.340  0.166                                   
    ## PrdctnSsnFl  0.382  0.638  0.015                            
    ## NsprRsltPst -0.060  0.003  0.006  0.001                     
    ## Site6       -0.496 -0.073  0.082 -0.120 -0.018              
    ## Site8       -0.285  0.010 -0.009  0.018 -0.279  0.353       
    ## Age         -0.849 -0.309  0.328 -0.493  0.037  0.227 -0.028

``` r
drop1(FULL3, test = "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Concentration ~ ProductionSeason + NeosporaResult + Site + Age + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                  npar    AIC    LRT   Pr(Chi)    
    ## <none>                155054                     
    ## ProductionSeason    3 155111 62.778 1.498e-13 ***
    ## NeosporaResult      1 155053  0.671   0.41259    
    ## Site                2 155059  8.833   0.01208 *  
    ## Age                 1 155053  0.726   0.39431    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
FULL4 <- lmer(
            Concentration ~ SampleSeason + NeosporaResult+Site+ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

summary(FULL4)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Concentration ~ SampleSeason + NeosporaResult + Site + Age +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 154804
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8353 -0.5915  0.0459  0.6362  4.0669 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 108085   328.8        
    ##                       scaledTime   22172   148.9    0.23
    ##  Residual                         145234   381.1        
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            1581.95419   83.48891  18.948
    ## SampleSeasonSpring      131.44276   13.40739   9.804
    ## SampleSeasonSummer      122.06165   11.51857  10.597
    ## SampleSeasonWinter      198.68829   13.07208  15.199
    ## NeosporaResultPositive   95.57303  126.43740   0.756
    ## Site6                  -279.76709   81.45634  -3.435
    ## Site8                   -90.07035   79.04570  -1.139
    ## Age                      -0.11214    0.04094  -2.739
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmplSsnSp SmplSsnSm SmplSW NsprRP Site6  Site8 
    ## SmplSsnSprn -0.336                                                
    ## SmplSsnSmmr -0.217  0.599                                         
    ## SmplSsnWntr -0.307  0.650     0.561                               
    ## NsprRsltPst -0.062 -0.003    -0.003    -0.001                     
    ## Site6       -0.485  0.058     0.031     0.051 -0.019              
    ## Site8       -0.321 -0.008    -0.007    -0.007 -0.279  0.359       
    ## Age         -0.811  0.297     0.164     0.263  0.041  0.184 -0.021

``` r
drop1(FULL4, test = "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Concentration ~ SampleSeason + NeosporaResult + Site + Age + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                npar    AIC     LRT   Pr(Chi)    
    ## <none>              154884                      
    ## SampleSeason      3 155111 233.026 < 2.2e-16 ***
    ## NeosporaResult    1 154883   0.602  0.437794    
    ## Site              2 154891  11.302  0.003513 ** 
    ## Age               1 154889   7.128  0.007587 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(FULL1, FULL2, FULL3, FULL4)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df_final
    ## Models:
    ## FULL1: Concentration ~ ProductionMeanTHI + NeosporaResult + Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL2: Concentration ~ SampleMaxTHI + NeosporaResult + Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL3: Concentration ~ ProductionSeason + NeosporaResult + Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL4: Concentration ~ SampleSeason + NeosporaResult + Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##       npar    AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)    
    ## FULL1   10 155000 155073 -77490   154980                         
    ## FULL2   10 155092 155164 -77536   155072   0.00  0               
    ## FULL3   12 155054 155141 -77515   155030  41.23  2  1.114e-09 ***
    ## FULL4   12 154884 154971 -77430   154860 170.25  0               
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Final Model

``` r
FINAL <- lmer(
            Concentration ~ ProductionMeanTHI + NeosporaResult+Site+ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )

summary (FINAL)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Concentration ~ ProductionMeanTHI + NeosporaResult + Site + Age +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 154943
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6503 -0.5981  0.0418  0.6295  3.9076 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 114378   338.2        
    ##                       scaledTime   24150   155.4    0.23
    ##  Residual                         146729   383.1        
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)            2123.94037   81.15232  26.172
    ## ProductionMeanTHI        -5.89045    0.54987 -10.712
    ## NeosporaResultPositive   99.24269  129.97530   0.764
    ## Site6                  -299.30943   83.67297  -3.577
    ## Site8                   -75.65648   81.27008  -0.931
    ## Age                      -0.16778    0.04087  -4.105
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI NsprRP Site6  Site8 
    ## PrdctnMnTHI -0.177                            
    ## NsprRsltPst -0.066 -0.001                     
    ## Site6       -0.480 -0.046 -0.019              
    ## Site8       -0.342 -0.006 -0.279  0.360       
    ## Age         -0.710 -0.238  0.043  0.178 -0.018

``` r
drop1(FINAL, test = "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## Concentration ~ ProductionMeanTHI + NeosporaResult + Site + Age + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                   npar    AIC     LRT   Pr(Chi)    
    ## <none>                 155000                      
    ## ProductionMeanTHI    1 155111 112.791 < 2.2e-16 ***
    ## NeosporaResult       1 154999   0.617  0.432241    
    ## Site                 2 155008  12.140  0.002312 ** 
    ## Age                  1 155014  15.510 8.206e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif.mer(FINAL)
```

    ##      ProductionMeanTHI NeosporaResultPositive                  Site6 
    ##               1.060253               1.093987               1.204328 
    ##                  Site8                    Age 
    ##               1.263754               1.101664

#### Final model with interaction

Interaction Age\*ProductionMeanTHI

``` r
FINAL1 <- lmer(
            Concentration ~ ProductionMeanTHI*Age + NeosporaResult + Site + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final
              )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(FINAL1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Concentration ~ ProductionMeanTHI * Age + NeosporaResult + Site +  
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ## REML criterion at convergence: 154936
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6866 -0.6003  0.0436  0.6323  3.8901 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 120492   347.1        
    ##                       scaledTime   24050   155.1    0.27
    ##  Residual                         146427   382.7        
    ## Number of obs: 10476, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error t value
    ## (Intercept)             1.856e+03  1.034e+02  17.950
    ## ProductionMeanTHI      -3.432e-01  1.366e+00  -0.251
    ## Age                     5.286e-03  5.757e-02   0.092
    ## NeosporaResultPositive  9.788e+01  1.321e+02   0.741
    ## Site6                  -3.108e+02  8.490e+01  -3.661
    ## Site8                  -7.824e+01  8.254e+01  -0.948
    ## ProductionMeanTHI:Age  -3.504e-03  7.904e-04  -4.433
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI Age    NsprRP Site6  Site8 
    ## PrdctnMnTHI -0.615                                   
    ## Age         -0.829  0.570                            
    ## NsprRsltPst -0.050 -0.005  0.027                     
    ## Site6       -0.377 -0.023  0.121 -0.019              
    ## Site8       -0.276  0.001 -0.010 -0.279  0.361       
    ## PrdctMTHI:A  0.613 -0.916 -0.698  0.005  0.005 -0.003
    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

``` r
drop1(FINAL1, test= "Chisq")
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling
    
    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

    ## Single term deletions
    ## 
    ## Model:
    ## Concentration ~ ProductionMeanTHI * Age + NeosporaResult + Site + 
    ##     (1 + scaledTime | SiteAnimalIdentifier)
    ##                       npar    AIC     LRT   Pr(Chi)    
    ## <none>                     154983                      
    ## NeosporaResult           1 154982  0.5818  0.445603    
    ## Site                     2 154992 12.7723  0.001685 ** 
    ## ProductionMeanTHI:Age    1 155000 19.2766 1.131e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
plot(FINAL1)
```

![](Concentration_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
plot_model(FINAL1, type = "pred", terms = c("Age","ProductionMeanTHI"))
```

![](Concentration_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
res=resid(FINAL1)
hist (res)
```

![](Concentration_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
qqPlot (res)
```

![](Concentration_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

    ## 3126 7373 
    ## 3126 7372

``` r
Anova(FINAL1) 
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Concentration
    ##                          Chisq Df Pr(>Chisq)    
    ## ProductionMeanTHI     114.7740  1  < 2.2e-16 ***
    ## Age                    17.5315  1  2.826e-05 ***
    ## NeosporaResult          0.5489  1   0.458756    
    ## Site                   13.5657  2   0.001133 ** 
    ## ProductionMeanTHI:Age  19.6480  1  9.310e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
