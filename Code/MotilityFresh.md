MotilityFresh
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
df_final = df_final %>% dplyr::mutate( 
 MotilityPercentageFreshCat = as.factor(case_when(
                  MotilityPercentageFresh >= 60 ~ 1,
                  MotilityPercentageFresh <= 59 ~ 0,
                   TRUE ~ 2
                  ))
 )
df_final = subset.data.frame(df_final,MotilityPercentageFresh != 2)
```

# Histogram outcome variable

    ##    0    1    2 
    ## 1504 8971    0

![](MotilityFresh_files/figure-gfm/pressure-1.png)<!-- --> \#
Discriptive summary of the results

``` r
df_final %>% dplyr::group_by(NeosporaResult) %>%
  summarise(
    count = n(),
    numberofsire = n_distinct(AnimalIdentifier, na.rm = TRUE),
    mean = mean(MotilityPercentageFresh, na.rm = TRUE),
    var = var(MotilityPercentageFresh, na.rm = TRUE),
    stdev = sd(MotilityPercentageFresh,na.rm = TRUE),
    max = max(MotilityPercentageFresh, na.rm = TRUE),
    min = min (MotilityPercentageFresh, na.rm = TRUE)
  )
```

    ## # A tibble: 2 x 8
    ##   NeosporaResult count numberofsire  mean   var stdev   max   min
    ##   <fct>          <int>        <int> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Negative        9534           84  64.3  216.  14.7    99     1
    ## 2 Positive         941            8  61.2  167.  12.9    85    10

# Model buidling

## Baseline models

Baseline model is needed to compare more complex models, and check
residual variance

``` r
LMM0.0 <- glmer(
                  MotilityPercentageFreshCat ~ 1 + (1| AnimalIdentifier), 
                  data = df_final, family="binomial"
                  
                  )
LMM0.1 <- glmer(
                  MotilityPercentageFreshCat ~ 1 + (1| SiteAnimalIdentifier), 
                  data = df_final, family="binomial"
                  
                  )
LMM0.2 <- glmer(
                  MotilityPercentageFreshCat ~ 1 + (1| AnimalIdentifier/Site), 
                  data = df_final, family="binomial"
                
                  )

LMM0.3<- glmer(
                  MotilityPercentageFreshCat ~ 1 + (1 +  scaledTime| SiteAnimalIdentifier), 
                  data = df_final,family="binomial"
                 
                  )


anova(LMM0.0,LMM0.1, LMM0.2, LMM0.3)
```

    ## Data: df_final
    ## Models:
    ## LMM0.0: MotilityPercentageFreshCat ~ 1 + (1 | AnimalIdentifier)
    ## LMM0.1: MotilityPercentageFreshCat ~ 1 + (1 | SiteAnimalIdentifier)
    ## LMM0.2: MotilityPercentageFreshCat ~ 1 + (1 | AnimalIdentifier/Site)
    ## LMM0.3: MotilityPercentageFreshCat ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC  logLik deviance    Chisq Df Pr(>Chisq)    
    ## LMM0.0    2 6876.9 6891.4 -3436.5   6872.9                           
    ## LMM0.1    2 6774.7 6789.2 -3385.4   6770.7 102.2122  0               
    ## LMM0.2    3 6774.8 6796.5 -3384.4   6768.8   1.9516  1     0.1624    
    ## LMM0.3    4 6551.0 6580.0 -3271.5   6543.0 225.7597  1     <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Model building using nested LMM

``` r
LMM1.0 <- glmer(
              MotilityPercentageFreshCat ~ SampleSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final,family="binomial"
              )
LMM1.1 <- glmer(
              MotilityPercentageFreshCat ~ ProductionSeason + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final,family="binomial"
              )

LMM1.2 <- glmer(
              MotilityPercentageFreshCat ~ Site + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final,family="binomial"
              )
LMM1.3 <- glmer(
            MotilityPercentageFreshCat ~ Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final,family="binomial"
              )
LMM1.4 <- glmer(
            MotilityPercentageFreshCat ~ SampleMaxTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final,family ="binomial"
              )
LMM1.5 <- glmer(
            MotilityPercentageFreshCat ~ ProductionMeanTHI + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final,family ="binomial"
              )
LMM1.6 <- glmer(
              MotilityPercentageFreshCat ~ NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final,family="binomial"
              )

anova(LMM0.3, LMM1.0,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageFreshCat ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.0: MotilityPercentageFreshCat ~ SampleSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar  AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)    
    ## LMM0.3    4 6551 6580.0 -3271.5     6543                        
    ## LMM1.0    7 6360 6410.8 -3173.0     6346   197  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.1,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageFreshCat ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.1: MotilityPercentageFreshCat ~ ProductionSeason + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    4 6551.0 6580.0 -3271.5   6543.0                         
    ## LMM1.1    7 6335.8 6386.6 -3160.9   6321.8 221.19  3  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.2,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageFreshCat ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.2: MotilityPercentageFreshCat ~ Site + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)  
    ## LMM0.3    4 6551.0 6580.0 -3271.5   6543.0                      
    ## LMM1.2    6 6547.8 6591.4 -3267.9   6535.8 7.149  2    0.02803 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.3,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageFreshCat ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.3: MotilityPercentageFreshCat ~ Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar  AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    4 6551 6580.0 -3271.5     6543                         
    ## LMM1.3    5 6521 6557.3 -3255.5     6511 31.945  1  1.586e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.4,test ="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageFreshCat ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.4: MotilityPercentageFreshCat ~ SampleMaxTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    4 6551.0 6580.0 -3271.5   6543.0                         
    ## LMM1.4    5 6496.2 6532.5 -3243.1   6486.2 56.813  1  4.793e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.5,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageFreshCat ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.5: MotilityPercentageFreshCat ~ ProductionMeanTHI + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## LMM0.3    4 6551.0 6580.0 -3271.5   6543.0                         
    ## LMM1.5    5 6367.4 6403.7 -3178.7   6357.4 185.62  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(LMM0.3, LMM1.6,test="Chisq")
```

    ## Data: df_final
    ## Models:
    ## LMM0.3: MotilityPercentageFreshCat ~ 1 + (1 + scaledTime | SiteAnimalIdentifier)
    ## LMM1.6: MotilityPercentageFreshCat ~ NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##        npar  AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
    ## LMM0.3    4 6551 6580.0 -3271.5     6543                    
    ## LMM1.6    5 6553 6589.3 -3271.5     6543  0.01  1     0.9205

### Backward approach

#### Full model with significant fixed effects.

``` r
FULL <- glmer(
            MotilityPercentageFreshCat ~ SampleMaxTHI +ProductionMeanTHI + SampleSeason + ProductionSeason + NeosporaResult+Site+Age+ (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final, family="binomial"
              )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.164524 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
vif.mer (FULL)
```

    ##           SampleMaxTHI      ProductionMeanTHI     SampleSeasonSpring 
    ##               3.280888               5.197750               2.953075 
    ##     SampleSeasonSummer     SampleSeasonWinter ProductionSeasonSummer 
    ##               2.456173               3.240285               3.306836 
    ## ProductionSeasonWinter   ProductionSeasonFall NeosporaResultPositive 
    ##               2.863033               4.069586               1.100476 
    ##                  Site6                  Site8                    Age 
    ##               1.221475               1.263443               1.346464

Multicollinearity between season and THI.

``` r
FULL1 <- glmer(
           MotilityPercentageFreshCat ~ ProductionMeanTHI +  Site + NeosporaResult + Age +(1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final, family= "binomial"
              )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00345814 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
drop1(FULL1,test="Chisq")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00274474 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00369871 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageFreshCat ~ ProductionMeanTHI + Site + NeosporaResult + 
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##                   npar    AIC     LRT   Pr(Chi)    
    ## <none>                 6355.7                      
    ## ProductionMeanTHI    1 6523.1 169.466 < 2.2e-16 ***
    ## Site                 2 6357.2   5.543 0.0625759 .  
    ## NeosporaResult       1 6354.3   0.634 0.4260624    
    ## Age                  1 6364.8  11.167 0.0008328 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## MotilityPercentageFreshCat ~ ProductionMeanTHI + Site + NeosporaResult +  
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6355.7   6421.0  -3168.8   6337.7    10466 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -12.8964   0.0869   0.1800   0.3327   2.7459 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.4748   1.8641       
    ##                       scaledTime  0.9543   0.9769   0.03
    ## Number of obs: 10475, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             7.3497494  0.6195210  11.864   <2e-16 ***
    ## ProductionMeanTHI      -0.0625854  0.0048500 -12.904   <2e-16 ***
    ## Site6                   0.9249055  0.5095187   1.815   0.0695 .  
    ## Site8                   1.0290004  0.4881528   2.108   0.0350 *  
    ## NeosporaResultPositive -0.6081262  0.7667671  -0.793   0.4277    
    ## Age                    -0.0009634  0.0003035  -3.175   0.0015 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI Site6  Site8  NsprRP
    ## PrdctnMnTHI -0.344                            
    ## Site6       -0.427 -0.048                     
    ## Site8       -0.209 -0.009  0.326              
    ## NsprRsltPst -0.088 -0.002 -0.010 -0.292       
    ## Age         -0.756 -0.143  0.227 -0.069  0.076
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.00345814 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
FULL2 <- glmer(
           MotilityPercentageFreshCat ~ SampleMaxTHI + NeosporaResult + Site + Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final, family= "binomial"
              )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
drop1(FULL2,test="Chisq")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?
    
    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?
    
    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageFreshCat ~ SampleMaxTHI + NeosporaResult + 
    ##     Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##                npar    AIC    LRT   Pr(Chi)    
    ## <none>              6468.8                     
    ## SampleMaxTHI      1 6523.1 56.302 6.215e-14 ***
    ## NeosporaResult    1 6467.5  0.649    0.4204    
    ## Site              2 6469.0  4.134    0.1266    
    ## Age               1 6494.3 27.471 1.595e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL2)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: MotilityPercentageFreshCat ~ SampleMaxTHI + NeosporaResult +  
    ##     Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6468.8   6534.1  -3225.4   6450.8    10466 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -9.7930  0.0895  0.1813  0.3449  2.1966 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 4.365    2.089        
    ##                       scaledTime  1.028    1.014    0.12
    ## Number of obs: 10475, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             6.471878   0.716020   9.039  < 2e-16 ***
    ## SampleMaxTHI           -0.026792   0.003538  -7.573 3.66e-14 ***
    ## NeosporaResultPositive -0.679322   0.849347  -0.800   0.4238    
    ## Site6                   0.635115   0.564140   1.126   0.2602    
    ## Site8                   1.086431   0.540462   2.010   0.0444 *  
    ## Age                    -0.001664   0.000361  -4.611 4.01e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmMTHI NsprRP Site6  Site8 
    ## SampleMxTHI -0.286                            
    ## NsprRsltPst -0.109  0.001                     
    ## Site6       -0.454 -0.011 -0.002              
    ## Site8       -0.181 -0.014 -0.293  0.318       
    ## Age         -0.847  0.001  0.095  0.239 -0.082
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
FULL3 <- glmer(
           MotilityPercentageFreshCat ~ ProductionSeason + NeosporaResult + Site + Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final, family= "binomial"
              )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.227699 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
drop1(FULL3,test="Chisq")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0325602 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00347445 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageFreshCat ~ ProductionSeason + NeosporaResult + 
    ##     Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##                  npar    AIC     LRT Pr(Chi)    
    ## <none>                6335.4                    
    ## ProductionSeason    3 6523.1 193.690 < 2e-16 ***
    ## NeosporaResult      1 6334.1   0.633 0.42631    
    ## Site                2 6337.9   6.495 0.03886 *  
    ## Age                 1 6334.1   0.690 0.40611    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL3)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: MotilityPercentageFreshCat ~ ProductionSeason + NeosporaResult +  
    ##     Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6335.4   6415.3  -3156.7   6313.4    10464 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -12.9244   0.0910   0.1769   0.3289   3.6527 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.0772   1.754        
    ##                       scaledTime  0.7074   0.841    0.00
    ## Number of obs: 10475, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             3.1095609  0.5505563   5.648 1.62e-08 ***
    ## ProductionSeasonSummer -1.3470726  0.1067357 -12.621  < 2e-16 ***
    ## ProductionSeasonWinter  0.2188708  0.1174294   1.864   0.0623 .  
    ## ProductionSeasonFall   -0.9315550  0.1269427  -7.338 2.16e-13 ***
    ## NeosporaResultPositive -0.5724924  0.7215314  -0.793   0.4275    
    ## Site6                   1.1484053  0.4808750   2.388   0.0169 *  
    ## Site8                   0.8216096  0.4595656   1.788   0.0738 .  
    ## Age                    -0.0002424  0.0002934  -0.826   0.4087    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrdcSS PrdcSW PrdcSF NsprRP Site6  Site8 
    ## PrdctnSsnSm  0.060                                          
    ## PrdctnSsnWn -0.248  0.292                                   
    ## PrdctnSsnFl  0.196  0.704  0.179                            
    ## NsprRsltPst -0.068  0.001  0.010 -0.005                     
    ## Site6       -0.478 -0.053  0.060 -0.081 -0.018              
    ## Site8       -0.240  0.013 -0.021  0.031 -0.291  0.330       
    ## Age         -0.852 -0.215  0.208 -0.364  0.052  0.235 -0.062
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.227699 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
FULL4 <- glmer(
           MotilityPercentageFreshCat ~ SampleSeason + NeosporaResult + Site + Age + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final, family= "binomial"
              )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0115355 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
drop1(FULL4,test="Chisq")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.395502 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00506784 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00200295 (tol = 0.002, component 1)

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageFreshCat ~ SampleSeason + NeosporaResult + 
    ##     Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##                npar    AIC     LRT Pr(Chi)    
    ## <none>              6355.0                    
    ## SampleSeason      3 6523.1 174.111 < 2e-16 ***
    ## NeosporaResult    1 6353.7   0.684 0.40817    
    ## Site              2 6356.4   5.376 0.06801 .  
    ## Age               1 6358.5   5.471 0.01933 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary (FULL4)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: MotilityPercentageFreshCat ~ SampleSeason + NeosporaResult +  
    ##     Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6355.0   6434.8  -3166.5   6333.0    10464 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -13.5314   0.0924   0.1810   0.3334   3.2614 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.2071   1.7908       
    ##                       scaledTime  0.7189   0.8479   0.08
    ## Number of obs: 10475, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             2.6246401  0.5595685   4.690 2.73e-06 ***
    ## SampleSeasonSpring      1.4359123  0.1222661  11.744  < 2e-16 ***
    ## SampleSeasonSummer      0.2023746  0.0902731   2.242   0.0250 *  
    ## SampleSeasonWinter      0.8466471  0.1094326   7.737 1.02e-14 ***
    ## NeosporaResultPositive -0.6058365  0.7358971  -0.823   0.4104    
    ## Site6                   0.9632422  0.4863437   1.981   0.0476 *  
    ## Site8                   0.8886470  0.4668470   1.904   0.0570 .  
    ## Age                    -0.0006442  0.0002810  -2.293   0.0219 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) SmplSsnSp SmplSsnSm SmplSW NsprRP Site6  Site8 
    ## SmplSsnSprn -0.274                                                
    ## SmplSsnSmmr -0.223  0.507                                         
    ## SmplSsnWntr -0.267  0.524     0.497                               
    ## NsprRsltPst -0.080 -0.001     0.001     0.004                     
    ## Site6       -0.466  0.048     0.024     0.051 -0.013              
    ## Site8       -0.236 -0.014    -0.014    -0.013 -0.290  0.333       
    ## Age         -0.860  0.228     0.161     0.213  0.063  0.217 -0.060
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0115355 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
anova(FULL1, FULL2, FULL3, FULL4)
```

    ## Data: df_final
    ## Models:
    ## FULL1: MotilityPercentageFreshCat ~ ProductionMeanTHI + Site + NeosporaResult + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL2: MotilityPercentageFreshCat ~ SampleMaxTHI + NeosporaResult + Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL3: MotilityPercentageFreshCat ~ ProductionSeason + NeosporaResult + Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ## FULL4: MotilityPercentageFreshCat ~ SampleSeason + NeosporaResult + Site + Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##       npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## FULL1    9 6355.7 6421.0 -3168.8   6337.7                         
    ## FULL2    9 6468.8 6534.1 -3225.4   6450.8   0.00  0               
    ## FULL3   11 6335.4 6415.3 -3156.7   6313.4 137.39  2  < 2.2e-16 ***
    ## FULL4   11 6355.0 6434.8 -3166.5   6333.0   0.00  0               
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Final Model

``` r
FINAL <- glmer(
           MotilityPercentageFreshCat ~ ProductionMeanTHI +  Site + NeosporaResult + Age +(1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final, family= "binomial"
              )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00345814 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
summary (FINAL)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## MotilityPercentageFreshCat ~ ProductionMeanTHI + Site + NeosporaResult +  
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6355.7   6421.0  -3168.8   6337.7    10466 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -12.8964   0.0869   0.1800   0.3327   2.7459 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.4748   1.8641       
    ##                       scaledTime  0.9543   0.9769   0.03
    ## Number of obs: 10475, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             7.3497494  0.6195210  11.864   <2e-16 ***
    ## ProductionMeanTHI      -0.0625854  0.0048500 -12.904   <2e-16 ***
    ## Site6                   0.9249055  0.5095187   1.815   0.0695 .  
    ## Site8                   1.0290004  0.4881528   2.108   0.0350 *  
    ## NeosporaResultPositive -0.6081262  0.7667671  -0.793   0.4277    
    ## Age                    -0.0009634  0.0003035  -3.175   0.0015 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI Site6  Site8  NsprRP
    ## PrdctnMnTHI -0.344                            
    ## Site6       -0.427 -0.048                     
    ## Site8       -0.209 -0.009  0.326              
    ## NsprRsltPst -0.088 -0.002 -0.010 -0.292       
    ## Age         -0.756 -0.143  0.227 -0.069  0.076
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.00345814 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
drop1(FINAL,test="Chisq")
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00274474 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.00369871 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageFreshCat ~ ProductionMeanTHI + Site + NeosporaResult + 
    ##     Age + (1 + scaledTime | SiteAnimalIdentifier)
    ##                   npar    AIC     LRT   Pr(Chi)    
    ## <none>                 6355.7                      
    ## ProductionMeanTHI    1 6523.1 169.466 < 2.2e-16 ***
    ## Site                 2 6357.2   5.543 0.0625759 .  
    ## NeosporaResult       1 6354.3   0.634 0.4260624    
    ## Age                  1 6364.8  11.167 0.0008328 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vif.mer(FINAL)
```

    ##      ProductionMeanTHI                  Site6                  Site8 
    ##               1.021433               1.211819               1.258458 
    ## NeosporaResultPositive                    Age 
    ##               1.104324               1.102321

#### Final Model with interaction

Interaction Age\*ProductionTHI

``` r
FINAL1 <- glmer(
           MotilityPercentageFreshCat ~ ProductionMeanTHI*Age +  Site + NeosporaResult + (1 +  scaledTime| SiteAnimalIdentifier),
              data = df_final, family= "binomial"
              )
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
summary(FINAL1)
```

    ## Warning in vcov.merMod(object, use.hessian = use.hessian): variance-covariance matrix computed from finite-difference Hessian is
    ## not positive definite or contains NA values: falling back to var-cov estimated from RX

    ## Warning in vcov.merMod(object, correlation = correlation, sigm = sig): variance-covariance matrix computed from finite-difference Hessian is
    ## not positive definite or contains NA values: falling back to var-cov estimated from RX

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: MotilityPercentageFreshCat ~ ProductionMeanTHI * Age + Site +  
    ##     NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##    Data: df_final
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6349.0   6421.6  -3164.5   6329.0    10465 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -13.7395   0.0883   0.1782   0.3350   2.8444 
    ## 
    ## Random effects:
    ##  Groups               Name        Variance Std.Dev. Corr
    ##  SiteAnimalIdentifier (Intercept) 3.4234   1.8502       
    ##                       scaledTime  0.9568   0.9782   0.01
    ## Number of obs: 10475, groups:  SiteAnimalIdentifier, 105
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             9.293e+00  8.907e-01  10.433  < 2e-16 ***
    ## ProductionMeanTHI      -9.618e-02  1.242e-02  -7.741 9.86e-15 ***
    ## Age                    -2.176e-03  4.990e-04  -4.361 1.29e-05 ***
    ## Site6                   9.242e-01  5.083e-01   1.818  0.06906 .  
    ## Site8                   1.013e+00  4.872e-01   2.079  0.03766 *  
    ## NeosporaResultPositive -5.914e-01  7.612e-01  -0.777  0.43723    
    ## ProductionMeanTHI:Age   2.095e-05  7.071e-06   2.962  0.00306 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PrMTHI Age    Site6  Site8  NsprRP
    ## PrdctnMnTHI -0.791                                   
    ## Age         -0.890  0.734                            
    ## Site6       -0.289 -0.009  0.126                     
    ## Site8       -0.155 -0.005 -0.029  0.334              
    ## NsprRsltPst -0.051  0.002  0.033 -0.018 -0.288       
    ## PrdctMTHI:A  0.750 -0.920 -0.828 -0.005 -0.001  0.000
    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## unable to evaluate scaled gradient
    ## Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

``` r
drop1(FINAL1, test= "Chisq")
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.003296 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

    ## Single term deletions
    ## 
    ## Model:
    ## MotilityPercentageFreshCat ~ ProductionMeanTHI * Age + Site + 
    ##     NeosporaResult + (1 + scaledTime | SiteAnimalIdentifier)
    ##                       npar    AIC    LRT  Pr(Chi)   
    ## <none>                     6349.0                   
    ## Site                     2 6350.5 5.5221 0.063224 . 
    ## NeosporaResult           1 6347.6 0.6081 0.435488   
    ## ProductionMeanTHI:Age    1 6355.7 8.6548 0.003262 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
plot(FINAL1)
```

![](MotilityFresh_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
plot_model(FINAL1, type = "pred", terms = c("Age","ProductionMeanTHI"))
```

![](MotilityFresh_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
res=resid(FINAL1)
hist (res)
```

![](MotilityFresh_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
qqPlot (res)
```

![](MotilityFresh_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

    ## 7074 6356 
    ## 7073 6355

``` r
Anova(FINAL1) 
```

    ## Warning in vcov.merMod(mod, complete = FALSE): variance-covariance matrix computed from finite-difference Hessian is
    ## not positive definite or contains NA values: falling back to var-cov estimated from RX

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: MotilityPercentageFreshCat
    ##                          Chisq Df Pr(>Chisq)    
    ## ProductionMeanTHI     163.0105  1  < 2.2e-16 ***
    ## Age                    11.5874  1   0.000664 ***
    ## Site                    5.7413  2   0.056661 .  
    ## NeosporaResult          0.6035  1   0.437227    
    ## ProductionMeanTHI:Age   8.7737  1   0.003056 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
