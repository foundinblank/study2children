Basic Eye Gaze (study2children)
================
Adam Stone, PhD
10-30-2017

-   [Starting Out](#starting-out)
-   [Statistical Testing of FCR/mFCR](#statistical-testing-of-fcrmfcr)
    -   [Language, Direction, & Age Predictors](#language-direction-age-predictors)
    -   [Language & Age Predictors](#language-age-predictors)
    -   [Direction & Language Predictors](#direction-language-predictors)
    -   [Language as only predictor](#language-as-only-predictor)
-   [Could Age be Polynomial?](#could-age-be-polynomial)
-   [How Young Can We Go?](#how-young-can-we-go)

Starting Out
============

We've cleaned up the data in [01importclean](01importclean.nb.html). So we're importing it here. Let's grab histograms of our kids and groups.

``` r
# Libraries
library(tidyverse)
library(feather)
library(scales)
library(stringr)
library(viridis)
library(lme4)
library(lmerTest)
library(RColorBrewer)
#library(cowplot)

# Import data that's been cleaned up from 01importclean
data <- read_feather("cleanedchildeyedata.feather")

data_ages <- data %>%
  select(participant, language, age, group) %>%
  distinct()

ggplot(data_ages, aes(x = age, fill = language)) + geom_histogram() + facet_grid(language ~ group) + scale_fill_brewer(palette = "Accent")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

First, let's do boxplots of each AOI across all stories and kids, grouping for forward vs. reversed.

``` r
# Boxplot!
ggplot(data, aes(x = aoi, y = percent, fill = direction)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  ggtitle("Looking Percentages for Each AOI, All Stories") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

Looks like by far most of the activity is along the Mid\*\* AOIs. Let's look closer.

``` r
data_mid <- data %>%
  filter(str_detect(aoi,"Mid"))

ggplot(data_mid, aes(x = aoi, y = percent, fill = direction)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  ggtitle("Looking Percentages for Middle AOIs, All Stories") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

As a preliminary analysis I'm going to just look at MidChestTop and MidFaceBottom. Are there differences based on direction, group, or age for either AOI?

``` r
data_mid %>% 
  filter(aoi == "MidChestTop" | aoi == "MidFaceBottom") %>%
  ggplot(aes(x = age, y = percent, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + facet_grid(language ~ aoi) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ggtitle("Top 2 AOIs")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

I am not observing big differences for directin. But I sort of notice that CODAs show increased looking at MidFaceBottom as they get older, while English kids don't (and actually show a downward trajectory).

What if we defined a Face-Chest Ratio (FCR) such that:

1.  MidFaceTop, MidFaceCenter, MidFaceBottom = Face
2.  MidChestTop, MidChestCenter, MidChestBottom = Chest
3.  FCR = face - chest / face + chest

Let's try that. Let's also try only MidFaceBottom vs. MidChestTop too, and call that MFCR (for middle of middle).

Ooh, cool chart below. Looks like both effects of direction, language, and age. Signing kids, right away, have higher FCR (positive) while English kids have just slightly negative FCR. And signing kids' behavior doesn't really change depending on direction or age. English kids, however, show a big change based on direction, and on age.

``` r
data_mid <- data_mid %>%
  select(-secs, -hits) %>%
  spread(aoi,percent) %>%
  group_by(participant, trial) %>%
  mutate(face = sum(MidFaceTop, MidFaceCenter, MidFaceBottom, na.rm = TRUE),
         chest = sum(MidChestTop, MidChestCenter, MidChestBottom, na.rm = TRUE),
         fcr = (face - chest) / (face + chest),
         mfcr = (MidFaceBottom - MidChestTop) / (MidFaceBottom + MidChestTop))

ggplot(data_mid, aes(x = age, y = fcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + facet_wrap("language") + ggtitle("FaceChest Ratio")
```

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Let's also check out MFCR (a stricter ratio). Almost the same thing.

``` r
ggplot(data_mid, aes(x = age, y = mfcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + facet_wrap("language") + ggtitle("Middle-Middle FaceChest Ratio")
```

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

So this is rad! Such obvious group differences should also be reflected in a heat map that is collapsed across age and direction. And the heat map below shows that CODAs really focus on the MidFaceBottom AOI, while English-exposed children are a bit more spread out, looking much more at the chest.

``` r
data_mid_heat <- data_mid %>%
  ungroup() %>%
  select(-face, -chest, -fcr, -mfcr) %>%
  gather(aoi, percent, MidChestBottom:MidFaceTop) %>%
  group_by(language, participant, direction, aoi) %>%
  summarise(percent = mean(percent, na.rm = TRUE)) %>%
  group_by(language, direction, aoi) %>%
  summarise(percent = mean(percent, na.rm = TRUE)) %>%
  group_by(language, aoi) %>%
  summarise(percent = mean(percent, na.rm = TRUE)) %>%
  mutate(aoi = factor(aoi, levels = c("MidChestBottom", "MidChestCenter", "MidChestTop",
                                      "MidFaceBottom", "MidFaceCenter", "MidFaceTop")))

ggplot(data_mid_heat, aes(x = language, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

Statistical Testing of FCR/mFCR
===============================

Now that we've found something interesting with FCR and mFCR, let's see if it holds up in stats modeling.

Language, Direction, & Age Predictors
-------------------------------------

First, a LMM with predictors Language, Direction, and Age, and outcome variable FCR. Nothing significant, or even close to it! I'm surprised.

``` r
fcr_lmm <- lmer(fcr ~ age * language * direction + (1|story) + (1|participant), data = data_mid)
summary(fcr_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: 
    ## fcr ~ age * language * direction + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 659.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0376 -0.5649  0.1092  0.6451  2.2577 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.15116  0.3888  
    ##  story       (Intercept) 0.01617  0.1272  
    ##  Residual                0.23600  0.4858  
    ## Number of obs: 404, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                         -0.24190    0.42603  32.60000  -0.568
    ## age                                  0.03489    0.08106  32.30000   0.430
    ## languagesign                         0.60052    0.62517  31.70000   0.961
    ## directionreversed                    0.26791    0.26636 370.60000   1.006
    ## age:languagesign                    -0.02423    0.11869  31.70000  -0.204
    ## age:directionreversed               -0.05633    0.05262 361.60000  -1.070
    ## languagesign:directionreversed      -0.51473    0.38663 366.60000  -1.331
    ## age:languagesign:directionreversed   0.08331    0.07413 370.50000   1.124
    ##                                    Pr(>|t|)
    ## (Intercept)                           0.574
    ## age                                   0.670
    ## languagesign                          0.344
    ## directionreversed                     0.315
    ## age:languagesign                      0.840
    ## age:directionreversed                 0.285
    ## languagesign:directionreversed        0.184
    ## age:languagesign:directionreversed    0.262
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.956                                          
    ## languagesgn -0.672  0.649                                   
    ## dirctnrvrsd -0.311  0.309  0.206                            
    ## age:lnggsgn  0.652 -0.680 -0.967 -0.206                     
    ## ag:drctnrvr  0.298 -0.322 -0.193 -0.959  0.212              
    ## lnggsgn:drc  0.210 -0.206 -0.308 -0.674  0.300  0.639       
    ## ag:lnggsgn: -0.208  0.223  0.296  0.670 -0.310 -0.694 -0.963

Let's repeat the LMM with outcome mFCR. Same thing, although the t values are slightly better.

``` r
mfcr_lmm <- lmer(mfcr ~ age * language * direction + (1|story) + (1|participant), data = data_mid)
summary(mfcr_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: 
    ## mfcr ~ age * language * direction + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 659.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0596 -0.5318  0.1206  0.6725  2.3830 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.15546  0.3943  
    ##  story       (Intercept) 0.01002  0.1001  
    ##  Residual                0.24173  0.4917  
    ## Number of obs: 399, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                         -0.18306    0.43076  32.20000  -0.425
    ## age                                  0.04023    0.08210  32.10000   0.490
    ## languagesign                         0.72851    0.63377  31.70000   1.149
    ## directionreversed                    0.34340    0.26880 364.80000   1.278
    ## age:languagesign                    -0.05579    0.12030  31.70000  -0.464
    ## age:directionreversed               -0.06633    0.05291 341.40000  -1.254
    ## languagesign:directionreversed      -0.63844    0.39141 362.30000  -1.631
    ## age:languagesign:directionreversed   0.10090    0.07497 366.30000   1.346
    ##                                    Pr(>|t|)
    ## (Intercept)                           0.674
    ## age                                   0.627
    ## languagesign                          0.259
    ## directionreversed                     0.202
    ## age:languagesign                      0.646
    ## age:directionreversed                 0.211
    ## languagesign:directionreversed        0.104
    ## age:languagesign:directionreversed    0.179
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.959                                          
    ## languagesgn -0.673  0.649                                   
    ## dirctnrvrsd -0.310  0.307  0.205                            
    ## age:lnggsgn  0.653 -0.680 -0.967 -0.205                     
    ## ag:drctnrvr  0.297 -0.318 -0.193 -0.959  0.211              
    ## lnggsgn:drc  0.210 -0.205 -0.308 -0.674  0.299  0.640       
    ## ag:lnggsgn: -0.207  0.220  0.295  0.668 -0.309 -0.692 -0.963

What if we did ANCOVAs (C because age would be a covariate). First, FCR as outcome.

Language & Age Predictors
-------------------------

Let's take out direction. The FCR LMM tells us same thing, nothing.

``` r
fcr_lmm_nodir <- lmer(fcr ~ age * language + (1|story) + (1|participant) + (1|direction), data = data_mid)
summary(fcr_lmm_nodir)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ age * language + (1 | story) + (1 | participant) + (1 |  
    ##     direction)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 648.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -3.14998 -0.54654  0.09965  0.64497  2.25547 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.150600 0.38807 
    ##  story       (Intercept) 0.016287 0.12762 
    ##  direction   (Intercept) 0.000753 0.02744 
    ##  Residual                0.235657 0.48545 
    ## Number of obs: 404, groups:  participant, 30; story, 8; direction, 2
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error        df t value Pr(>|t|)
    ## (Intercept)      -0.108697   0.404621 26.749000  -0.269    0.790
    ## age               0.006949   0.076621 26.025000   0.091    0.928
    ## languagesign      0.346048   0.593658 26.016000   0.583    0.565
    ## age:languagesign  0.016855   0.112629 25.976000   0.150    0.882
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg
    ## age         -0.955              
    ## languagesgn -0.671  0.651       
    ## age:lnggsgn  0.650 -0.680 -0.967

And the mFCR, nothing either.

``` r
mfcr_lmm_nodir <- lmer(mfcr ~ age * language + (1|story) + (1|participant) + (1|direction), data = data_mid)
summary(mfcr_lmm_nodir)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: mfcr ~ age * language + (1 | story) + (1 | participant) + (1 |  
    ##     direction)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 649.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1864 -0.5490  0.1278  0.6472  2.3849 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance  Std.Dev.
    ##  participant (Intercept) 0.1548546 0.39352 
    ##  story       (Intercept) 0.0097690 0.09884 
    ##  direction   (Intercept) 0.0002963 0.01721 
    ##  Residual                0.2423653 0.49231 
    ## Number of obs: 399, groups:  participant, 30; story, 8; direction, 2
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error        df t value Pr(>|t|)
    ## (Intercept)      -0.012274   0.408957 26.419000  -0.030    0.976
    ## age               0.007356   0.077707 26.038000   0.095    0.925
    ## languagesign      0.411420   0.602027 26.020000   0.683    0.500
    ## age:languagesign -0.005789   0.114225 25.987000  -0.051    0.960
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg
    ## age         -0.958              
    ## languagesgn -0.674  0.651       
    ## age:lnggsgn  0.652 -0.680 -0.967

Direction & Language Predictors
-------------------------------

In the LMMs we've run so far, age is always the worst predictor. So we should have taken that out first, anyway.

Alright, we got a very significant effect of language (p = 0.003). Signing kids have a much higher FCR than non-signing kids.

``` r
fcr_lmm_noage <- lmer(fcr ~ direction * language + (1|story) + (1|participant), data = data_mid)
summary(fcr_lmm_noage)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ direction * language + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 646.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0681 -0.5849  0.1025  0.6381  2.2422 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.14087  0.3753  
    ##  story       (Intercept) 0.01691  0.1300  
    ##  Residual                0.23534  0.4851  
    ## Number of obs: 404, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##                                  Estimate Std. Error         df t value
    ## (Intercept)                     -0.070530   0.121760  38.600000  -0.579
    ## directionreversed               -0.006493   0.075027 363.300000  -0.087
    ## languagesign                     0.484699   0.154673  35.400000   3.134
    ## directionreversed:languagesign  -0.102347   0.102928 368.400000  -0.994
    ##                                Pr(>|t|)   
    ## (Intercept)                     0.56579   
    ## directionreversed               0.93108   
    ## languagesign                    0.00346 **
    ## directionreversed:languagesign  0.32070   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.308              
    ## languagesgn -0.678  0.252       
    ## drctnrvrsd:  0.232 -0.753 -0.333

The mFCR LMM gives us similar results - strong effect of language (p = 0.006).

``` r
mfcr_lmm_noage <- lmer(mfcr ~ direction * language + (1|story) + (1|participant), data = data_mid)
summary(mfcr_lmm_noage)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: mfcr ~ direction * language + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 647
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0870 -0.5615  0.1239  0.6630  2.3605 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.14449  0.3801  
    ##  story       (Intercept) 0.01045  0.1022  
    ##  Residual                0.24146  0.4914  
    ## Number of obs: 399, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error        df t value
    ## (Intercept)                      0.01533    0.11966  37.70000   0.128
    ## directionreversed                0.01898    0.07582 342.20000   0.250
    ## languagesign                     0.45073    0.15659  35.30000   2.878
    ## directionreversed:languagesign  -0.13723    0.10427 350.50000  -1.316
    ##                                Pr(>|t|)   
    ## (Intercept)                     0.89873   
    ## directionreversed               0.80247   
    ## languagesign                    0.00674 **
    ## directionreversed:languagesign  0.18901   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.312              
    ## languagesgn -0.697  0.247       
    ## drctnrvrsd:  0.234 -0.749 -0.331

Language as only predictor
--------------------------

Same thing here. So it's not age or direction, but language, that has the effect. And that's good! I want to look at forward v. reversed separately next.

``` r
fcr_lmm_langonly <- lmer(fcr ~  language + (1|story) + (1|participant) + (1|direction), data = data_mid)
summary(fcr_lmm_langonly)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: 
    ## fcr ~ language + (1 | story) + (1 | participant) + (1 | direction)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 642.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -3.15166 -0.55183  0.09935  0.64648  2.24817 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance  Std.Dev.
    ##  participant (Intercept) 0.1391768 0.37306 
    ##  story       (Intercept) 0.0163083 0.12770 
    ##  direction   (Intercept) 0.0007472 0.02734 
    ##  Residual                0.2356472 0.48544 
    ## Number of obs: 404, groups:  participant, 30; story, 8; direction, 2
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error       df t value Pr(>|t|)   
    ## (Intercept)  -0.07375    0.11663 29.06900  -0.632  0.53212   
    ## languagesign  0.43350    0.14505 28.06200   2.989  0.00577 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.661

Forward only. Language effect, p = 0.004.

``` r
fcr_lmm_langonly_f <- lmer(fcr ~  language + (1|story) + (1|participant), data = filter(data_mid,direction=="forward"))
summary(fcr_lmm_langonly_f)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ language + (1 | story) + (1 | participant)
    ##    Data: filter(data_mid, direction == "forward")
    ## 
    ## REML criterion at convergence: 319.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.86405 -0.55118  0.05384  0.65064  2.10862 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.1335   0.3654  
    ##  story       (Intercept) 0.0359   0.1895  
    ##  Residual                0.2064   0.4543  
    ## Number of obs: 203, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error       df t value Pr(>|t|)   
    ## (Intercept)  -0.06071    0.13078 26.36700  -0.464  0.64630   
    ## languagesign  0.46706    0.15325 29.54600   3.048  0.00482 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.640

Reversed only. Weaker language effect, p = 0.0131.

``` r
fcr_lmm_langonly_r <- lmer(fcr ~  language + (1|story) + (1|participant), data = filter(data_mid,direction=="reversed"))
summary(fcr_lmm_langonly_r)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ language + (1 | story) + (1 | participant)
    ##    Data: filter(data_mid, direction == "reversed")
    ## 
    ## REML criterion at convergence: 351.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8838 -0.6107  0.1776  0.6633  2.2985 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.13867  0.3724  
    ##  story       (Intercept) 0.01621  0.1273  
    ##  Residual                0.25716  0.5071  
    ## Number of obs: 201, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error       df t value Pr(>|t|)  
    ## (Intercept)  -0.09902    0.12328 28.05200  -0.803    0.429  
    ## languagesign  0.41026    0.15697 29.46300   2.614    0.014 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.686

Let's go ahead and plot boxplots to represent LMMs with only language as an important effect. (We can add in direction too). FaceChest Ratio here.

``` r
ggplot(data_mid, aes(x = language, y = fcr, fill = language)) + geom_boxplot() + scale_fill_brewer(palette = "Dark2") + ylab("FaceChest Ratio") + ggtitle("FaceChest Ratio by Language")
```

    ## Warning: Removed 1 rows containing non-finite values (stat_boxplot).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

``` r
ggplot(data_mid, aes(x = language, y = fcr, fill = direction)) + geom_boxplot() + ylab("FaceChest Ratio") + ggtitle("FaceChest Ratio by Language & Direction")
```

    ## Warning: Removed 1 rows containing non-finite values (stat_boxplot).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-2.png)

And Middle-Middle FaceChest Ratio here.

``` r
ggplot(data_mid, aes(x = language, y = mfcr, fill = language)) + geom_boxplot() + scale_fill_brewer(palette = "Dark2") + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language")
```

    ## Warning: Removed 6 rows containing non-finite values (stat_boxplot).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png)

``` r
ggplot(data_mid, aes(x = language, y = mfcr, fill = direction)) + geom_boxplot() + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language & Direction")
```

    ## Warning: Removed 6 rows containing non-finite values (stat_boxplot).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-2.png)

Could Age be Polynomial?
========================

I graphed age using a polynomial scale (^2, parabolic) and there definitely seems to be something there. I'm not sure how to work with these, though, so putting it off for a bit later. See below:

``` r
ggplot(data_mid, aes(x = age, y = fcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", span = 5) + facet_wrap("language") + ggtitle("FaceChest Ratio")
```

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-19-1.png)

``` r
ggplot(data_mid, aes(x = age, y = mfcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", span = 5) + facet_wrap("language") + ggtitle("MFaceChest Ratio")
```

    ## Warning: Removed 6 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-19-2.png)

How Young Can We Go?
====================

How young can we still find differences between NSE and CODA kids? If we look only at 2 to 4.9 year old kids then the language effect gets weaker (p = 0.06) but then again it's only 12 kids total. (If I lower the cutoff to 4.5, it's 0.05, but 10 kids; if it's 4, it's 0.60, but only 6 kids).

``` r
data_young <- data_mid %>%
  ungroup() %>%
  filter(age < 4)
data_young %>% select(participant, language) %>% distinct() %>% count(language)
```

    ## # A tibble: 2 x 2
    ##   language     n
    ##     <fctr> <int>
    ## 1  english     3
    ## 2     sign     4

``` r
fcr_lmm_young <- lmer(fcr ~ direction * language + (1|story) + (1|participant), data = data_young)
summary(fcr_lmm_young)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ direction * language + (1 | story) + (1 | participant)
    ##    Data: data_young
    ## 
    ## REML criterion at convergence: 142.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8017 -0.5142  0.2239  0.6646  1.3741 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  story       (Intercept) 0.008679 0.09316 
    ##  participant (Intercept) 0.049877 0.22333 
    ##  Residual                0.219074 0.46805 
    ## Number of obs: 95, groups:  story, 8; participant, 7
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error      df t value
    ## (Intercept)                      0.2869     0.1696  8.2100   1.691
    ## directionreversed                0.1201     0.1493 82.8100   0.805
    ## languagesign                     0.1687     0.2192  7.7400   0.769
    ## directionreversed:languagesign  -0.3245     0.1960 80.9100  -1.655
    ##                                Pr(>|t|)
    ## (Intercept)                       0.128
    ## directionreversed                 0.423
    ## languagesign                      0.464
    ## directionreversed:languagesign    0.102
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.437              
    ## languagesgn -0.741  0.331       
    ## drctnrvrsd:  0.326 -0.746 -0.441
