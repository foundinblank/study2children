Basic Eye Gaze (study2children)
================
Adam Stone, PhD
11-26-2017

-   [Starting Out](#starting-out)
-   [Heat Maps](#heat-maps)
-   [Statistical Testing of FCR/mFCR](#statistical-testing-of-fcrmfcr)
    -   [Language, Direction, & Age Predictors](#language-direction-age-predictors)
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
  filter(str_detect(aoi,"Mid") | aoi == "BelowChest")

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

1.  MidFaceCenter, MidFaceBottom = Face
2.  MidChestTop, MidChestCenter, MidChestBottom, BelowChest = Chest
3.  FCR = face - chest / face + chest

Let's try that. Let's also try only MidFaceBottom vs. MidChestTop too, and call that MFCR (for middle of middle).

Ooh, cool chart below. Looks like both effects of direction, language, and age. Signing kids, right away, have higher FCR (positive) while English kids have just slightly negative FCR. And signing kids' behavior doesn't really change depending on direction or age. English kids, however, show a big change based on direction, and on age.

``` r
data_mid <- data_mid %>%
  select(-secs, -hits) %>%
  spread(aoi,percent) %>%
  group_by(participant, trial) %>%
  mutate(face = sum(MidFaceCenter, MidFaceBottom, na.rm = TRUE),
         chest = sum(MidChestTop, MidChestCenter, MidChestBottom, BelowChest, na.rm = TRUE),
         fcr = (face - chest) / (face + chest),
         mfcr = (MidFaceBottom - MidChestTop) / (MidFaceBottom + MidChestTop))

ggplot(data_mid, aes(x = age, y = fcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + facet_wrap("language") + ggtitle("FaceChest Ratio")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Let's also check out MFCR (a stricter ratio). Almost the same thing.

``` r
ggplot(data_mid, aes(x = age, y = mfcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + facet_wrap("language") + ggtitle("Middle-Middle FaceChest Ratio")
```

    ## Warning: Removed 10 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 10 rows containing missing values (geom_point).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

Heat Maps
=========

So this is rad! Such obvious group differences should also be reflected in a heat map that is collapsed across age and direction. And the heat map below shows that CODAs really focus on the MidFaceBottom AOI, while English-exposed children are a bit more spread out, looking much more at the chest.

``` r
data_mid_heat <- data_mid %>%
  ungroup() %>%
  select(-face, -chest, -fcr, -mfcr) %>%
  gather(aoi, percent, BelowChest:MidFaceTop) %>%
  group_by(language, participant, direction, aoi) %>%
  summarise(percent = mean(percent, na.rm = TRUE)) %>%
  group_by(language, direction, aoi) %>%
  summarise(percent = mean(percent, na.rm = TRUE)) %>%
  group_by(language, aoi) %>%
  summarise(percent = mean(percent, na.rm = TRUE)) %>%
  mutate(aoi = factor(aoi, levels = c("BelowChest", "MidChestBottom", "MidChestCenter", "MidChestTop",
                                      "MidFaceBottom", "MidFaceCenter", "MidFaceTop")))

ggplot(data_mid_heat, aes(x = language, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,.5)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

``` r
data_mid_heat <- data_mid %>%
  ungroup() %>%
  select(-face, -chest, -fcr, -mfcr) %>%
  gather(aoi, percent, BelowChest:MidFaceTop) %>%
  group_by(language, participant, direction, aoi) %>%
  summarise(percent = mean(percent, na.rm = TRUE)) %>%
  group_by(language, direction, aoi) %>%
  summarise(percent = mean(percent, na.rm = TRUE)) %>%
  mutate(aoi = factor(aoi, levels = c("BelowChest", "MidChestBottom", "MidChestCenter", "MidChestTop",
                                      "MidFaceBottom", "MidFaceCenter", "MidFaceTop")))

ggplot(data_mid_heat, aes(x = language, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,.5)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map by Direction") + facet_wrap("direction")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

``` r
ggplot(data_mid_heat, aes(x = direction, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,.5)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map by Group") + facet_wrap("language")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-2.png)

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
    ## REML criterion at convergence: 642.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1809 -0.5136  0.1401  0.6797  2.2455 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.17581  0.4193  
    ##  story       (Intercept) 0.01855  0.1362  
    ##  Residual                0.21907  0.4680  
    ## Number of obs: 408, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                         -0.13373    0.44072  31.80000  -0.303
    ## age                                  0.01382    0.08478  31.40000   0.163
    ## languagesign                         0.51908    0.65578  30.80000   0.792
    ## directionreversed                    0.20690    0.25417 374.30000   0.814
    ## age:languagesign                    -0.01031    0.12493  30.80000  -0.083
    ## age:directionreversed               -0.04837    0.05082 366.20000  -0.952
    ## languagesign:directionreversed      -0.45169    0.37096 371.60000  -1.218
    ## age:languagesign:directionreversed   0.07168    0.07147 374.50000   1.003
    ##                                    Pr(>|t|)
    ## (Intercept)                           0.764
    ## age                                   0.872
    ## languagesign                          0.435
    ## directionreversed                     0.416
    ## age:languagesign                      0.935
    ## age:directionreversed                 0.342
    ## languagesign:directionreversed        0.224
    ## age:languagesign:directionreversed    0.317
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.957                                          
    ## languagesgn -0.662  0.640                                   
    ## dirctnrvrsd -0.288  0.287  0.187                            
    ## age:lnggsgn  0.648 -0.676 -0.967 -0.189                     
    ## ag:drctnrvr  0.276 -0.297 -0.176 -0.963  0.194              
    ## lnggsgn:drc  0.193 -0.191 -0.282 -0.669  0.275  0.637       
    ## ag:lnggsgn: -0.193  0.206  0.271  0.672 -0.284 -0.694 -0.964

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
    ## REML criterion at convergence: 645.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1964 -0.4779  0.1024  0.6187  1.9208 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.17843  0.4224  
    ##  story       (Intercept) 0.01012  0.1006  
    ##  Residual                0.23126  0.4809  
    ## Number of obs: 398, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                         -0.13858    0.44396  31.10000  -0.312
    ## age                                  0.02689    0.08561  30.90000   0.314
    ## languagesign                         0.71795    0.66274  30.50000   1.083
    ## directionreversed                    0.31268    0.25969 359.40000   1.204
    ## age:languagesign                    -0.04705    0.12623  30.50000  -0.373
    ## age:directionreversed               -0.05796    0.05172 333.20000  -1.121
    ## languagesign:directionreversed      -0.58304    0.38103 362.50000  -1.530
    ## age:languagesign:directionreversed   0.08643    0.07330 364.80000   1.179
    ##                                    Pr(>|t|)
    ## (Intercept)                           0.757
    ## age                                   0.756
    ## languagesign                          0.287
    ## directionreversed                     0.229
    ## age:languagesign                      0.712
    ## age:directionreversed                 0.263
    ## languagesign:directionreversed        0.127
    ## age:languagesign:directionreversed    0.239
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.960                                          
    ## languagesgn -0.664  0.640                                   
    ## dirctnrvrsd -0.292  0.289  0.190                            
    ## age:lnggsgn  0.649 -0.676 -0.967 -0.191                     
    ## ag:drctnrvr  0.279 -0.298 -0.179 -0.962  0.196              
    ## lnggsgn:drc  0.195 -0.192 -0.286 -0.668  0.279  0.637       
    ## ag:lnggsgn: -0.194  0.207  0.275  0.668 -0.288 -0.692 -0.964

What if we did ANCOVAs (C because age would be a covariate). First, FCR as outcome.

Direction & Language Predictors
-------------------------------

In the LMMs we've run so far, age is always the worst predictor. So we should have taken that out first, anyway.

**OLD TEXT** Alright, we got a very significant effect of language (p = 0.003). Signing kids have a much higher FCR than non-signing kids.

``` r
fcr_lmm_noage <- lmer(fcr ~ direction * language + (1|story) + (1|participant), data = data_mid)
summary(fcr_lmm_noage)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ direction * language + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 629.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2089 -0.5303  0.1000  0.6962  2.2331 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.16357  0.4044  
    ##  story       (Intercept) 0.01879  0.1371  
    ##  Residual                0.21841  0.4673  
    ## Number of obs: 408, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error        df t value
    ## (Intercept)                     -0.06831    0.12462  38.00000  -0.548
    ## directionreversed               -0.02677    0.06853 376.30000  -0.391
    ## languagesign                     0.47227    0.16241  33.90000   2.908
    ## directionreversed:languagesign  -0.09830    0.09668 377.40000  -1.017
    ##                                Pr(>|t|)   
    ## (Intercept)                     0.58680   
    ## directionreversed               0.69625   
    ## languagesign                    0.00638 **
    ## directionreversed:languagesign  0.30996   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.278              
    ## languagesgn -0.653  0.219       
    ## drctnrvrsd:  0.201 -0.724 -0.300

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
    ## REML criterion at convergence: 632.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2227 -0.5211  0.1160  0.6426  1.9536 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.16552  0.4068  
    ##  story       (Intercept) 0.01036  0.1018  
    ##  Residual                0.23082  0.4804  
    ## Number of obs: 398, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##                                  Estimate Std. Error         df t value
    ## (Intercept)                     -0.007933   0.121395  36.400000  -0.065
    ## directionreversed                0.032190   0.071087 362.700000   0.453
    ## languagesign                     0.483829   0.164045  33.600000   2.949
    ## directionreversed:languagesign  -0.156332   0.100129 366.000000  -1.561
    ##                                Pr(>|t|)   
    ## (Intercept)                     0.94826   
    ## directionreversed               0.65094   
    ## languagesign                    0.00576 **
    ## directionreversed:languagesign  0.11932   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.288              
    ## languagesgn -0.677  0.218       
    ## drctnrvrsd:  0.208 -0.723 -0.302

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
    ## REML criterion at convergence: 625.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2895 -0.5556  0.0786  0.6862  2.2458 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.162229 0.40278 
    ##  story       (Intercept) 0.018385 0.13559 
    ##  direction   (Intercept) 0.001862 0.04315 
    ##  Residual                0.218626 0.46757 
    ## Number of obs: 408, groups:  participant, 30; story, 8; direction, 2
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error       df t value Pr(>|t|)  
    ## (Intercept)  -0.08153    0.12298 27.27900  -0.663   0.5129  
    ## languagesign  0.42305    0.15438 28.13900   2.740   0.0105 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.627

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
    ## REML criterion at convergence: 313.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.84834 -0.48608  0.01798  0.61646  2.19251 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.16533  0.4066  
    ##  story       (Intercept) 0.04115  0.2029  
    ##  Residual                0.19167  0.4378  
    ## Number of obs: 204, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error       df t value Pr(>|t|)  
    ## (Intercept)  -0.05162    0.13698 27.32300  -0.377   0.7092  
    ## languagesign  0.44593    0.16398 28.97600   2.719   0.0109 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.611

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
    ## REML criterion at convergence: 347.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8459 -0.5282  0.1570  0.7121  2.1623 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.15060  0.3881  
    ##  story       (Intercept) 0.01419  0.1191  
    ##  Residual                0.24269  0.4926  
    ## Number of obs: 204, groups:  participant, 30; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error      df t value Pr(>|t|)  
    ## (Intercept)   -0.1058     0.1205 28.7850  -0.878   0.3872  
    ## languagesign   0.3903     0.1594 29.0120   2.448   0.0207 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.666

Let's go ahead and plot boxplots to represent LMMs with only language as an important effect. (We can add in direction too). FaceChest Ratio here.

``` r
ggplot(data_mid, aes(x = language, y = fcr, fill = language)) + geom_boxplot() + scale_fill_brewer(palette = "Dark2") + ylab("FaceChest Ratio") + ggtitle("FaceChest Ratio by Language")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

``` r
ggplot(data_mid, aes(x = language, y = fcr, fill = direction)) + geom_boxplot() + ylab("FaceChest Ratio") + ggtitle("FaceChest Ratio by Language & Direction")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-2.png)

And Middle-Middle FaceChest Ratio here.

``` r
ggplot(data_mid, aes(x = language, y = mfcr, fill = language)) + geom_boxplot() + scale_fill_brewer(palette = "Dark2") + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language")
```

    ## Warning: Removed 10 rows containing non-finite values (stat_boxplot).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

``` r
ggplot(data_mid, aes(x = language, y = mfcr, fill = direction)) + geom_boxplot() + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language & Direction")
```

    ## Warning: Removed 10 rows containing non-finite values (stat_boxplot).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-2.png)

Could Age be Polynomial?
========================

I graphed age using a polynomial scale (^2, parabolic) and there definitely seems to be something there. I'm not sure how to work with these, though, so putting it off for a bit later. See below:

``` r
ggplot(data_mid, aes(x = age, y = fcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", span = 5) + facet_wrap("language") + ggtitle("FaceChest Ratio")
```

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png)

``` r
ggplot(data_mid, aes(x = age, y = mfcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", span = 5) + facet_wrap("language") + ggtitle("MFaceChest Ratio")
```

    ## Warning: Removed 10 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 10 rows containing missing values (geom_point).

![](02basiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-2.png)

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
    ## REML criterion at convergence: 137.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8218 -0.4808  0.2176  0.6824  1.3643 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  story       (Intercept) 0.008234 0.09074 
    ##  participant (Intercept) 0.036824 0.19189 
    ##  Residual                0.209272 0.45746 
    ## Number of obs: 95, groups:  story, 8; participant, 7
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error      df t value
    ## (Intercept)                      0.2497     0.1545  8.9900   1.616
    ## directionreversed                0.0747     0.1459 83.1600   0.512
    ## languagesign                     0.2152     0.1990  8.4100   1.081
    ## directionreversed:languagesign  -0.2568     0.1915 81.4800  -1.341
    ##                                Pr(>|t|)
    ## (Intercept)                       0.141
    ## directionreversed                 0.610
    ## languagesign                      0.310
    ## directionreversed:languagesign    0.184
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.469              
    ## languagesgn -0.739  0.356       
    ## drctnrvrsd:  0.350 -0.747 -0.475
