Basic Eye Gaze (study2children)
================
Adam Stone, PhD
11-18-2017

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
    ## REML criterion at convergence: 616.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1968 -0.5084  0.1077  0.6641  2.2523 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.16733  0.4091  
    ##  story       (Intercept) 0.01751  0.1323  
    ##  Residual                0.21735  0.4662  
    ## Number of obs: 394, groups:  participant, 29; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                         -0.28044    0.44099  30.50000  -0.636
    ## age                                  0.03286    0.08388  30.20000   0.392
    ## languagesign                         0.66924    0.64854  29.70000   1.032
    ## directionreversed                    0.23322    0.25604 361.40000   0.911
    ## age:languagesign                    -0.02988    0.12290  29.70000  -0.243
    ## age:directionreversed               -0.05073    0.05068 355.40000  -1.001
    ## languagesign:directionreversed      -0.48265    0.37216 357.50000  -1.297
    ## age:languagesign:directionreversed   0.07474    0.07128 361.10000   1.048
    ##                                    Pr(>|t|)
    ## (Intercept)                           0.530
    ## age                                   0.698
    ## languagesign                          0.310
    ## directionreversed                     0.363
    ## age:languagesign                      0.810
    ## age:directionreversed                 0.317
    ## languagesign:directionreversed        0.196
    ## age:languagesign:directionreversed    0.295
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.956                                          
    ## languagesgn -0.671  0.648                                   
    ## dirctnrvrsd -0.289  0.288  0.191                            
    ## age:lnggsgn  0.651 -0.680 -0.966 -0.192                     
    ## ag:drctnrvr  0.276 -0.299 -0.179 -0.959  0.197              
    ## lnggsgn:drc  0.195 -0.192 -0.286 -0.674  0.278  0.639       
    ## ag:lnggsgn: -0.194  0.208  0.274  0.671 -0.288 -0.695 -0.963

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
    ## REML criterion at convergence: 616.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2457 -0.4811  0.0973  0.6299  1.9439 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.18646  0.4318  
    ##  story       (Intercept) 0.01089  0.1043  
    ##  Residual                0.22575  0.4751  
    ## Number of obs: 384, groups:  participant, 29; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                         -0.16326    0.46161  29.50000  -0.354
    ## age                                  0.03027    0.08797  29.40000   0.344
    ## languagesign                         0.74548    0.68081  29.00000   1.095
    ## directionreversed                    0.30590    0.26027 350.90000   1.175
    ## age:languagesign                    -0.05087    0.12899  29.00000  -0.394
    ## age:directionreversed               -0.05713    0.05133 332.90000  -1.113
    ## languagesign:directionreversed      -0.58129    0.37949 347.90000  -1.532
    ## age:languagesign:directionreversed   0.08633    0.07261 351.70000   1.189
    ##                                    Pr(>|t|)
    ## (Intercept)                           0.726
    ## age                                   0.733
    ## languagesign                          0.283
    ## directionreversed                     0.241
    ## age:languagesign                      0.696
    ## age:directionreversed                 0.267
    ## languagesign:directionreversed        0.126
    ## age:languagesign:directionreversed    0.235
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.959                                          
    ## languagesgn -0.672  0.648                                   
    ## dirctnrvrsd -0.280  0.278  0.185                            
    ## age:lnggsgn  0.653 -0.680 -0.966 -0.185                     
    ## ag:drctnrvr  0.268 -0.288 -0.174 -0.958  0.190              
    ## lnggsgn:drc  0.189 -0.185 -0.277 -0.674  0.270  0.639       
    ## ag:lnggsgn: -0.187  0.200  0.266  0.669 -0.279 -0.693 -0.962

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
    ## REML criterion at convergence: 603.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2251 -0.5259  0.0876  0.6860  2.2425 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.15520  0.3940  
    ##  story       (Intercept) 0.01784  0.1336  
    ##  Residual                0.21672  0.4655  
    ## Number of obs: 394, groups:  participant, 29; story, 8
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error        df t value
    ## (Intercept)                     -0.11895    0.12556  37.10000  -0.947
    ## directionreversed               -0.01376    0.07223 357.10000  -0.190
    ## languagesign                     0.52353    0.16179  33.00000   3.236
    ## directionreversed:languagesign  -0.11226    0.09948 362.60000  -1.128
    ##                                Pr(>|t|)   
    ## (Intercept)                     0.34960   
    ## directionreversed               0.84906   
    ## languagesign                    0.00276 **
    ## directionreversed:languagesign  0.25987   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.287              
    ## languagesgn -0.668  0.230       
    ## drctnrvrsd:  0.214 -0.744 -0.308

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
    ## REML criterion at convergence: 603.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2711 -0.5204  0.1225  0.6437  1.9733 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.1725   0.4153  
    ##  story       (Intercept) 0.0111   0.1053  
    ##  Residual                0.2253   0.4747  
    ## Number of obs: 384, groups:  participant, 29; story, 8
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error        df t value
    ## (Intercept)                     -0.01466    0.12749  35.00000  -0.115
    ## directionreversed                0.02740    0.07428 337.10000   0.369
    ## languagesign                     0.49109    0.16960  32.20000   2.896
    ## directionreversed:languagesign  -0.15266    0.10219 347.20000  -1.494
    ##                                Pr(>|t|)   
    ## (Intercept)                     0.90911   
    ## directionreversed               0.71246   
    ## languagesign                    0.00675 **
    ## directionreversed:languagesign  0.13611   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.283              
    ## languagesgn -0.689  0.219       
    ## drctnrvrsd:  0.210 -0.743 -0.296

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
    ## REML criterion at convergence: 599.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3068 -0.5491  0.0693  0.6797  2.2590 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.153501 0.39179 
    ##  story       (Intercept) 0.017259 0.13137 
    ##  direction   (Intercept) 0.001605 0.04007 
    ##  Residual                0.217157 0.46600 
    ## Number of obs: 394, groups:  participant, 29; story, 8; direction, 2
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error      df t value Pr(>|t|)   
    ## (Intercept)   -0.1258     0.1228 27.1920  -1.025  0.31460   
    ## languagesign   0.4674     0.1532 27.1120   3.052  0.00505 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.644

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
    ## REML criterion at convergence: 305.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.83804 -0.48686  0.00356  0.62351  2.17000 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.15473  0.3934  
    ##  story       (Intercept) 0.03782  0.1945  
    ##  Residual                0.19459  0.4411  
    ## Number of obs: 198, groups:  participant, 29; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error      df t value Pr(>|t|)   
    ## (Intercept)   -0.1072     0.1372 26.9410  -0.782  0.44114   
    ## languagesign   0.5032     0.1635 28.4900   3.078  0.00458 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.634

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
    ## REML criterion at convergence: 327
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6902 -0.5529  0.1116  0.7156  2.2685 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.1459   0.3820  
    ##  story       (Intercept) 0.0183   0.1353  
    ##  Residual                0.2317   0.4814  
    ## Number of obs: 196, groups:  participant, 29; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error      df t value Pr(>|t|)  
    ## (Intercept)   -0.1510     0.1256 27.3960  -1.203   0.2392  
    ## languagesign   0.4357     0.1606 28.3090   2.714   0.0112 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.672

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
