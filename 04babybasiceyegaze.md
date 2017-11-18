Baby Basic Eye Gaze (study2children)
================
Adam Stone, PhD
11-18-2017

-   [Starting Out](#starting-out)
-   [Heat Maps](#heat-maps)
-   [Statistical Testing of FCR/mFCR](#statistical-testing-of-fcrmfcr)
    -   [Language, Direction, & Age Predictors](#language-direction-age-predictors)
    -   [Direction & Language Predictors](#direction-language-predictors)
    -   [Language as only predictor](#language-as-only-predictor)

Starting Out
============

We've cleaned up the data in [03importcleanbabies](03importcleanbabies.nb.html). So we're importing it here. Let's get a chart of how old our babies are.

``` r
# Libraries
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────────── tidyverse 1.2.0 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.3.4     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.7.2     ✔ stringr 1.2.0
    ## ✔ readr   1.1.1     ✔ forcats 0.2.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(feather)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(stringr)
library(viridis)
```

    ## Loading required package: viridisLite

    ## 
    ## Attaching package: 'viridis'

    ## The following object is masked from 'package:scales':
    ## 
    ##     viridis_pal

``` r
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

``` r
library(lmerTest)
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(RColorBrewer)
#library(cowplot)

# Import data that's been cleaned up from 01importclean
data <- read_feather("cleanedbabyeyedata.feather")

data_ages <- data %>%
  select(participant, language, age, group) %>%
  distinct()

ggplot(data_ages, aes(x = age, fill = language)) + geom_histogram() + facet_grid(language ~ group) + scale_fill_brewer(palette = "Accent")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

Next, let's do boxplots of each AOI across all stories and kids, grouping for forward vs. reversed.

``` r
# Boxplot!
ggplot(data, aes(x = aoi, y = percent, fill = direction)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  ggtitle("Looking Percentages for Each AOI, All Stories") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
```

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

Looks like by far most of the activity is along the Mid\*\* AOIs. Let's look closer.

``` r
data_mid <- data %>%
  filter(str_detect(aoi,"Mid") | aoi == "BelowChest")

ggplot(data_mid, aes(x = aoi, y = percent, fill = direction)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  ggtitle("Looking Percentages for Middle AOIs, All Stories") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
```

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

As a preliminary analysis I'm going to just look at MidChestTop and MidFaceBottom. Are there differences based on direction, group, or age for either AOI?

``` r
data_mid %>% 
  filter(aoi == "MidChestTop" | aoi == "MidFaceBottom") %>%
  ggplot(aes(x = age, y = percent, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + facet_grid(language ~ aoi) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ggtitle("Top 2 AOIs")
```

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

I am not observing big differences for direction. And not strong patterns across age, although there are subtle upward trends for increased chest looking in NSE kids as they get older, but increased face looking in CODAS as they get older.

What if we defined a Face-Chest Ratio (FCR) such that:

1.  MidFaceCenter, MidFaceBottom = Face
2.  MidChestTop, MidChestCenter, MidChestBottom, BelowChest = Chest
3.  FCR = face - chest / face + chest

Let's try that. Let's also try only MidFaceBottom vs. MidChestTop too, and call that MFCR (for middle of middle).

CHart belows suggests group differences and that CODA babies have a trend to more face-looking while NSE babies don't.

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

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Let's also check out MFCR (a stricter ratio). Almost the same thing.

``` r
ggplot(data_mid, aes(x = age, y = mfcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + facet_wrap("language") + ggtitle("Middle-Middle FaceChest Ratio")
```

    ## Warning: Removed 5 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

So this is rad! Such obvious group differences should also be reflected in a heat map that is collapsed across age and direction. And the heat map below shows that CODAs really focus on the MidFaceBottom AOI, while English-exposed children are a bit more spread out, looking much more at the chest.

Heat Maps
=========

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

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

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

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

``` r
ggplot(data_mid_heat, aes(x = direction, y = aoi)) +
  geom_tile(aes(fill=percent),color="lightgray",na.rm=TRUE) + 
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,.5)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map by Group") + facet_wrap("language")
```

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-2.png)

Statistical Testing of FCR/mFCR
===============================

Now that we've found something interesting with FCR and mFCR, let's see if it holds up in stats modeling.

Language, Direction, & Age Predictors
-------------------------------------

First, a LMM with predictors Language, Direction, and Age, and outcome variable FCR. Nothing significant.

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
    ## REML criterion at convergence: 369.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.52868 -0.67375  0.03807  0.61411  2.97657 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.268988 0.51864 
    ##  story       (Intercept) 0.009965 0.09982 
    ##  Residual                0.128503 0.35847 
    ## Number of obs: 338, groups:  participant, 27; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                         -0.01485    0.37289  24.98000  -0.040
    ## age                                 -0.36358    0.52649  24.31000  -0.691
    ## languagesign                        -0.33884    0.63893  24.66000  -0.530
    ## directionreversed                    0.02233    0.14098 301.61000   0.158
    ## age:languagesign                     0.97261    0.76245  24.40000   1.276
    ## age:directionreversed                0.10706    0.19502 301.80000   0.549
    ## languagesign:directionreversed      -0.15456    0.25088 300.79000  -0.616
    ## age:languagesign:directionreversed  -0.12000    0.29100 301.43000  -0.412
    ##                                    Pr(>|t|)
    ## (Intercept)                           0.969
    ## age                                   0.496
    ## languagesign                          0.601
    ## directionreversed                     0.874
    ## age:languagesign                      0.214
    ## age:directionreversed                 0.583
    ## languagesign:directionreversed        0.538
    ## age:languagesign:directionreversed    0.680
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.939                                          
    ## languagesgn -0.578  0.548                                   
    ## dirctnrvrsd -0.188  0.174  0.110                            
    ## age:lnggsgn  0.648 -0.691 -0.926 -0.121                     
    ## ag:drctnrvr  0.177 -0.184 -0.104 -0.943  0.127              
    ## lnggsgn:drc  0.106 -0.098 -0.187 -0.562  0.170  0.531       
    ## ag:lnggsgn: -0.119  0.123  0.175  0.633 -0.183 -0.672 -0.932

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
    ## REML criterion at convergence: 460.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9885 -0.5905  0.0732  0.6389  2.4805 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.226250 0.47566 
    ##  story       (Intercept) 0.008297 0.09109 
    ##  Residual                0.179761 0.42398 
    ## Number of obs: 333, groups:  participant, 27; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                          0.16081    0.35124  26.15000   0.458
    ## age                                 -0.46613    0.49485  25.24000  -0.942
    ## languagesign                        -0.36576    0.60371  26.13000  -0.606
    ## directionreversed                    0.02309    0.16944 296.95000   0.136
    ## age:languagesign                     1.02519    0.71792  25.51000   1.428
    ## age:directionreversed                0.12510    0.23294 297.16000   0.537
    ## languagesign:directionreversed      -0.17973    0.30089 295.59000  -0.597
    ## age:languagesign:directionreversed  -0.13717    0.34735 296.43000  -0.395
    ##                                    Pr(>|t|)
    ## (Intercept)                           0.651
    ## age                                   0.355
    ## languagesign                          0.550
    ## directionreversed                     0.892
    ## age:languagesign                      0.165
    ## age:directionreversed                 0.592
    ## languagesign:directionreversed        0.551
    ## age:languagesign:directionreversed    0.693
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.939                                          
    ## languagesgn -0.577  0.546                                   
    ## dirctnrvrsd -0.245  0.225  0.142                            
    ## age:lnggsgn  0.647 -0.689 -0.927 -0.155                     
    ## ag:drctnrvr  0.230 -0.237 -0.134 -0.944  0.163              
    ## lnggsgn:drc  0.137 -0.126 -0.247 -0.562  0.223  0.531       
    ## ag:lnggsgn: -0.154  0.159  0.230  0.633 -0.237 -0.671 -0.933

What if we did ANCOVAs (C because age would be a covariate). First, FCR as outcome.

Direction & Language Predictors
-------------------------------

In the LMMs we've run so far, age is always the worst predictor. So we should have taken that out first, anyway.

Alright, we got an effect of language (p = 0.07) AND an interaction with direction, p = 0.008. CODA babies generally have much higher FCR, but they're also strongly affected by reversal.

``` r
fcr_lmm_noage <- lmer(fcr ~ direction * language + (1|story) + (1|participant), data = data_mid)
summary(fcr_lmm_noage)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ direction * language + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 369.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.54695 -0.64652  0.04382  0.61175  2.97282 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.26377  0.5136  
    ##  story       (Intercept) 0.01006  0.1003  
    ##  Residual                0.12779  0.3575  
    ## Number of obs: 338, groups:  participant, 27; story, 8
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error        df t value
    ## (Intercept)                     -0.25744    0.12752  29.81000  -2.019
    ## directionreversed                0.09537    0.04674 302.25000   2.040
    ## languagesign                     0.43785    0.22459  26.27000   1.950
    ## directionreversed:languagesign  -0.23924    0.08509 302.35000  -2.812
    ##                                Pr(>|t|)   
    ## (Intercept)                     0.05258 . 
    ## directionreversed               0.04220 * 
    ## languagesign                    0.06198 . 
    ## directionreversed:languagesign  0.00525 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.181              
    ## languagesgn -0.524  0.102       
    ## drctnrvrsd:  0.099 -0.549 -0.181

The mFCR LMM gives us similar results.

``` r
mfcr_lmm_noage <- lmer(mfcr ~ direction * language + (1|story) + (1|participant), data = data_mid)
summary(mfcr_lmm_noage)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: mfcr ~ direction * language + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 461.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9893 -0.5899  0.0563  0.6405  2.4299 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.224255 0.47356 
    ##  story       (Intercept) 0.008418 0.09175 
    ##  Residual                0.178765 0.42281 
    ## Number of obs: 333, groups:  participant, 27; story, 8
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error       df t value
    ## (Intercept)                     -0.1509     0.1202  30.2100  -1.255
    ## directionreversed                0.1092     0.0558 297.2700   1.957
    ## languagesign                     0.4376     0.2122  27.1100   2.062
    ## directionreversed:languagesign  -0.2773     0.1012 297.4900  -2.739
    ##                                Pr(>|t|)   
    ## (Intercept)                     0.21913   
    ## directionreversed               0.05134 . 
    ## languagesign                    0.04888 * 
    ## directionreversed:languagesign  0.00653 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.231              
    ## languagesgn -0.525  0.131       
    ## drctnrvrsd:  0.127 -0.551 -0.232

Language as only predictor
--------------------------

**OLD OLD OLD** Same thing here. So it's not age or direction, but language, that has the effect. And that's good! I want to look at forward v. reversed separately next.

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
    ## REML criterion at convergence: 370
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.54138 -0.63981  0.05108  0.60122  3.05902 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.2641   0.5139  
    ##  story       (Intercept) 0.0101   0.1005  
    ##  direction   (Intercept) 0.0000   0.0000  
    ##  Residual                0.1304   0.3611  
    ## Number of obs: 338, groups:  participant, 27; story, 8; direction, 2
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error      df t value Pr(>|t|)
    ## (Intercept)   -0.2105     0.1256 27.8840  -1.676    0.105
    ## languagesign   0.3229     0.2211 24.5640   1.461    0.157
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.522

Forward only. Language effect, p = 0.05.

``` r
fcr_lmm_langonly_f <- lmer(fcr ~  language + (1|story) + (1|participant), data = filter(data_mid,direction=="forward"))
summary(fcr_lmm_langonly_f)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ language + (1 | story) + (1 | participant)
    ##    Data: filter(data_mid, direction == "forward")
    ## 
    ## REML criterion at convergence: 189.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5993 -0.5567  0.1215  0.5792  1.9983 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.28704  0.5358  
    ##  story       (Intercept) 0.02783  0.1668  
    ##  Residual                0.10308  0.3211  
    ## Number of obs: 173, groups:  participant, 27; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error      df t value Pr(>|t|)  
    ## (Intercept)   -0.2745     0.1398 28.6770  -1.963   0.0594 .
    ## languagesign   0.4538     0.2323 23.6460   1.954   0.0627 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.494

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
    ## REML criterion at convergence: 217.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.43439 -0.58613 -0.06177  0.55012  2.62907 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.22074  0.4698  
    ##  story       (Intercept) 0.01745  0.1321  
    ##  Residual                0.14124  0.3758  
    ## Number of obs: 165, groups:  participant, 27; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error      df t value Pr(>|t|)
    ## (Intercept)   -0.1596     0.1230 26.9310  -1.298    0.205
    ## languagesign   0.1936     0.2091 23.6230   0.926    0.364
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.503

Let's go ahead and plot boxplots to represent LMMs with only language as an important effect. (We can add in direction too). FaceChest Ratio here.

``` r
ggplot(data_mid, aes(x = language, y = fcr, fill = language)) + geom_boxplot() + scale_fill_brewer(palette = "Dark2") + ylab("FaceChest Ratio") + ggtitle("FaceChest Ratio by Language")
```

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

``` r
ggplot(data_mid, aes(x = language, y = fcr, fill = direction)) + geom_boxplot() + ylab("FaceChest Ratio") + ggtitle("FaceChest Ratio by Language & Direction")
```

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-2.png)

And Middle-Middle FaceChest Ratio here.

``` r
ggplot(data_mid, aes(x = language, y = mfcr, fill = language)) + geom_boxplot() + scale_fill_brewer(palette = "Dark2") + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language")
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

``` r
ggplot(data_mid, aes(x = language, y = mfcr, fill = direction)) + geom_boxplot() + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language & Direction")
```

    ## Warning: Removed 5 rows containing non-finite values (stat_boxplot).

![](04babybasiceyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-2.png)
