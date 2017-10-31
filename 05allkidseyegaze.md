All Children's Eye Gaze (study2children)
================
Adam Stone, PhD
10-31-2017

-   [All Children!](#all-children)
-   [Statistical Testing of FCR/mFCR](#statistical-testing-of-fcrmfcr)
    -   [Language, Direction, & Age Predictors](#language-direction-age-predictors)
    -   [Direction & Language Predictors](#direction-language-predictors)
    -   [Separate for direction](#separate-for-direction)
-   [Are Babies and Children Different?](#are-babies-and-children-different)
-   [Means and SEs](#means-and-ses)

All Children!
=============

We're importing *both* the child and baby data. Let's get a chart of how old all our kids are.

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
babydata <- read_feather("cleanedbabyeyedata.feather")
kiddata <- read_feather("cleanedchildeyedata.feather")
data <- rbind(babydata,kiddata)

data_ages <- data %>%
  select(participant, language, age, group) %>%
  distinct()

ggplot(data_ages, aes(x = age, fill = language)) + geom_histogram(binwidth = 0.1) + facet_grid(language ~ .) + scale_fill_brewer(palette = "Accent")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

Next, let's do boxplots of each AOI across all stories and kids, grouping for forward vs. reversed.

``` r
# Boxplot!
ggplot(data, aes(x = aoi, y = percent, fill = direction)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  ggtitle("Looking Percentages for Each AOI, All Stories") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

Looks like by far most of the activity is along the Mid\*\* AOIs. Let's look closer.

``` r
data_mid <- data %>%
  filter(str_detect(aoi,"Mid") | aoi == "BelowChest")

ggplot(data_mid, aes(x = aoi, y = percent, fill = direction)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  ggtitle("Looking Percentages for Middle AOIs, All Stories") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

As a preliminary analysis I'm going to just look at MidChestTop and MidFaceBottom. Are there differences based on direction, group, or age for either AOI?

``` r
data_mid %>% 
  filter(aoi == "MidChestTop" | aoi == "MidFaceBottom") %>%
  ggplot(aes(x = age, y = percent, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + facet_grid(language ~ aoi) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  ggtitle("Top 2 AOIs")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

I am not observing big differences for direction. And not strong patterns across age, although there are subtle upward trends for increased chest looking in NSE kids as they get older, but increased face looking in CODAS as they get older.

What if we defined a Face-Chest Ratio (FCR) such that:

1.  MidFaceCenter, MidFaceBottom = Face
2.  MidChestTop, MidChestCenter, MidChestBottom, BelowChest = Chest
3.  FCR = face - chest / face + chest

Let's try that. Let's also try only MidFaceBottom vs. MidChestTop too, and call that MFCR (for middle-middle).

Chart belows suggests group differences. CODAs overall have higher FCR ratios, AND direction effects work differently. While NSE kids generally do not show an age-related trend towards more face-looking.

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

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Let's also check out MFCR (a stricter ratio). Almost the same thing.

``` r
ggplot(data_mid, aes(x = age, y = mfcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") + facet_wrap("language") + ggtitle("Middle-Middle FaceChest Ratio")
```

    ## Warning: Removed 11 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 11 rows containing missing values (geom_point).

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

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
  scale_fill_viridis(option = "viridis", direction=-1, limits = c(0,1)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  ylab("") + xlab("") + ggtitle("Eye Gaze Heat Map")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

Statistical Testing of FCR/mFCR
===============================

Now that we've found something interesting with FCR and mFCR, let's see if it holds up in stats modeling.

Language, Direction, & Age Predictors
-------------------------------------

First, a LMM with predictors Language, Direction, and Age, and outcome variable FCR.

Here, some interesting effects (and this model was NOT working with just babies and children!).

1.  *NEED TO UPDATE WRITE-UP...ADDED MORE BABIES*
2.  Overall group effect of language. CODA kids on average have 0.48 higher FCR than NSE kids, p = 0.0494.
3.  Significant Language X Direction interaction (p = 0.01).
4.  No effect of age.

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
    ## REML criterion at convergence: 1051.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1861 -0.5796  0.0506  0.6523  2.6752 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.186281 0.43160 
    ##  story       (Intercept) 0.008095 0.08997 
    ##  Residual                0.193237 0.43959 
    ## Number of obs: 727, groups:  participant, 56; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                         -0.29338    0.12315  64.00000  -2.382
    ## age                                  0.04361    0.03435  59.00000   1.270
    ## languagesign                         0.52863    0.22722  59.00000   2.327
    ## directionreversed                    0.13098    0.06399 662.40000   2.047
    ## age:languagesign                    -0.01312    0.05545  58.80000  -0.237
    ## age:directionreversed               -0.04024    0.01872 658.10000  -2.150
    ## languagesign:directionreversed      -0.30884    0.12246 661.10000  -2.522
    ## age:languagesign:directionreversed   0.05375    0.02987 666.80000   1.800
    ##                                    Pr(>|t|)  
    ## (Intercept)                          0.0202 *
    ## age                                  0.2092  
    ## languagesign                         0.0234 *
    ## directionreversed                    0.0411 *
    ## age:languagesign                     0.8138  
    ## age:directionreversed                0.0319 *
    ## languagesign:directionreversed       0.0119 *
    ## age:languagesign:directionreversed   0.0724 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.707                                          
    ## languagesgn -0.506  0.383                                   
    ## dirctnrvrsd -0.257  0.198  0.139                            
    ## age:lnggsgn  0.438 -0.620 -0.816 -0.123                     
    ## ag:drctnrvr  0.189 -0.270 -0.103 -0.735  0.169              
    ## lnggsgn:drc  0.134 -0.104 -0.259 -0.523  0.213  0.385       
    ## ag:lnggsgn: -0.119  0.171  0.213  0.463 -0.264 -0.634 -0.816

Let's repeat the LMM with outcome mFCR. Stronger statistical values.

1.  *NEED TO UPDATE WRITE-UP...ADDED MORE BABIES*
2.  Overall group effect of language, p = 0.361.
3.  Significant Direction effect, p = 0.0485.
4.  Significant Language X Direction interaction (p = 0.01).
5.  No effect of age.

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
    ## REML criterion at convergence: 1103.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2209 -0.5907  0.0889  0.6690  2.6702 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.1790   0.42304 
    ##  story       (Intercept) 0.0054   0.07348 
    ##  Residual                0.2149   0.46360 
    ## Number of obs: 716, groups:  participant, 56; story, 8
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error        df t value
    ## (Intercept)                         -0.18629    0.12101  64.30000  -1.540
    ## age                                  0.04077    0.03406  60.00000   1.197
    ## languagesign                         0.53906    0.22591  60.60000   2.386
    ## directionreversed                    0.14988    0.06810 652.20000   2.201
    ## age:languagesign                    -0.02018    0.05504  60.10000  -0.367
    ## age:directionreversed               -0.03256    0.01982 631.10000  -1.642
    ## languagesign:directionreversed      -0.34732    0.12997 650.40000  -2.672
    ## age:languagesign:directionreversed   0.05027    0.03165 653.80000   1.588
    ##                                    Pr(>|t|)   
    ## (Intercept)                         0.12858   
    ## age                                 0.23601   
    ## languagesign                        0.02016 * 
    ## directionreversed                   0.02809 * 
    ## age:languagesign                    0.71511   
    ## age:directionreversed               0.10100   
    ## languagesign:directionreversed      0.00772 **
    ## age:languagesign:directionreversed  0.11274   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) age    lnggsg drctnr ag:lng ag:drc lnggs:
    ## age         -0.715                                          
    ## languagesgn -0.511  0.383                                   
    ## dirctnrvrsd -0.280  0.212  0.150                            
    ## age:lnggsgn  0.443 -0.619 -0.817 -0.132                     
    ## ag:drctnrvr  0.206 -0.286 -0.110 -0.736  0.179              
    ## lnggsgn:drc  0.147 -0.111 -0.282 -0.524  0.230  0.386       
    ## ag:lnggsgn: -0.129  0.181  0.230  0.463 -0.283 -0.632 -0.817

What if we did ANCOVAs (C because age would be a covariate). First, FCR as outcome.

Direction & Language Predictors
-------------------------------

In the LMMs we've run so far, age is always the worst predictor. So we should take that out.

1.  *NEED TO UPDATE WRITE-UP...ADDED MORE BABIES*
2.  Strong effect of language (p &lt; 0.001)
3.  Significant language x direction interaction (p = 0.02).

``` r
fcr_lmm_noage <- lmer(fcr ~ direction * language + (1|story) + (1|participant), data = data_mid)
summary(fcr_lmm_noage)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ direction * language + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 1036.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1674 -0.6023  0.0594  0.6522  2.5727 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.184903 0.43000 
    ##  story       (Intercept) 0.008229 0.09071 
    ##  Residual                0.193963 0.44041 
    ## Number of obs: 727, groups:  participant, 56; story, 8
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error        df t value
    ## (Intercept)                     -0.18330    0.08702  61.10000  -2.106
    ## directionreversed                0.03003    0.04346 668.90000   0.691
    ## languagesign                     0.53656    0.12614  61.10000   4.254
    ## directionreversed:languagesign  -0.15590    0.06766 668.70000  -2.304
    ##                                Pr(>|t|)    
    ## (Intercept)                      0.0393 *  
    ## directionreversed                0.4899    
    ## languagesign                   7.34e-05 ***
    ## directionreversed:languagesign   0.0215 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.247              
    ## languagesgn -0.596  0.172       
    ## drctnrvrsd:  0.160 -0.649 -0.264

The mFCR LMM gives us similar results. *NEED TO UPDATE WRITE-UP...ADDED MORE BABIES*

``` r
mfcr_lmm_noage <- lmer(mfcr ~ direction * language + (1|story) + (1|participant), data = data_mid)
summary(mfcr_lmm_noage)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: mfcr ~ direction * language + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 1086.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2141 -0.5971  0.1001  0.6560  2.5850 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.176484 0.42010 
    ##  story       (Intercept) 0.005464 0.07392 
    ##  Residual                0.215222 0.46392 
    ## Number of obs: 716, groups:  participant, 56; story, 8
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error        df t value
    ## (Intercept)                     -0.08262    0.08420  63.00000  -0.981
    ## directionreversed                0.06758    0.04610 657.30000   1.466
    ## languagesign                     0.51504    0.12482  62.30000   4.126
    ## directionreversed:languagesign  -0.19644    0.07178 658.80000  -2.737
    ##                                Pr(>|t|)    
    ## (Intercept)                    0.330225    
    ## directionreversed              0.143161    
    ## languagesign                   0.000111 ***
    ## directionreversed:languagesign 0.006376 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) drctnr lnggsg
    ## dirctnrvrsd -0.271              
    ## languagesgn -0.610  0.185       
    ## drctnrvrsd:  0.176 -0.649 -0.284

Separate for direction
----------------------

Forward only. Language effect, p &lt; 0.001. (I ran this WITH age, and age was not significant). *NEED TO UPDATE WRITE-UP...ADDED MORE BABIES*

``` r
fcr_lmm_langonly_f <- lmer(fcr ~ language + (1|story) + (1|participant), data = filter(data_mid,direction=="forward"))
summary(fcr_lmm_langonly_f)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ language + (1 | story) + (1 | participant)
    ##    Data: filter(data_mid, direction == "forward")
    ## 
    ## REML criterion at convergence: 513.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1723 -0.5106  0.0275  0.5872  2.4914 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.19540  0.4420  
    ##  story       (Intercept) 0.03001  0.1732  
    ##  Residual                0.16197  0.4025  
    ## Number of obs: 368, groups:  participant, 56; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)   -0.1871     0.1028 30.4200  -1.820 0.078658 .  
    ## languagesign   0.5340     0.1284 52.8700   4.159 0.000118 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.518

Reversed only. Slightly weaker language effect, p = 0.008. (I ran this with AGE too and not sig.) *NEED TO UPDATE WRITE-UP...ADDED MORE BABIES*

``` r
fcr_lmm_langonly_r <- lmer(fcr ~ language + (1|story) + (1|participant), data = filter(data_mid,direction=="reversed"))
summary(fcr_lmm_langonly_r)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ language + (1 | story) + (1 | participant)
    ##    Data: filter(data_mid, direction == "reversed")
    ## 
    ## REML criterion at convergence: 572.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -3.04814 -0.54162  0.03133  0.66742  2.76008 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.15693  0.3961  
    ##  story       (Intercept) 0.01582  0.1258  
    ##  Residual                0.21241  0.4609  
    ## Number of obs: 359, groups:  participant, 56; story, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error       df t value Pr(>|t|)   
    ## (Intercept)  -0.15279    0.08865 34.82000  -1.724  0.09366 . 
    ## languagesign  0.38357    0.11946 52.28000   3.211  0.00226 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## languagesgn -0.557

Let's go ahead and plot boxplots to represent LMMs with only language as an important effect. (We can add in direction too). FaceChest Ratio here.

``` r
ggplot(data_mid, aes(x = language, y = fcr, fill = language)) + geom_boxplot() + scale_fill_brewer(palette = "Dark2") + ylab("FaceChest Ratio") + ggtitle("FaceChest Ratio by Language")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png)

``` r
ggplot(data_mid, aes(x = language, y = fcr, fill = direction)) + geom_boxplot() + ylab("FaceChest Ratio") + ggtitle("FaceChest Ratio by Language & Direction")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-2.png)

And Middle-Middle FaceChest Ratio here.

``` r
ggplot(data_mid, aes(x = language, y = mfcr, fill = language)) + geom_boxplot() + scale_fill_brewer(palette = "Dark2") + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language")
```

    ## Warning: Removed 11 rows containing non-finite values (stat_boxplot).

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-1.png)

``` r
ggplot(data_mid, aes(x = language, y = mfcr, fill = direction)) + geom_boxplot() + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language & Direction")
```

    ## Warning: Removed 11 rows containing non-finite values (stat_boxplot).

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-2.png)

Maybe error bars would be nice here. Let's do it.

``` r
data_mid_fcr <- data_mid %>%
  group_by(language, participant, direction) %>%
  summarise(fcr_mean_subj = mean(fcr, na.rm=TRUE)) %>%
  group_by(language, direction) %>%
  summarise(fcr_mean = mean(fcr_mean_subj, na.rm=TRUE),
            fcr_sd = sd(fcr_mean_subj, na.rm=TRUE),
            fcr_n = n()) %>%
  ungroup() %>%
  mutate(fcr_se = fcr_sd/sqrt(fcr_n))

ggplot(data_mid_fcr, aes(x = language, y = fcr_mean, color = direction, group = direction, fill = direction)) + geom_errorbar(aes(ymin = fcr_mean-fcr_se, ymax = fcr_mean+fcr_se), position = "dodge", width = 0.2) +
  geom_point(position = position_dodge(width = 0.2)) + scale_y_continuous(limits = c(-1,1)) + ylab("M-FaceChest Ratio") + ggtitle("FaceChest Ratio by Language & Direction")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

``` r
data_mid_mfcr <- data_mid %>%
  group_by(language, participant, direction) %>%
  summarise(fcr_mean_subj = mean(mfcr, na.rm=TRUE)) %>%
  group_by(language, direction) %>%
  summarise(fcr_mean = mean(fcr_mean_subj, na.rm=TRUE),
            fcr_sd = sd(fcr_mean_subj, na.rm=TRUE),
            fcr_n = n()) %>%
  ungroup() %>%
  mutate(fcr_se = fcr_sd/sqrt(fcr_n))

ggplot(data_mid_mfcr, aes(x = language, y = fcr_mean, color = direction, group = direction, fill = direction)) + geom_errorbar(aes(ymin = fcr_mean-fcr_se, ymax = fcr_mean+fcr_se), position = "dodge", width = 0.2) +
  geom_point(position = position_dodge(width = 0.2)) + scale_y_continuous(limits = c(-1,1)) + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language & Direction")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

``` r
data_mid_fcr <- data_mid %>%
  group_by(language, participant) %>%
  summarise(fcr_mean_subj = mean(fcr, na.rm=TRUE)) %>%
  group_by(language) %>%
  summarise(fcr_mean = mean(fcr_mean_subj, na.rm=TRUE),
            fcr_sd = sd(fcr_mean_subj, na.rm=TRUE),
            fcr_n = n()) %>%
  ungroup() %>%
  mutate(fcr_se = fcr_sd/sqrt(fcr_n))

ggplot(data_mid_fcr, aes(x = language, y = fcr_mean, color = language)) + geom_errorbar(aes(ymin = fcr_mean-fcr_se, ymax = fcr_mean+fcr_se), width = 0.2) +
  geom_point() + scale_y_continuous(limits = c(-1,1)) + ylab("M-FaceChest Ratio") + ggtitle("FaceChest Ratio by Language") + scale_color_brewer(palette = "Dark2")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png)

``` r
data_mid_mfcr <- data_mid %>%
  group_by(language, participant) %>%
  summarise(fcr_mean_subj = mean(mfcr, na.rm=TRUE)) %>%
  group_by(language) %>%
  summarise(fcr_mean = mean(fcr_mean_subj, na.rm=TRUE),
            fcr_sd = sd(fcr_mean_subj, na.rm=TRUE),
            fcr_n = n()) %>%
  ungroup() %>%
  mutate(fcr_se = fcr_sd/sqrt(fcr_n))

ggplot(data_mid_mfcr, aes(x = language, y = fcr_mean, color = language)) + geom_errorbar(aes(ymin = fcr_mean-fcr_se, ymax = fcr_mean+fcr_se), width = 0.2) +
  geom_point() + scale_y_continuous(limits = c(-1,1)) + ylab("M-FaceChest Ratio") + ggtitle("Middle-Middle FaceChest Ratio by Language") + scale_color_brewer(palette = "Dark2")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-19-1.png)

Are Babies and Children Different?
==================================

Quick check here. First, are CODA babies different from CODA children?

``` r
data_mid <- data_mid %>% 
  mutate(agegroup = ifelse(age < 2, "baby", "child")) %>%
  mutate(agegroup = as.factor(agegroup))

ggplot(data_mid, aes(x = age, y = fcr, color = direction)) + geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", aes(linetype = agegroup)) + facet_grid(language ~ .) + ggtitle("FaceChest Ratio")
```

![](05allkidseyegaze_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-20-1.png) Let's do a LMM with agegroup

``` r
agegroup_lmm <- lmer(fcr ~ agegroup * direction * language + (1|story) + (1|participant), data = data_mid)
summary(agegroup_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: 
    ## fcr ~ agegroup * direction * language + (1 | story) + (1 | participant)
    ##    Data: data_mid
    ## 
    ## REML criterion at convergence: 1041
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2017 -0.5904  0.0549  0.6484  2.6514 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.187558 0.43308 
    ##  story       (Intercept) 0.008123 0.09013 
    ##  Residual                0.193704 0.44012 
    ## Number of obs: 727, groups:  participant, 56; story, 8
    ## 
    ## Fixed effects:
    ##                                               Estimate Std. Error
    ## (Intercept)                                   -0.26739    0.11215
    ## agegroupchild                                  0.19672    0.16452
    ## directionreversed                              0.09569    0.05752
    ## languagesign                                   0.53514    0.20678
    ## agegroupchild:directionreversed               -0.15272    0.08741
    ## agegroupchild:languagesign                    -0.07464    0.26825
    ## directionreversed:languagesign                -0.25213    0.11129
    ## agegroupchild:directionreversed:languagesign   0.19672    0.14399
    ##                                                     df t value Pr(>|t|)  
    ## (Intercept)                                   63.80000  -2.384   0.0201 *
    ## agegroupchild                                 58.60000   1.196   0.2366  
    ## directionreversed                            660.30000   1.664   0.0967 .
    ## languagesign                                  58.90000   2.588   0.0121 *
    ## agegroupchild:directionreversed              666.70000  -1.747   0.0811 .
    ## agegroupchild:languagesign                    58.70000  -0.278   0.7818  
    ## directionreversed:languagesign               660.10000  -2.266   0.0238 *
    ## agegroupchild:directionreversed:languagesign 666.60000   1.366   0.1723  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) aggrpc drctnr lnggsg aggrpchld:d aggrpchld:l drctn:
    ## agegropchld -0.627                                                    
    ## dirctnrvrsd -0.253  0.172                                             
    ## languagesgn -0.498  0.340  0.137                                      
    ## aggrpchld:d  0.167 -0.264 -0.656 -0.092                               
    ## aggrpchld:l  0.384 -0.614 -0.105 -0.771  0.165                        
    ## drctnrvrsd:  0.131 -0.090 -0.516 -0.257  0.343       0.198            
    ## aggrpchld:: -0.101  0.163  0.397  0.200 -0.616      -0.261      -0.775

LMM here for CODA babies vs. CODA children - no difference based on age group. And here there's no direction effect, either. (Same for mFCR)

``` r
data_coda <- data_mid %>% 
  filter(language=="sign")

coda_lmm <- lmer(fcr ~ agegroup * direction + (1|story) + (1|participant), data = data_coda)
summary(coda_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ agegroup * direction + (1 | story) + (1 | participant)
    ##    Data: data_coda
    ## 
    ## REML criterion at convergence: 452.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9848 -0.5206  0.1613  0.6723  2.2100 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.119839 0.3462  
    ##  story       (Intercept) 0.009447 0.0972  
    ##  Residual                0.217239 0.4661  
    ## Number of obs: 298, groups:  participant, 23; story, 8
    ## 
    ## Fixed effects:
    ##                                  Estimate Std. Error        df t value
    ## (Intercept)                       0.27506    0.15247  27.41000   1.804
    ## agegroupchild                     0.11193    0.17799  25.20000   0.629
    ## directionreversed                -0.15734    0.10100 266.75000  -1.558
    ## agegroupchild:directionreversed   0.04889    0.12029 268.79000   0.406
    ##                                 Pr(>|t|)  
    ## (Intercept)                       0.0822 .
    ## agegroupchild                     0.5351  
    ## directionreversed                 0.1205  
    ## agegroupchild:directionreversed   0.6847  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) aggrpc drctnr
    ## agegropchld -0.813              
    ## dirctnrvrsd -0.312  0.268       
    ## aggrpchld:d  0.262 -0.324 -0.842

LMM here for NSE babies vs. NSE children - no difference based on age group or direction. Same for mFCR.

``` r
data_nse <- data_mid %>% 
  filter(language=="english")

nse_lmm <- lmer(fcr ~ agegroup * direction + (1|story) + (1|participant), data = data_nse)
summary(nse_lmm)
```

    ## Linear mixed model fit by REML t-tests use Satterthwaite approximations
    ##   to degrees of freedom [lmerMod]
    ## Formula: fcr ~ agegroup * direction + (1 | story) + (1 | participant)
    ##    Data: data_nse
    ## 
    ## REML criterion at convergence: 587.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.77588 -0.62008 -0.00794  0.61742  2.87846 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.233593 0.48331 
    ##  story       (Intercept) 0.006391 0.07995 
    ##  Residual                0.178171 0.42210 
    ## Number of obs: 429, groups:  participant, 33; story, 8
    ## 
    ## Fixed effects:
    ##                                  Estimate Std. Error        df t value
    ## (Intercept)                      -0.26324    0.12104  37.10000  -2.175
    ## agegroupchild                     0.18789    0.18030  34.10000   1.042
    ## directionreversed                 0.09544    0.05518 387.50000   1.730
    ## agegroupchild:directionreversed  -0.14050    0.08452 390.10000  -1.662
    ##                                 Pr(>|t|)  
    ## (Intercept)                       0.0361 *
    ## agegroupchild                     0.3047  
    ## directionreversed                 0.0845 .
    ## agegroupchild:directionreversed   0.0972 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) aggrpc drctnr
    ## agegropchld -0.635              
    ## dirctnrvrsd -0.225  0.150       
    ## aggrpchld:d  0.147 -0.233 -0.649

Means and SEs
=============

FCR = Face-Chest Ratio

``` r
data_mid_fcr <- data_mid %>%
  group_by(agegroup, language, participant, direction) %>%
  summarise(fcr_mean_subj = mean(fcr, na.rm=TRUE)) %>%
  group_by(agegroup, language, direction) %>%
  summarise(fcr_mean = mean(fcr_mean_subj, na.rm=TRUE),
            fcr_sd = sd(fcr_mean_subj, na.rm=TRUE),
            fcr_n = n()) %>%
  ungroup() %>%
  mutate(fcr_se = fcr_sd/sqrt(fcr_n))

data_mid_fcr
```

    ## # A tibble: 8 x 7
    ##   agegroup language direction    fcr_mean    fcr_sd fcr_n     fcr_se
    ##      <chr>   <fctr>    <fctr>       <dbl>     <dbl> <int>      <dbl>
    ## 1     baby  english   forward -0.25731936 0.5510026    19 0.12640866
    ## 2     baby  english  reversed -0.15891037 0.4862433    19 0.11155186
    ## 3     baby     sign   forward  0.27948590 0.5892332     7 0.22270922
    ## 4     baby     sign  reversed  0.11129552 0.5859941     7 0.22148495
    ## 5    child  english   forward -0.06352509 0.5035101    14 0.13456873
    ## 6    child  english  reversed -0.13291127 0.5094853    14 0.13616568
    ## 7    child     sign   forward  0.39265962 0.2715946    16 0.06789865
    ## 8    child     sign  reversed  0.27799568 0.2816727    16 0.07041817

mFCR = Middle-Middle Face-Chest Ratio

``` r
data_mid_mfcr <- data_mid %>%
  group_by(agegroup, language, participant, direction) %>%
  summarise(fcr_mean_subj = mean(mfcr, na.rm=TRUE)) %>%
  group_by(agegroup, language, direction) %>%
  summarise(fcr_mean = mean(fcr_mean_subj, na.rm=TRUE),
            fcr_sd = sd(fcr_mean_subj, na.rm=TRUE),
            fcr_n = n()) %>%
  ungroup() %>%
  mutate(fcr_se = fcr_sd/sqrt(fcr_n))

data_mid_mfcr
```

    ## # A tibble: 8 x 7
    ##   agegroup language direction    fcr_mean    fcr_sd fcr_n     fcr_se
    ##      <chr>   <fctr>    <fctr>       <dbl>     <dbl> <int>      <dbl>
    ## 1     baby  english   forward -0.14960679 0.5216557    19 0.11967603
    ## 2     baby  english  reversed -0.03582799 0.4578598    19 0.10504023
    ## 3     baby     sign   forward  0.36802270 0.5704882     7 0.21562426
    ## 4     baby     sign  reversed  0.18253494 0.5896765     7 0.22287677
    ## 5    child  english   forward  0.03297344 0.5298127    14 0.14159841
    ## 6    child  english  reversed  0.01985989 0.5622754    14 0.15027441
    ## 7    child     sign   forward  0.47268061 0.2661491    16 0.06653727
    ## 8    child     sign  reversed  0.35318443 0.2414770    16 0.06036926
