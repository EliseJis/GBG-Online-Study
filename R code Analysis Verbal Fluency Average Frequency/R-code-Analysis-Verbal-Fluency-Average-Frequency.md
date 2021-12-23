R Code Full Analysis Verbal Fluency Average Frequency
================
Elise Oosterhuis
Last compiled on 23/12/21

# Analysis Verbal Fluency Data - Average Frequency of Correctly Produced Words

##### Read in data

``` r
#Semantic fluency
VFcat <- read.csv("../Data/Tidy/VFcat_complete_final.csv")
# head(VFcat[1:6,1:11]) #reads the top 6 rows of the first 11 columns
# tail(VFcat[1:6,1:11]) #reads the bottom 6 rows of the first 11 columns

#Letter fluency
VFlet <- read.csv("../Data/Tidy/VFlet_complete_final.csv")
# head(VFlet[1:6,1:9])
# tail(VFlet[1:6,1:9])

#Action fluency
VFact <- read.csv("../Data/Tidy/VFact_complete_final.csv")
# head(VFact[1:6,1:8])
# tail(VFact[1:6,1:8])
```

## Verbal Fluency - Semantic/Categories

### Descriptive Statistics

``` r
## Create dataset that only includes data of the Subtlex Average Frequency of the produced words
VFcat_AvFreq <- VFcat %>%
  dplyr::filter(Measures=="AvWordFreq_subtlex") %>%
  #Remove column called Measures (which now only contains "AvWordFreq_subtlex")
  dplyr::select(-Measures)

# head(VFcat_AvFreq, L=6)
```

``` r
#Create a descriptives table by summarising data of the semantic fluency task
(Descr_VFcat <- VFcat_AvFreq %>%
#Per Age Group
  group_by(Age.Category) %>%
  summarise(Nppt = length(unique(ID)), #Number of participants
            total = mean(Total, na.rm=T), #Total words correctly produced
            ztotal = mean(zComp.Cat, na.rm=T), #Z-score per age group of total words correctly produced
            animals = mean(Animals, na.rm=T), #Total words correctly produced in category Animals
            vehicles = mean(Vehicles, na.rm=T), #Total words correctly produced in category vehicles
            vandF = mean(Fruits.and.Vegetables, na.rm=T), #Total words correctly produced in category Fruits and Vegetables
            fluid = mean(Fluid, na.rm=T), #Total words correctly produced in category fluid
            writing = mean(Writing.Utensils, na.rm=T))) #Total words correctly produced in category writing utensils
```

    ## # A tibble: 3 x 9
    ##   Age.Category        Nppt total  ztotal animals vehicles vandF fluid writing
    ##   <chr>              <int> <dbl>   <dbl>   <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ## 1 18 to 30 years old    30  3.96  0.160     4.12     4.06  3.79  4.32    3.54
    ## 2 40 to 55 years old    30  4.40 -0.0515    3.92     3.97  3.73  4.21    3.47
    ## 3 65 to 80 years old    30  4.23 -0.0584    3.99     3.90  3.75  4.09    3.57

``` r
#Write table to file
# write.csv(Descr_VFcat, "./Figures and Tables/Descr_VFcat_AvFreq.csv", row.names = F)
```

``` r
#Visualise z-distribution of the composite score for semantic fluency Average word frequency
hist(VFcat_AvFreq$zComp.Cat, breaks=20) #Composite z-score of Semantic Fluency
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_hist_VFcatAvFreq_zScores-1.png)<!-- -->

#### Visualisation AvFreq Semantic Fluency

``` r
#Convert to long format for visualisation
VFcat_AvFreq.long <- VFcat_AvFreq %>%
  pivot_longer(cols=Total:Writing.Utensils, names_to = "category", values_to = "AvFreq")

#Boxplot VF Average frequency (AvFreq)

#Write figure to png file
# png(file="./Figures and Tables/Boxplot_VFcatAvFreq.png",
# width=800, height=350)


(Boxplot_VF <- VFcat_AvFreq.long %>%
    dplyr::filter(category!="Total") %>%
    ggplot(aes(x=factor(category, levels=c("Animals", "Vehicles", "Fruits.and.Vegetables", "Fluid", "Writing.Utensils")), y=AvFreq, 
                                               fill = as.factor(Age.Category))) +
    geom_boxplot(outlier.shape = NA, colour="grey50")+
    stat_summary(aes(label=round(..y..,1), group=as.factor(Age.Category)), 
               fun=mean, geom = "label", size=4,
               fill="white", show.legend=NA, label.size=NA,
               position = position_dodge(.75), vjust=-3) +
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Category",
         y = "AvFreq (raw scores)",
         title = "Average Word Frequency per Age Group and Category")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old")) +
  coord_cartesian(ylim = c(3,5)))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFcatAvFreq_rawPerCategory-1.png)<!-- -->

``` r
# dev.off() #Closes png() function
```

``` r
#Boxplot VF AvFreq Raw Total
# png(file="./Figures and Tables/Boxplot_VFcatAvFreq_RawTotal.png",
# width=600, height=350)

(Boxplot_VF <- VFcat_AvFreq.long %>%
    dplyr::filter(category=="Total") %>%
    ggplot(aes(x=category, y=AvFreq,fill = as.factor(Age.Category))) +
    geom_boxplot(outlier.shape = NA, colour="grey50")+
    stat_summary(aes(label=round(..y..,2), group=as.factor(Age.Category)), 
               fun=mean, geom = "label", size=4,
               fill="white", show.legend=NA, label.size=NA,
               position = position_dodge(.75), vjust=-2) +
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Category",
         y = "AvFreq (raw scores)",
         title = "Average Word Frequency for All Categories per Age Group")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old")) +
  coord_cartesian(ylim = c(3,5)))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFcatAvFreq_rawTotal-1.png)<!-- -->

``` r
# dev.off()
```

*Figures AvFreq Semantic Fluency zscores*

``` r
#Convert to long format for visualisation
VFcat_AvFreq.long.zscores <- VFcat_AvFreq %>%
  pivot_longer(cols=zComp.Cat:zWriting, names_to = "zcategory", values_to = "zAvFreq")

#Boxplot VF Average frequency (AvFreq)

#Write figure to png file
# png(file="./Figures and Tables/Boxplot_VFcatAvFreq_zscores.png",
# width=800, height=350)

(Boxplot_VFcat.zscores <- VFcat_AvFreq.long.zscores %>%
    dplyr::filter(zcategory!="zComp.Cat") %>%
    ggplot(aes(x=factor(zcategory, levels=c("zAnimals", "zVehicles", "zFandV", "zFluid", "zWriting")), y=zAvFreq, 
                                               fill = as.factor(Age.Category))) +
    geom_boxplot(colour="grey50")+
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Category",
         y = "AvFreq (z scores)",
         title = "Average Word Frequency per Age Group and Category")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old")) +
  coord_cartesian(ylim = c(-3,3)))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFcatAvFreq_zPerCategory-1.png)<!-- -->

``` r
# dev.off() #Closes png() function
```

``` r
#Boxplot VF AvFreq Raw Total
# png(file="./Figures and Tables/Boxplot_VFcatAvFreq_Total_zscores.png",
# width=600, height=350)

(Boxplot_VFcat.Totalzscore <- VFcat_AvFreq.long.zscores %>%
    dplyr::filter(zcategory=="zComp.Cat") %>%
    ggplot(aes(x=zcategory, y=zAvFreq,fill = as.factor(Age.Category))) +
    geom_boxplot(colour="grey50")+
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Category",
         y = "AvFreq (z scores)",
         title = "Average Word Frequency for All Categories per Age Group")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old")) +
  coord_cartesian(ylim = c(-3,3)))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFcatAvFreq_zTotal-1.png)<!-- -->

``` r
# dev.off()
```

## Multiple Linear Regression - Semantic Fluency

*Unconditional model, i.e. without covariates*

``` r
lmUncond.VFcat.AvFreq <- lm(zComp.Cat ~ Age.Category*CR.composite.before, data = VFcat_AvFreq)
# broom::tidy(lmUncond.VFcat.AvFreq, conf.int=T)
```

*Full model of AvFreq in z-distribution, including covariates*

``` r
lmFull.VFcat.AvFreq <- lm(zComp.Cat ~ Age.Category*CR.composite.before + zStroop.SRC + 
                              zWM.Score + zSoP.comp, data = VFcat_AvFreq)

#Tidy table output
(tidylmFull.VFcat.AvFreq <- broom::tidy(lmFull.VFcat.AvFreq, conf.int=T))
```

    ## # A tibble: 9 x 7
    ##   term                   estimate std.error statistic p.value conf.low conf.high
    ##   <chr>                     <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)              0.195     0.0965     2.03   0.0460  0.00352    0.387 
    ## 2 Age.Category40 to 55 ~  -0.263     0.123     -2.14   0.0357 -0.508     -0.0180
    ## 3 Age.Category65 to 80 ~  -0.267     0.156     -1.72   0.0900 -0.577      0.0426
    ## 4 CR.composite.before     -0.0413    0.0814    -0.507  0.613  -0.203      0.121 
    ## 5 zStroop.SRC              0.0942    0.0542     1.74   0.0860 -0.0136     0.202 
    ## 6 zWM.Score                0.0423    0.0596     0.711  0.479  -0.0762     0.161 
    ## 7 zSoP.comp               -0.0337    0.0869    -0.388  0.699  -0.207      0.139 
    ## 8 Age.Category40 to 55 ~   0.0934    0.115      0.811  0.420  -0.136      0.322 
    ## 9 Age.Category65 to 80 ~  -0.112     0.116     -0.960  0.340  -0.343      0.120

``` r
# write.csv(tidylmFull.VFcat.AvFreq, "./Figures and Tables/VFcat_AvFreq_zlmFull.csv", row.names = F)
```

*Full model of AvFreq as raw score, including covariates*

``` r
lmFull.VFcat.AvFreq.raw <- lm(Total ~ Age.Category*CR.composite.before + zStroop.SRC + 
                              zWM.Score + zSoP.comp, data = VFcat_AvFreq)

#Tidy table output
broom::tidy(lmFull.VFcat.AvFreq.raw, conf.int=T)
```

    ## # A tibble: 9 x 7
    ##   term                  estimate std.error statistic  p.value conf.low conf.high
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)             3.78       0.462     8.18  3.28e-12    2.86      4.70 
    ## 2 Age.Category40 to 55~   0.529      0.590     0.896 3.73e- 1   -0.646     1.70 
    ## 3 Age.Category65 to 80~   0.662      0.746     0.887 3.78e- 1   -0.823     2.15 
    ## 4 CR.composite.before     0.0502     0.390     0.129 8.98e- 1   -0.725     0.826
    ## 5 zStroop.SRC             0.276      0.260     1.06  2.91e- 1   -0.241     0.792
    ## 6 zWM.Score              -0.104      0.285    -0.366 7.16e- 1   -0.672     0.463
    ## 7 zSoP.comp              -0.509      0.416    -1.22  2.25e- 1   -1.34      0.319
    ## 8 Age.Category40 to 55~   0.751      0.551     1.36  1.77e- 1   -0.346     1.85 
    ## 9 Age.Category65 to 80~  -0.108      0.557    -0.194 8.46e- 1   -1.22      1.00

The unconditional model and the model with the raw total scores do not
seem to predict the outcome variable for Verbal Fluency Categories. In
the full model with the average z-score as outcome variable, middle-aged
adults seem to significantly produce higher frequency words than young
adults.

Let’s check the model assumptions + fit.

*Assumption 1 - Linearity*

``` r
## Unconditional model with z composite score
plot(lmUncond.VFcat.AvFreq, 1, main = "Unconditional model")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_Linearity_LMunconditional-1.png)<!-- -->

``` r
## Full model with z composite score
plot(lmFull.VFcat.AvFreq, 1, main = "Full model (z-score composite score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_Linearity_LMzfull-1.png)<!-- -->

``` r
## Full model with raw total score
plot(lmFull.VFcat.AvFreq.raw, 1, main = "Full model (raw total score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_Linearity_LMrawfull-1.png)<!-- -->

For the models with the z-composite score as outcome variable, the red
line is mostly horizontal, meaning that there is a linear relationship
between the fitted line and the residual value. The model with the raw
average frequency scores seems to violate the assumption.

*Assumption 2 - Independence of Variables*

``` r
# Z composite of VF cat
## Create table with correlation values between predictor variables
tibble::as_tibble(cor(VFcat_AvFreq[,c(5,20,25,27,28)]), rownames="rowname") 
```

    ## # A tibble: 5 x 6
    ##   rowname             zComp.Cat zStroop.SRC zSoP.comp zWM.Score CR.composite.be~
    ##   <chr>                   <dbl>       <dbl>     <dbl>     <dbl>            <dbl>
    ## 1 zComp.Cat              1           0.0671   -0.116    0.0539          -0.0968 
    ## 2 zStroop.SRC            0.0671      1         0.432    0.116            0.0483 
    ## 3 zSoP.comp             -0.116       0.432     1        0.144            0.0228 
    ## 4 zWM.Score              0.0539      0.116     0.144    1               -0.00547
    ## 5 CR.composite.before   -0.0968      0.0483    0.0228  -0.00547          1

``` r
## Create correlation plot between predictor variables
corrplot(cor(VFcat_AvFreq[,c(5,20,25,27,28)]),method='circle') 
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_corPredictors_VFcatAvFreq_zScores-1.png)<!-- -->

``` r
## Raw scores
## Create table with correlation values between predictor variables
tibble::as_tibble(cor(VFcat_AvFreq[,c(11,20,25,27,28)]), rownames = "rowname")
```

    ## # A tibble: 5 x 6
    ##   rowname               Total zStroop.SRC zSoP.comp zWM.Score CR.composite.befo~
    ##   <chr>                 <dbl>       <dbl>     <dbl>     <dbl>              <dbl>
    ## 1 Total                1           0.109    -0.0451  -0.0596             0.128  
    ## 2 zStroop.SRC          0.109       1         0.432    0.116              0.0483 
    ## 3 zSoP.comp           -0.0451      0.432     1        0.144              0.0228 
    ## 4 zWM.Score           -0.0596      0.116     0.144    1                 -0.00547
    ## 5 CR.composite.before  0.128       0.0483    0.0228  -0.00547            1

``` r
## Create correlation plot between predictor variables
corrplot(cor(VFcat_AvFreq[,c(11,20,25,27,28)]))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_corPredictors_VFcatAvFreq_rawScores-1.png)<!-- -->

There seems to be no strong relationship between the predictor
variables. Hence, we can assume Independence of Variables.

*Assumption 3 - Normal Distribution of Residuals*

Full model with z composite score

``` r
plot(lmFull.VFcat.AvFreq, 2, main = "Full model (z-score composite score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_Normality_zScores-1.png)<!-- -->
Full model with raw total score

``` r
plot(lmFull.VFcat.AvFreq.raw, 2, main = "Full model (raw total score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_Normality_rawScores-1.png)<!-- -->

The model with the raw scores violates the assumption. This is likely
due to the two outliers on the right (point 7 and 17). The points in
both the unconditional and full model with the z-composite score seem to
roughly follow the straight line. There’s a little bit of a bulk on the
right for the full z-model and a small dip in both models. One
explanation could be the online format of the study. For example, some
participants might have scored lower than they would in a lab-based
study, due to unclear instructions. Also, it could have caused more
distractions than there would have been in a lab based study.

*Assumption 4 - Homoscedasticity or Equal Variance of Variables*

Full model with z composite score

``` r
plot(lmFull.VFcat.AvFreq, 3, main = "Full model (z-score composite score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_Homoscedasticity_zScores-1.png)<!-- -->
Full model with raw total score

``` r
plot(lmFull.VFcat.AvFreq.raw, 3, main = "Full model (raw total score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_Homoscedasticity_rawScores-1.png)<!-- -->
Again, the full model with the raw total score as outcome variable
violates the assumption. For the other two models, the variance of
residuals seems relatively equal across the predictors. Hence, the error
terms are relatively the same across all values of the independent
variable for all three models.

Conclusion: the full model with the z-composite variable as outcome
variable seem to meet the assumptions for multiple linear regression.

#### Model fit diagnostics

*Variation Inflation Factor*

Any VIF value above 4 needs further investigation. There seems to be no
concerning signs of collinearity (VIF values higher than 4). The
tolerance values indicate the percentage of variance that cannot be
explained for by the other predictor variables.

``` r
ols_vif_tol(lmFull.VFcat.AvFreq)
```

    ##                                            Variables Tolerance      VIF
    ## 1                     Age.Category40 to 55 years old 0.6255213 1.598667
    ## 2                     Age.Category65 to 80 years old 0.3910939 2.556931
    ## 3                                CR.composite.before 0.3297057 3.033008
    ## 4                                        zStroop.SRC 0.7584502 1.318478
    ## 5                                          zWM.Score 0.9498853 1.052759
    ## 6                                          zSoP.comp 0.4868349 2.054084
    ## 7 Age.Category40 to 55 years old:CR.composite.before 0.4944811 2.022322
    ## 8 Age.Category65 to 80 years old:CR.composite.before 0.4842401 2.065091

*Plot Diagnositcs Full model with z composite score for Semantic
Fluency*

``` r
ols_plot_diagnostics(lmFull.VFcat.AvFreq)
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_lmFull_diagnostics-1.png)<!-- -->![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_lmFull_diagnostics-2.png)<!-- -->![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFcatAvFreq_lmFull_diagnostics-3.png)<!-- -->

The Observed vs Predicted Plot shows that the model doesn’t fit the data
very well (pity) –&gt; due to outliers?? What to do….

### Model comparisons for the CR measure preciding and coinciding with the COVID-19 pandemic

The effect of the COVID-19 pandemic on CR and subsequent behavioural
performance

``` r
lmFull.VFcat.AvFreq.during <- lm(zComp.Cat ~ Age.Category*CR.composite.during + 
                                   zStroop.SRC + zWM.Score + zSoP.comp, data = 
                                   VFcat_AvFreq)
summary(lmFull.VFcat.AvFreq.during)
```

    ## 
    ## Call:
    ## lm(formula = zComp.Cat ~ Age.Category * CR.composite.during + 
    ##     zStroop.SRC + zWM.Score + zSoP.comp, data = VFcat_AvFreq)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.0190 -0.2890 -0.0848  0.2446  0.9211 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value
    ## (Intercept)                                         0.20434    0.09853   2.074
    ## Age.Category40 to 55 years old                     -0.27014    0.12559  -2.151
    ## Age.Category65 to 80 years old                     -0.28682    0.15937  -1.800
    ## CR.composite.during                                -0.02521    0.08506  -0.296
    ## zStroop.SRC                                         0.09345    0.05568   1.679
    ## zWM.Score                                           0.03405    0.06033   0.564
    ## zSoP.comp                                          -0.01705    0.08861  -0.192
    ## Age.Category40 to 55 years old:CR.composite.during  0.06740    0.11900   0.566
    ## Age.Category65 to 80 years old:CR.composite.during -0.06745    0.12111  -0.557
    ##                                                    Pr(>|t|)  
    ## (Intercept)                                          0.0413 *
    ## Age.Category40 to 55 years old                       0.0345 *
    ## Age.Category65 to 80 years old                       0.0756 .
    ## CR.composite.during                                  0.7677  
    ## zStroop.SRC                                          0.0971 .
    ## zWM.Score                                            0.5741  
    ## zSoP.comp                                            0.8479  
    ## Age.Category40 to 55 years old:CR.composite.during   0.5727  
    ## Age.Category65 to 80 years old:CR.composite.during   0.5791  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4426 on 81 degrees of freedom
    ## Multiple R-squared:  0.103,  Adjusted R-squared:  0.01442 
    ## F-statistic: 1.163 on 8 and 81 DF,  p-value: 0.3318

``` r
anova(lmFull.VFcat.AvFreq, lmFull.VFcat.AvFreq.during)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: zComp.Cat ~ Age.Category * CR.composite.before + zStroop.SRC + 
    ##     zWM.Score + zSoP.comp
    ## Model 2: zComp.Cat ~ Age.Category * CR.composite.during + zStroop.SRC + 
    ##     zWM.Score + zSoP.comp
    ##   Res.Df    RSS Df Sum of Sq F Pr(>F)
    ## 1     81 15.382                      
    ## 2     81 15.868  0  -0.48648

Models are not significantly different from each other.

``` r
AIC(lmFull.VFcat.AvFreq)
```

    ## [1] 116.4126

``` r
AIC(lmFull.VFcat.AvFreq.during)
```

    ## [1] 119.2149

Although very similar, the model with the CR composite score
pre-pandemic seems to fit the data slightly better. (Lower AIC indicates
better fit)

## Verbal Fluency - Letters

``` r
## Create dataset that only includes data of the Subtlex Average Frequency of the produced words
VFlet_AvFreq <- VFlet %>%
  dplyr::filter(Measures=="AvWordFreq_subtlex") %>%
  dplyr::select(-Measures)

# head(VFlet_AvFreq, L=6)
```

### Descriptives

``` r
(Descr_VFlet <- VFlet_AvFreq %>%
  #Per Age Group
  group_by(Age.Category) %>%
  summarise(Nppt = length(unique(ID)), #Number of participants
            total = mean(Total, na.rm=T), #Total words correctly produced
            ztotal = mean(zComp.Let, na.rm=T), #Z-score per age group of total words correctly produced
            letterM = mean(M, na.rm=T), #Total words correctly produced for the letter M
            letterS = mean(S, na.rm=T), #Total words correctly produced for the letter S
            letterP = mean(P, na.rm=T))) #Total words correctly produced for the letter P
```

    ## # A tibble: 3 x 7
    ##   Age.Category        Nppt total  ztotal letterM letterS letterP
    ##   <chr>              <int> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 18 to 30 years old    30  4.22  0.264     4.25    4.24    4.17
    ## 2 40 to 55 years old    30  4.08 -0.0355    4.07    4.13    4.03
    ## 3 65 to 80 years old    30  4.92 -0.0746    4.06    4.21    3.96

``` r
# write.csv(Descr_VFlet, "./Figures and Tables/Descr_VFlet_AvFreq.csv", row.names = F)
```

``` r
#Visualise z-distribution of the composite score for letter fluency Average word frequency
hist(VFlet_AvFreq$zComp.Let, breaks=20) #Composite z-score of Letter Fluency
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_hist_VFlet_zAvFreq-1.png)<!-- -->

#### Visualisation AvFreq Semantic Fluency

``` r
#Convert to long format for visualisation
VFlet_AvFreq.long <- VFlet_AvFreq %>%
  pivot_longer(cols=Total:S, names_to = "letter", values_to = "AvFreq")

# Boxplot VFlet AvFreq

# png(file="./Figures and Tables/Boxplot_VFletAvFreq.png",
# width=600, height=350)

(Boxplot_VF <- VFlet_AvFreq.long %>%
    dplyr::filter(letter!="Total") %>%
    ggplot(aes(x=letter, y=AvFreq, 
                                               fill = as.factor(Age.Category))) +
    geom_boxplot(colour="grey50")+
    stat_summary(aes(label=round(..y..,2), group=as.factor(Age.Category)), 
               fun=mean, geom = "label", size=4,
               fill="white", show.legend=NA, label.size=NA,
               position = position_dodge(.75), vjust=-3) +
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Letter",
         y = "AvFreq (raw scores)",
         title = "Average Word Frequency per Age Group and Letter")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old")))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFletAvFreq_rawPerCategory-1.png)<!-- -->

``` r
# dev.off()
```

``` r
# Boxplot VFlet AvFreq raw total
# png(file="./Figures and Tables/Boxplot_VFletAvFreq_RawTotal.png",
# width=600, height=350)

(Boxplot_VF <- VFlet_AvFreq.long %>%
    dplyr::filter(letter=="Total") %>%
    ggplot(aes(x=letter, y=AvFreq,fill = as.factor(Age.Category))) +
    geom_boxplot(outlier.shape = NA, colour="grey50")+
    stat_summary(aes(label=round(..y.., 2), group=as.factor(Age.Category)), 
               fun=mean, geom = "label", size=4,
               fill="white", show.legend=NA, label.size=NA,
               position = position_dodge(.75), vjust=-3) +
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Letter",
         y = "AvFreq (raw scores)",
         title = "Average Word Frequency for All Letters per Age Group")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old"))+
  coord_cartesian(ylim = c(3,7)))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFletAvFreq_rawTotal-1.png)<!-- -->

``` r
# dev.off()
```

*Figures AvFreq Letter Fluency zscores*

``` r
#Convert to long format for visualisation
VFlet_AvFreq.long.zscores <- VFlet_AvFreq %>%
  pivot_longer(cols=zComp.Let:zP, names_to = "zletter", values_to = "zAvFreq")

# Boxplot VFlet AvFreq
# png(file="./Figures and Tables/Boxplot_VFletAvFreq_zscores.png",
# width=600, height=350)

(Boxplot_VFlet.zscores <- VFlet_AvFreq.long.zscores %>%
    dplyr::filter(zletter!="zComp.Let") %>%
    ggplot(aes(x=zletter, y=zAvFreq, 
                                               fill = as.factor(Age.Category))) +
    geom_boxplot(colour="grey50")+
    # stat_summary(aes(label=round(..y..,2), group=as.factor(Age.Category)), 
    #            fun=mean, geom = "label", size=4,
    #            fill="white", show.legend=NA, label.size=NA,
    #            position = position_dodge(.75), vjust=-3) +
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Letter",
         y = "AvFreq (z scores)",
         title = "Average Word Frequency per Age Group and Letter")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old")))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFletAvFreq_zPerCategory-1.png)<!-- -->

``` r
# dev.off()
```

``` r
# Boxplot VFlet AvFreq raw total
# png(file="./Figures and Tables/Boxplot_VFletAvFreq_Total_zscores.png",
# width=600, height=350)

(Boxplot_VFlet.zscoreTotal <- VFlet_AvFreq.long.zscores %>%
    dplyr::filter(zletter=="zComp.Let") %>%
    ggplot(aes(x=zletter, y=zAvFreq,fill = as.factor(Age.Category))) +
    geom_boxplot(colour="grey50")+
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Composite Letter Fluency",
         y = "AvFreq (z scores)",
         title = "Average Word Frequency for All Letters per Age Group")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old"))+
  coord_cartesian(ylim = c(-3,3)))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFleatAvFreq_zTotal-1.png)<!-- -->

``` r
# dev.off()
```

## Multiple Linear Regression - Letter Fluency

*Unconditional model, i.e. without covariates*

``` r
lmUncond.VFlet.AvFreq <- lm(zComp.Let ~ Age.Category*CR.composite.before, data = VFlet_AvFreq)
# broom::tidy(lmUncond.VFlet.AvFreq, conf.int=T)
```

*Full model of AvFreq in z-distribution, including covariates*

``` r
lmFull.VFlet.AvFreq <- lm(zComp.Let ~ Age.Category*CR.composite.before + zStroop.SRC + 
                              zWM.Score + zSoP.comp, data = VFlet_AvFreq)
#Tidy table output
(tidylmFull.VFlet.AvFreq <- broom::tidy(lmFull.VFlet.AvFreq, conf.int=T))
```

    ## # A tibble: 9 x 7
    ##   term                   estimate std.error statistic p.value conf.low conf.high
    ##   <chr>                     <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)             0.322      0.140      2.30   0.0237   0.0440    0.599 
    ## 2 Age.Category40 to 55 ~ -0.334      0.178     -1.88   0.0643  -0.689     0.0203
    ## 3 Age.Category65 to 80 ~ -0.462      0.225     -2.05   0.0438  -0.910    -0.0132
    ## 4 CR.composite.before     0.0988     0.118      0.839  0.404   -0.135     0.333 
    ## 5 zStroop.SRC            -0.0603     0.0784    -0.770  0.444   -0.216     0.0956
    ## 6 zWM.Score               0.00877    0.0861     0.102  0.919   -0.163     0.180 
    ## 7 zSoP.comp               0.142      0.126      1.13   0.262   -0.108     0.392 
    ## 8 Age.Category40 to 55 ~ -0.0938     0.166     -0.564  0.575   -0.425     0.237 
    ## 9 Age.Category65 to 80 ~ -0.229      0.168     -1.36   0.177   -0.564     0.105

``` r
# write.csv(tidylmFull.VFlet.AvFreq, "./Figures and Tables/VFlet_zAvFreq_lmFull.csv")
```

*Full model of AvFreq as raw score, including covariates*

``` r
lmFull.VFlet.AvFreq.raw <- lm(Total ~ Age.Category*CR.composite.before + zStroop.SRC + 
                              zWM.Score + zSoP.comp, data = VFlet_AvFreq)
#Tidy table output
broom::tidy(lmFull.VFlet.AvFreq.raw, conf.int=T)
```

    ## # A tibble: 9 x 7
    ##   term                  estimate std.error statistic  p.value conf.low conf.high
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)             4.14       0.347    11.9   1.62e-19    3.45      4.83 
    ## 2 Age.Category40 to 55~  -0.111      0.443    -0.251 8.02e- 1   -0.993     0.770
    ## 3 Age.Category65 to 80~   0.862      0.560     1.54  1.28e- 1   -0.253     1.98 
    ## 4 CR.composite.before     0.0487     0.293     0.167 8.68e- 1   -0.533     0.631
    ## 5 zStroop.SRC             0.194      0.195     0.995 3.23e- 1   -0.194     0.581
    ## 6 zWM.Score              -0.168      0.214    -0.784 4.35e- 1   -0.594     0.258
    ## 7 zSoP.comp              -0.262      0.312    -0.837 4.05e- 1   -0.883     0.360
    ## 8 Age.Category40 to 55~  -0.0594     0.414    -0.144 8.86e- 1   -0.883     0.764
    ## 9 Age.Category65 to 80~  -0.320      0.418    -0.764 4.47e- 1   -1.15      0.512

The model doesn’t seem to predict the raw Total score for Verbal Fluency
Letter. However, in the Full model with the z-composite score, older
adults seem to produce significantly higher frequent words than younger
adults.

Let’s check the model assumptions + fit.

*Assumption 1 - Linearity*

``` r
## Unconditional model with z composite score
plot(lmUncond.VFlet.AvFreq, 1, main = "Unconditional model")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_Linearity_LMunconditional-1.png)<!-- -->

``` r
## Full model with z composite score
plot(lmFull.VFlet.AvFreq, 1, main = "Full model (z-score composite score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_Linearity_LMzfull-1.png)<!-- -->

``` r
## Full model with raw total score
plot(lmFull.VFlet.AvFreq.raw, 1, main = "Full model (raw total score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_Linearity_LMrawfull-1.png)<!-- -->

For the models with the z-composite score as outcome variable, the red
line is mostly horizontal, meaning that there is a linear relationship
between the fitted line and the residual value. The model with the raw
average frequency scores seems to violate the assumption.

*Assumption 2 - Independence of Variables*

``` r
## z composite of VF let
## Create table with correlation values between predictor variables
tibble::as_tibble(cor(VFlet_AvFreq[,c(5,16, 21, 23,24)]), rownames="rowname")
```

    ## # A tibble: 5 x 6
    ##   rowname             zComp.Let zStroop.SRC zSoP.comp zWM.Score CR.composite.be~
    ##   <chr>                   <dbl>       <dbl>     <dbl>     <dbl>            <dbl>
    ## 1 zComp.Let             1           -0.162    -0.0657  -0.00699         -0.0146 
    ## 2 zStroop.SRC          -0.162        1         0.432    0.116            0.0483 
    ## 3 zSoP.comp            -0.0657       0.432     1        0.144            0.0228 
    ## 4 zWM.Score            -0.00699      0.116     0.144    1               -0.00547
    ## 5 CR.composite.before  -0.0146       0.0483    0.0228  -0.00547          1

``` r
## Create correlation plot between predictor variables
corrplot(cor(VFlet_AvFreq[,c(5,16, 21, 23,24)]),method='circle')
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_corPredictors_VFletAvFreq_zScores-1.png)<!-- -->

``` r
## Raw scores
## Create table with correlation values between predictor variables
tibble::as_tibble(cor(VFlet_AvFreq[,c(9, 16, 21, 23,24)]), rownames = "rowname")
```

    ## # A tibble: 5 x 6
    ##   rowname               Total zStroop.SRC zSoP.comp zWM.Score CR.composite.befo~
    ##   <chr>                 <dbl>       <dbl>     <dbl>     <dbl>              <dbl>
    ## 1 Total                1           0.116     0.0763  -0.0994            -0.0456 
    ## 2 zStroop.SRC          0.116       1         0.432    0.116              0.0483 
    ## 3 zSoP.comp            0.0763      0.432     1        0.144              0.0228 
    ## 4 zWM.Score           -0.0994      0.116     0.144    1                 -0.00547
    ## 5 CR.composite.before -0.0456      0.0483    0.0228  -0.00547            1

``` r
## Create correlation plot between predictor variables
corrplot(cor(VFlet_AvFreq[,c(9,16, 21, 23,24)]))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_corPredictors_VFletAvFreq_rawScores-1.png)<!-- -->

There seems to be no strong relationship between the predictor
variables. Hence, we can assume Independence of Variables.

*Assumption 3 - Normal Distribution of Residuals*

Full model with z composite score

``` r
plot(lmFull.VFlet.AvFreq, 2, main = "Full model (z-score composite score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_Normality_zScores-1.png)<!-- -->
Full model with raw total score

``` r
plot(lmFull.VFlet.AvFreq.raw, 2, main = "Full model (raw total score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_Normality_rawScores-1.png)<!-- -->

For the unconditional and full model with the z-score as outcome
variable, the points seem to roughly follow a straight line, except for
two outliers on right. So, these two models seem to roughly meet the
assumption. The full model with the raw score as outcome variable, there
are three massive outliers on the right. Hence, other
relationships/predictors that have not been included into the models
could explain the variance.

*Assumption 4 - Homoscedasticity or Equal Variance of Variables*

Full model with z composite score

``` r
plot(lmFull.VFlet.AvFreq, 3, main = "Full model (z-score composite score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_Homoscedasticity_zScores-1.png)<!-- -->
Full model with raw total score

``` r
plot(lmFull.VFlet.AvFreq.raw, 3, main = "Full model (raw total score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_Homoscedasticity_rawScores-1.png)<!-- -->
The full model with the raw score seem to violete the assumption of
Homoscedasticity So, the variance of residuals does not seem equal
across the predictors. The full model with the z-scores does seem to
meet the assumption of Homoscedasticity. Hence, the error terms are
relatively the same across all values of the independent variable for
this model.

Conclusion: only the full model with the outcome variable along the
z-distribution seems to meet all the assumptions.

#### Model fit diagnostics

*Variation Inflation Factor*

Any VIF value above 4 needs further investigation. There seems to be no
concerning signs of collinearity (VIF values higher than 4). The
tolerance values indicate the percentage of variance that cannot be
explained for by the other predictor variables.

``` r
ols_vif_tol(lmFull.VFlet.AvFreq)
```

    ##                                            Variables Tolerance      VIF
    ## 1                     Age.Category40 to 55 years old 0.6255213 1.598667
    ## 2                     Age.Category65 to 80 years old 0.3910939 2.556931
    ## 3                                CR.composite.before 0.3297057 3.033008
    ## 4                                        zStroop.SRC 0.7584502 1.318478
    ## 5                                          zWM.Score 0.9498853 1.052759
    ## 6                                          zSoP.comp 0.4868349 2.054084
    ## 7 Age.Category40 to 55 years old:CR.composite.before 0.4944811 2.022322
    ## 8 Age.Category65 to 80 years old:CR.composite.before 0.4842401 2.065091

*Plot Diagnositcs Full model with z composite score for Semantic
Fluency*

``` r
ols_plot_diagnostics(lmFull.VFlet.AvFreq)
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_lmFull_diagnostics-1.png)<!-- -->![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_lmFull_diagnostics-2.png)<!-- -->![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFletAvFreq_lmFull_diagnostics-3.png)<!-- -->

The Observed vs Predicted Plot shows that the model doesn’t fit the data
very well (pity) –&gt; due to outliers?? What to do…. –&gt; perhaps
identify outliers using the Cook chart and residual plot and rerun the
models?

### Model comparisons for the CR measure preciding and coinciding with the COVID-19 pandemic

The effect of the COVID-19 pandemic on CR and subsequent behavioural
performance

``` r
lmFull.VFlet.AvFreq.during <- lm(zComp.Let ~ Age.Category*CR.composite.during + 
                                   zStroop.SRC + zWM.Score + zSoP.comp, data = 
                                   VFlet_AvFreq)
summary(lmFull.VFlet.AvFreq.during)
```

    ## 
    ## Call:
    ## lm(formula = zComp.Let ~ Age.Category * CR.composite.during + 
    ##     zStroop.SRC + zWM.Score + zSoP.comp, data = VFlet_AvFreq)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.62951 -0.38516 -0.02094  0.29871  1.63947 
    ## 
    ## Coefficients:
    ##                                                     Estimate Std. Error t value
    ## (Intercept)                                         0.337357   0.140576   2.400
    ## Age.Category40 to 55 years old                     -0.347366   0.179174  -1.939
    ## Age.Category65 to 80 years old                     -0.493763   0.227370  -2.172
    ## CR.composite.during                                 0.049872   0.121360   0.411
    ## zStroop.SRC                                        -0.059932   0.079434  -0.754
    ## zWM.Score                                           0.001874   0.086080   0.022
    ## zSoP.comp                                           0.167237   0.126418   1.323
    ## Age.Category40 to 55 years old:CR.composite.during  0.012475   0.169774   0.073
    ## Age.Category65 to 80 years old:CR.composite.during -0.179392   0.172791  -1.038
    ##                                                    Pr(>|t|)  
    ## (Intercept)                                          0.0187 *
    ## Age.Category40 to 55 years old                       0.0560 .
    ## Age.Category65 to 80 years old                       0.0328 *
    ## CR.composite.during                                  0.6822  
    ## zStroop.SRC                                          0.4527  
    ## zWM.Score                                            0.9827  
    ## zSoP.comp                                            0.1896  
    ## Age.Category40 to 55 years old:CR.composite.during   0.9416  
    ## Age.Category65 to 80 years old:CR.composite.during   0.3023  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6315 on 81 degrees of freedom
    ## Multiple R-squared:  0.09696,    Adjusted R-squared:  0.007766 
    ## F-statistic: 1.087 on 8 and 81 DF,  p-value: 0.3807

``` r
# Model comparison
anova(lmFull.VFlet.AvFreq, lmFull.VFlet.AvFreq.during)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: zComp.Let ~ Age.Category * CR.composite.before + zStroop.SRC + 
    ##     zWM.Score + zSoP.comp
    ## Model 2: zComp.Let ~ Age.Category * CR.composite.during + zStroop.SRC + 
    ##     zWM.Score + zSoP.comp
    ##   Res.Df    RSS Df Sum of Sq F Pr(>F)
    ## 1     81 32.181                      
    ## 2     81 32.299  0  -0.11824

Models are not significantly different from each other.

``` r
# Mode comparison with AIC
AIC(lmFull.VFlet.AvFreq)
```

    ## [1] 182.8506

``` r
AIC(lmFull.VFlet.AvFreq.during)
```

    ## [1] 183.1807

Although very similar, the model with the CR composite score
pre-pandemic seems to fit the data slightly better.

## Verbal Fluency - Actions

### Descriptive Statistics

``` r
## Create dataset that only includes data of the Subtlex Average Frequency of the produced words
VFact_AvFreq <- VFact %>%
  dplyr::filter(Measures=="AvWordFreq_subtlex") %>%
  #Remove column called Measures (which now only contains "AvWordFreq_subtlex")
  dplyr::select(-Measures)

# head(VFact_AvFreq, L=6)
```

``` r
#Create a descriptives table by summarising data of the semantic fluency task
(Descr_VFact <- VFact_AvFreq %>%
#Per Age Group
  group_by(Age.Category) %>%
  summarise(Nppt = length(unique(ID)), #Number of participants
            total = mean(Total, na.rm=T), #Total words correctly produced
            ztotal = mean(zComp.Act, na.rm=T), #Z-score per age group of total words correctly produced
            people = mean(Things.people.do, na.rm=T), #Total words correctly produced in category "Things people do"
            eggs = mean(Egg, na.rm=T))) #Total words correctly produced in category "Things you can do to an egg"
```

    ## # A tibble: 3 x 6
    ##   Age.Category        Nppt total  ztotal people  eggs
    ##   <chr>              <int> <dbl>   <dbl>  <dbl> <dbl>
    ## 1 18 to 30 years old    30  4.20 0.0152    4.40  4.00
    ## 2 40 to 55 years old    30  4.21 0.0505    4.41  4.00
    ## 3 65 to 80 years old    30  4.32 0.00348   4.35  3.99

``` r
# write.csv(Descr_VFact, "./Figures and Tables/Descr_VFact_AvFreq.csv")
```

``` r
hist(VFact_AvFreq$zComp.Act, breaks=20) #Composite z-score of Semantic Fluency
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_hist_VFactAvFreq_zScores-1.png)<!-- -->

#### Visualisation AvFreq Action Fluency

``` r
#Convert to long format for visualisation
VFact_AvFreq.long <- VFact_AvFreq %>%
  pivot_longer(cols=Total:Egg, names_to = "action", values_to = "AvFreq")

#Boxplot VF act AvFreq
# png(file="./Figures and Tables/Boxplot_VFactAvFreq.png",
# width=600, height=350)

(Boxplot_VF <- VFact_AvFreq.long %>%
    dplyr::filter(action!="Total") %>%
    ggplot(aes(x=factor(action), y=AvFreq, 
                                               fill = as.factor(Age.Category))) +
    geom_boxplot(colour="grey50")+
    stat_summary(aes(label=round(..y..,2), group=as.factor(Age.Category)), 
               fun=mean, geom = "label", size=4,
               fill="white", show.legend=NA, label.size=NA,
               position = position_dodge(.75), vjust=-3) +
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Action Category",
         y = "AvFreq (raw scores)",
         title = "Average Word frequency per Age Group and Action Category")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old")))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFactAvFreq_rawPerCategory-1.png)<!-- -->

``` r
# dev.off()
```

``` r
#Boxplot VFact Raw Total
# png(file="./Figures and Tables/Boxplot_VFactAvFreq_RawTotal.png",
# width=600, height=350)

(Boxplot_VF <- VFact_AvFreq.long %>%
    dplyr::filter(action=="Total") %>%
    ggplot(aes(x=action, y=AvFreq,fill = as.factor(Age.Category))) +
    geom_boxplot(colour="grey50")+
    stat_summary(aes(label=round(..y..,2), group=as.factor(Age.Category)), 
               fun=mean, geom = "label", size=4,
               fill="white", show.legend=NA, label.size=NA,
               position = position_dodge(.75), vjust=-3) +
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "",
         y = "AvFreq (raw scores)",
         title = "Average Word Frequency for Both Action Categories per Age Group")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old"))+
  coord_cartesian(ylim = c(3,5)))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFactAvFreq_rawTotal-1.png)<!-- -->

``` r
# dev.off()
```

*Figures AvFreq Action Fluency zscores*

``` r
#Convert to long format for visualisation
VFact_AvFreq.long.zscores <- VFact_AvFreq %>%
  pivot_longer(cols=zComp.Act:zEggs, names_to = "zaction", values_to = "zAvFreq")

#Boxplot VF act AvFreq
# png(file="./Figures and Tables/Boxplot_VFactAvFreq_zscores.png",
# width=600, height=350)

(Boxplot_VF_sub.zscores <- VFact_AvFreq.long.zscores %>%
    dplyr::filter(zaction!="zComp.Act") %>%
    ggplot(aes(x=factor(zaction), y=zAvFreq, 
                                               fill = as.factor(Age.Category))) +
    geom_boxplot(colour="grey50")+
    stat_summary(aes(label=round(..y..,2), group=as.factor(Age.Category)), 
               fun=mean, geom = "label", size=4,
               fill="white", show.legend=NA, label.size=NA,
               position = position_dodge(.75), vjust=-3) +
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Action Category",
         y = "AvFreq (z scores)",
         title = "Average Word frequency per Age Group and Action Category")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old")))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFactAvFreq_zPerCategory-1.png)<!-- -->

``` r
# dev.off()
```

``` r
#Boxplot VFact Raw Total
# png(file="./Figures and Tables/Boxplot_VFactAvFreq_Total_zscores.png",
# width=600, height=350)

(Boxplot_VF_comp.zscores <- VFact_AvFreq.long.zscores %>%
    dplyr::filter(zaction=="zComp.Act") %>%
    ggplot(aes(x=zaction, y=zAvFreq,fill = as.factor(Age.Category))) +
    geom_boxplot(colour="grey50")+
      stat_summary(fun = "mean", position = position_dodge(.75), 
               show.legend=F, colour="white")+ #Mean as white dot
    labs(x = "Composite Action Fluency",
         y = "AvFreq (z scores)",
         title = "Average Word Frequency for Both Action Categories per Age Group")+
        scale_fill_discrete(guide=guide_legend(title = "Age Group"), labels=c("18 to 30 years old","40 to 55 years old", "65 to 80 years old"))+
  coord_cartesian(ylim = c(-3,3)))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_Boxplot_VFactAvFreq_zTotal-1.png)<!-- -->

``` r
# dev.off()
```

## Multiple Linear Regression - Action Fluency

*Unconditional model, i.e. without covariates*

``` r
lmUncond.VFact.AvFreq <- lm(zComp.Act ~ Age.Category*CR.composite.before, data = VFact_AvFreq)
# broom::tidy(lmUncond.VFact.AvFreq, conf.int=T)
```

*Full model of AvFreq in z-distribution, including covariates*

``` r
lmFull.VFact.AvFreq <- lm(zComp.Act ~ Age.Category*CR.composite.before + zStroop.SRC + 
                              zWM.Score + zSoP.comp, data = VFact_AvFreq)
#Tidy table output
(tidylmFull.VFact.AvFreq <- broom::tidy(lmFull.VFact.AvFreq, conf.int=T))
```

    ## # A tibble: 9 x 7
    ##   term                   estimate std.error statistic p.value conf.low conf.high
    ##   <chr>                     <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)              0.0358    0.148      0.242   0.809   -0.258    0.330 
    ## 2 Age.Category40 to 55 ~   0.0309    0.189      0.164   0.870   -0.344    0.406 
    ## 3 Age.Category65 to 80 ~  -0.0782    0.239     -0.328   0.744   -0.553    0.396 
    ## 4 CR.composite.before      0.0449    0.125      0.360   0.720   -0.203    0.293 
    ## 5 zStroop.SRC             -0.0457    0.0829    -0.551   0.583   -0.211    0.119 
    ## 6 zWM.Score               -0.112     0.0912    -1.23    0.223   -0.293    0.0693
    ## 7 zSoP.comp                0.0933    0.133      0.702   0.485   -0.171    0.358 
    ## 8 Age.Category40 to 55 ~  -0.0789    0.176     -0.448   0.656   -0.429    0.272 
    ## 9 Age.Category65 to 80 ~  -0.0641    0.178     -0.360   0.720   -0.418    0.290

``` r
# write.csv(tidylmFull.VFact.AvFreq, "./Figures and Tables/VFact_zAvFreq_lmFull.csv")
```

*Full model of AvFreq as raw score, including covariates*

``` r
lmFull.VFact.AvFreq.raw <- lm(Total ~ Age.Category*CR.composite.before + zStroop.SRC + 
                              zWM.Score + zSoP.comp, data = VFact_AvFreq)
#Tidy table output
broom::tidy(lmFull.VFact.AvFreq.raw, conf.int=T)
```

    ## # A tibble: 9 x 7
    ##   term                  estimate std.error statistic  p.value conf.low conf.high
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)            4.12       0.119    34.7    2.10e-50    3.89     4.36  
    ## 2 Age.Category40 to 55~  0.0677     0.152     0.446  6.56e- 1   -0.234    0.370 
    ## 3 Age.Category65 to 80~  0.268      0.192     1.40   1.66e- 1   -0.114    0.650 
    ## 4 CR.composite.before    0.0367     0.100     0.366  7.15e- 1   -0.163    0.236 
    ## 5 zStroop.SRC            0.00150    0.0667    0.0225 9.82e- 1   -0.131    0.134 
    ## 6 zWM.Score             -0.0123     0.0733   -0.167  8.68e- 1   -0.158    0.134 
    ## 7 zSoP.comp             -0.119      0.107    -1.12   2.68e- 1   -0.332    0.0935
    ## 8 Age.Category40 to 55~ -0.0420     0.142    -0.296  7.68e- 1   -0.324    0.240 
    ## 9 Age.Category65 to 80~ -0.0483     0.143    -0.338  7.37e- 1   -0.333    0.237

None of the models seem to predict either the outcome variable for
Verbal Fluency Action Categories. Let’s check the model assumptions +
fit.

*Assumption 1 - Linearity*

``` r
## Unconditional model with z composite score
plot(lmUncond.VFact.AvFreq, 1, main = "Unconditional model")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_Linearity_LMunconditional-1.png)<!-- -->

``` r
## Full model with z composite score
plot(lmFull.VFact.AvFreq, 1, main = "Full model (z-score composite score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_Linearity_LMzfull-1.png)<!-- -->

``` r
## Full model with raw total score
plot(lmFull.VFact.AvFreq.raw, 1, main = "Full model (raw total score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_Linearity_LMrawfull-1.png)<!-- -->
For the unconditional model and the full model with raw scores, the red
line is mostly horizontal, meaning that there is a linear relationship
between the fitted line and the residual value. However, residuals seem
to be higher for the extreme min and max fitted values. Hence, we can’t
assume linearity. Possibly due to outliers?

*Assumption 2 - Independence of Variables*

``` r
# Z composite of VF action
## Create table with correlation values between predictor variables
tibble::as_tibble(cor(VFact_AvFreq[,c(5,14,19,21,23)]), rownames="rowname")
```

    ## # A tibble: 5 x 6
    ##   rowname             zComp.Act zStroop.SRC zSoP.comp zWM.Score CR.composite.be~
    ##   <chr>                   <dbl>       <dbl>     <dbl>     <dbl>            <dbl>
    ## 1 zComp.Act             1           -0.0584    0.0228  -0.131           -0.00435
    ## 2 zStroop.SRC          -0.0584       1         0.432    0.116            0.0483 
    ## 3 zSoP.comp             0.0228       0.432     1        0.144            0.0228 
    ## 4 zWM.Score            -0.131        0.116     0.144    1               -0.00547
    ## 5 CR.composite.before  -0.00435      0.0483    0.0228  -0.00547          1

``` r
## Create correlation plot between predictor variables
corrplot(cor(VFact_AvFreq[,c(5,14,19,21,23)]),method='circle')
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_corPredictors_VFactAvFreq_zScores-1.png)<!-- -->

``` r
## Raw scores
## Create table with correlation values between predictor variables
tibble::as_tibble(cor(VFact_AvFreq[,c(8,14,19,21,23)]), rownames = "rowname")
```

    ## # A tibble: 5 x 6
    ##   rowname                Total zStroop.SRC zSoP.comp zWM.Score CR.composite.bef~
    ##   <chr>                  <dbl>       <dbl>     <dbl>     <dbl>             <dbl>
    ## 1 Total                1          -0.00314   -0.0285  -0.0412            0.00883
    ## 2 zStroop.SRC         -0.00314     1          0.432    0.116             0.0483 
    ## 3 zSoP.comp           -0.0285      0.432      1        0.144             0.0228 
    ## 4 zWM.Score           -0.0412      0.116      0.144    1                -0.00547
    ## 5 CR.composite.before  0.00883     0.0483     0.0228  -0.00547           1

``` r
## Create correlation plot between predictor variables
corrplot(cor(VFact_AvFreq[,c(8,14,19,21,23)]))
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_corPredictors_VFactAvFreq_rawScores-1.png)<!-- -->

There seems to be no strong relationship between the predictor
variables. Hence, we can assume Independence of Variables.

*Assumption 3 - Normal Distribution of Residuals*

Full model with z composite score

``` r
## Full model with z composite score
plot(lmFull.VFact.AvFreq, 2, main = "Full model (z-score composite score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_Normality_zScores-1.png)<!-- -->

Full model with raw total score

``` r
plot(lmFull.VFact.AvFreq.raw, 2, main = "Full model (raw total score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_Normality_rawScores-1.png)<!-- -->

For the unconditional and full model with z-composite score, the points
seem to roughly follow a straight line. However, there is a dip on the
right and an influential point (point 12). Hence, other
relationships/predictors that have not been included into the models
could explain the variance for these models. One explanation could be
the online format of the study. For example, some participants might
have scored lower than they would in a lab-based study, due to unclear
instructions. Also, it could have caused more distractions than there
would have been in a lab based study. For the full model with the raw
scores, there is one massive outlier (point 7).

*Assumption 4 - Homoscedasticity or Equal Variance of Variables*

Full model with z composite score

``` r
plot(lmFull.VFact.AvFreq, 3, main = "Full model (z-score composite score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_Homoscedasticity_zScores-1.png)<!-- -->

Full model with raw total score

``` r
plot(lmFull.VFact.AvFreq.raw, 3, main = "Full model (raw total score)")
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_Homoscedasticity_rawScores-1.png)<!-- -->

#### Model fit diagnostics

*Variation Inflation Factor*

Any VIF value above 4 needs further investigation. There seems to be no
concerning signs of collinearity (VIF values higher than 4). The
tolerance values indicate the percentage of variance that cannot be
explained for by the other predictor variables.

``` r
ols_vif_tol(lmFull.VFact.AvFreq) #zscores only 
```

    ##                                            Variables Tolerance      VIF
    ## 1                     Age.Category40 to 55 years old 0.6255213 1.598667
    ## 2                     Age.Category65 to 80 years old 0.3910939 2.556931
    ## 3                                CR.composite.before 0.3297057 3.033008
    ## 4                                        zStroop.SRC 0.7584502 1.318478
    ## 5                                          zWM.Score 0.9498853 1.052759
    ## 6                                          zSoP.comp 0.4868349 2.054084
    ## 7 Age.Category40 to 55 years old:CR.composite.before 0.4944811 2.022322
    ## 8 Age.Category65 to 80 years old:CR.composite.before 0.4842401 2.065091

*Plot Diagnositcs Full model with z composite score for Action Fluency*

``` r
ols_plot_diagnostics(lmFull.VFact.AvFreq)
```

![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_lmFull_diagnostics-1.png)<!-- -->![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_lmFull_diagnostics-2.png)<!-- -->![](GBGon_VFAvFreq_figs/VFAvFreq_figs_VFactAvFreq_lmFull_diagnostics-3.png)<!-- -->

For all models, the Observed vs Predicted Plot shows that the model
doesn’t fit the data very well (pity) –&gt; due to outliers?? What to
do….

It can explain the fact that the models do not explain the outcome
variable in any case.

### Model comparisons for the CR measure preciding and coinciding with the COVID-19 pandemic

The effect of the COVID-19 pandemic on CR and subsequent behavioural
performance

``` r
lmFull.VFact.AvFreq.during <- lm(zComp.Act ~ Age.Category*CR.composite.during + 
                                   zStroop.SRC + zWM.Score + zSoP.comp, data = 
                                   VFact_AvFreq)
summary(lmFull.VFact.AvFreq.during)
```

    ## 
    ## Call:
    ## lm(formula = zComp.Act ~ Age.Category * CR.composite.during + 
    ##     zStroop.SRC + zWM.Score + zSoP.comp, data = VFact_AvFreq)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.66685 -0.42898  0.02427  0.43403  1.28112 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value
    ## (Intercept)                                         0.03217    0.14819   0.217
    ## Age.Category40 to 55 years old                      0.03402    0.18887   0.180
    ## Age.Category65 to 80 years old                     -0.07118    0.23968  -0.297
    ## CR.composite.during                                 0.07270    0.12793   0.568
    ## zStroop.SRC                                        -0.04644    0.08373  -0.555
    ## zWM.Score                                          -0.11657    0.09074  -1.285
    ## zSoP.comp                                           0.08808    0.13326   0.661
    ## Age.Category40 to 55 years old:CR.composite.during -0.12630    0.17897  -0.706
    ## Age.Category65 to 80 years old:CR.composite.during -0.04116    0.18215  -0.226
    ##                                                    Pr(>|t|)
    ## (Intercept)                                           0.829
    ## Age.Category40 to 55 years old                        0.857
    ## Age.Category65 to 80 years old                        0.767
    ## CR.composite.during                                   0.571
    ## zStroop.SRC                                           0.581
    ## zWM.Score                                             0.203
    ## zSoP.comp                                             0.510
    ## Age.Category40 to 55 years old:CR.composite.during    0.482
    ## Age.Category65 to 80 years old:CR.composite.during    0.822
    ## 
    ## Residual standard error: 0.6657 on 81 degrees of freedom
    ## Multiple R-squared:  0.03417,    Adjusted R-squared:  -0.06122 
    ## F-statistic: 0.3582 on 8 and 81 DF,  p-value: 0.9393

``` r
#Model comparison
anova(lmFull.VFact.AvFreq, lmFull.VFact.AvFreq.during)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: zComp.Act ~ Age.Category * CR.composite.before + zStroop.SRC + 
    ##     zWM.Score + zSoP.comp
    ## Model 2: zComp.Act ~ Age.Category * CR.composite.during + zStroop.SRC + 
    ##     zWM.Score + zSoP.comp
    ##   Res.Df    RSS Df Sum of Sq F Pr(>F)
    ## 1     81 36.046                      
    ## 2     81 35.891  0   0.15442

Models are not significantly different from each other.

``` r
#Model comparison through AIC comparison
AIC(lmFull.VFact.AvFreq)
```

    ## [1] 193.0574

``` r
AIC(lmFull.VFact.AvFreq.during)
```

    ## [1] 192.671

Although very similar, the model with the CR composite score
during-pandemic seems to fit the data slightly better.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-R-rio" class="csl-entry">

Chan, Chung-hong, and Thomas J. Leeper. 2021. *Rio: A Swiss-Army Knife
for Data i/o*. <https://github.com/leeper/rio>.

</div>

<div id="ref-car2019" class="csl-entry">

Fox, John, and Sanford Weisberg. 2019. *An R Companion to Applied
Regression*. Third. Thousand Oaks CA: Sage.
<https://socialsciences.mcmaster.ca/jfox/Books/Companion/>.

</div>

<div id="ref-R-carData" class="csl-entry">

Fox, John, Sanford Weisberg, and Brad Price. 2020. *carData: Companion
to Applied Regression Data Sets*.
<https://CRAN.R-project.org/package=carData>.

</div>

<div id="ref-R-car" class="csl-entry">

———. 2021. *Car: Companion to Applied Regression*.
<https://CRAN.R-project.org/package=car>.

</div>

<div id="ref-R-olsrr" class="csl-entry">

Hebbali, Aravind. 2020. *Olsrr: Tools for Building OLS Regression
Models*. <https://CRAN.R-project.org/package=olsrr>.

</div>

<div id="ref-R-purrr" class="csl-entry">

Henry, Lionel, and Hadley Wickham. 2020. *Purrr: Functional Programming
Tools*. <https://CRAN.R-project.org/package=purrr>.

</div>

<div id="ref-R-fs" class="csl-entry">

Hester, Jim, and Hadley Wickham. 2020. *Fs: Cross-Platform File System
Operations Based on Libuv*. <https://CRAN.R-project.org/package=fs>.

</div>

<div id="ref-performance2021" class="csl-entry">

Lüdecke, Daniel, Mattan S. Ben-Shachar, Indrajeet Patil, Philip
Waggoner, and Dominique Makowski. 2021. “<span
class="nocase">performance</span>: An R Package for Assessment,
Comparison and Testing of Statistical Models.” *Journal of Open Source
Software* 6 (60): 3139. <https://doi.org/10.21105/joss.03139>.

</div>

<div id="ref-R-performance" class="csl-entry">

Lüdecke, Daniel, Dominique Makowski, Mattan S. Ben-Shachar, Indrajeet
Patil, Philip Waggoner, and Brenton M. Wiernik. 2021. *Performance:
Assessment of Regression Models Performance*.
<https://easystats.github.io/performance/>.

</div>

<div id="ref-R-e1071" class="csl-entry">

Meyer, David, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel, and
Friedrich Leisch. 2021. *E1071: Misc Functions of the Department of
Statistics, Probability Theory Group (Formerly: E1071), TU Wien*.
<https://CRAN.R-project.org/package=e1071>.

</div>

<div id="ref-R-tibble" class="csl-entry">

Müller, Kirill, and Hadley Wickham. 2021. *Tibble: Simple Data Frames*.
<https://CRAN.R-project.org/package=tibble>.

</div>

<div id="ref-R-base" class="csl-entry">

R Core Team. 2020. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-R-broom" class="csl-entry">

Robinson, David, Alex Hayes, and Simon Couch. 2021. *Broom: Convert
Statistical Objects into Tidy Tibbles*.
<https://CRAN.R-project.org/package=broom>.

</div>

<div id="ref-lattice2008" class="csl-entry">

Sarkar, Deepayan. 2008. *Lattice: Multivariate Data Visualization with
r*. New York: Springer. <http://lmdvr.r-forge.r-project.org>.

</div>

<div id="ref-R-lattice" class="csl-entry">

———. 2021. *Lattice: Trellis Graphics for r*.
<http://lattice.r-forge.r-project.org/>.

</div>

<div id="ref-R-corrplot" class="csl-entry">

Wei, Taiyun, and Viliam Simko. 2021a. *Corrplot: Visualization of a
Correlation Matrix*. <https://github.com/taiyun/corrplot>.

</div>

<div id="ref-corrplot2021" class="csl-entry">

———. 2021b. *R Package ’Corrplot’: Visualization of a Correlation
Matrix*. <https://github.com/taiyun/corrplot>.

</div>

<div id="ref-ggplot22016" class="csl-entry">

Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. <https://ggplot2.tidyverse.org>.

</div>

<div id="ref-R-stringr" class="csl-entry">

———. 2019. *Stringr: Simple, Consistent Wrappers for Common String
Operations*. <https://CRAN.R-project.org/package=stringr>.

</div>

<div id="ref-R-forcats" class="csl-entry">

———. 2021a. *Forcats: Tools for Working with Categorical Variables
(Factors)*. <https://CRAN.R-project.org/package=forcats>.

</div>

<div id="ref-R-tidyr" class="csl-entry">

———. 2021b. *Tidyr: Tidy Messy Data*.
<https://CRAN.R-project.org/package=tidyr>.

</div>

<div id="ref-R-tidyverse" class="csl-entry">

———. 2021c. *Tidyverse: Easily Install and Load the Tidyverse*.
<https://CRAN.R-project.org/package=tidyverse>.

</div>

<div id="ref-tidyverse2019" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-R-readxl" class="csl-entry">

Wickham, Hadley, and Jennifer Bryan. 2019. *Readxl: Read Excel Files*.
<https://CRAN.R-project.org/package=readxl>.

</div>

<div id="ref-R-ggplot2" class="csl-entry">

Wickham, Hadley, Winston Chang, Lionel Henry, Thomas Lin Pedersen,
Kohske Takahashi, Claus Wilke, Kara Woo, Hiroaki Yutani, and Dewey
Dunnington. 2021. *Ggplot2: Create Elegant Data Visualisations Using the
Grammar of Graphics*. <https://CRAN.R-project.org/package=ggplot2>.

</div>

<div id="ref-R-dplyr" class="csl-entry">

Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2021.
*Dplyr: A Grammar of Data Manipulation*.
<https://CRAN.R-project.org/package=dplyr>.

</div>

<div id="ref-R-readr" class="csl-entry">

Wickham, Hadley, and Jim Hester. 2021. *Readr: Read Rectangular Text
Data*. <https://CRAN.R-project.org/package=readr>.

</div>

</div>
