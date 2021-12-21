R Code Full Analysis Picture Naming Reaction Times
================
Elise Oosterhuis
Last compiled on 21/12/21

# Analysis Picture Naming Reaction Times GBG online study

##### Read in data

## Descriptives

*Mean and standard deviations picture naming tasks*

``` r
options(dplyr.summarise.inform = FALSE) #Suppress summarise message (`summarise()` has grouped output by 'Task.Name', 'Age.Category'. You can override using the `.groups` argument.) in output

# Combine both picture-naming datasets
PN_all <- rbind(PNobjects, PNactions) %>%
  convert(chr(type)) %>%
  dplyr::mutate(Task.Name=dplyr::recode(Task.Name, 'Picture Naming Task - Actions' #Change task names
                                = "Actions", 'Picture Naming Task - Objects' 
                                = "Objects")) %>%
  dplyr::mutate(Age.Category=as.factor(dplyr::recode(Age.Category, '18 to 30 years old'=1, #Recode age groups as numeric values
                                    '40 to 55 years old'=2,
                                    '65 to 80 years old'=3)))

#Filter out outliers for Reaction Time for the combined dataset
PN_RT <- PN_all %>%
  dplyr::filter(Acc==1) %>% #Acc==1 means only correctly answered trials will be included, i.e., removing all incorrect trials
#Create z scores for reaction time per age group (younger, middle-aged, and older adults) and per type (Actions and Objects separately)
  group_by(Age.Category, type) %>%
  mutate(zRT = scale(RT)) %>% 
#Use the z scores to filter out outliers (i.e., exclude values +/-2.5 SD per trial )
  filter(between(zRT, -2.5, +2.5)) %>% 
  ungroup()

# Mean and SD for picture naming
## Reaction Time (only correct trials; outliers excluded)
(PNrt_sum <- PN_RT %>%
    group_by(Task.Name, Age.Category) %>%
#Obtain mean and standard deviation for reaction time per task (actions and objects separately) and Age Group
    dplyr::summarise(mean_RT = round(mean(RT, na.rm=T),2),
            sd_RT = round(sd(RT,na.rm=T),2)))
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["Task.Name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Age.Category"],"name":[2],"type":["fct"],"align":["left"]},{"label":["mean_RT"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["sd_RT"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"Actions","2":"1","3":"1063.86","4":"348.30"},{"1":"Actions","2":"2","3":"1006.61","4":"312.40"},{"1":"Actions","2":"3","3":"1071.20","4":"326.84"},{"1":"Objects","2":"1","3":"763.57","4":"183.16"},{"1":"Objects","2":"2","3":"792.91","4":"222.38"},{"1":"Objects","2":"3","3":"880.98","4":"246.94"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
## Save output table as .csv fil
# write.csv(PNrt_sum, file = "./Figures and Tables/Descriptives_PNrt.csv", row.names = F)e
```

### Plots for Reaction Time

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### Missing data CR composite variable

The missing data for the CR composite variable posed a problem in that
it removed all data of participants with missing data in the LMEs. To
solve this problem, we agreed on replacing values with the means per age
category per CR subscale. As some participants were extreme outliers, we
decided to winsorize at -2.5 and 2.5 SD. In this way, extreme values
were still extreme values but wouldn’t be excluded and, hence, result in
missing values. This procedure led to no missing values and no excluded
participants in our statistical models.

## Statistical Analysis

### Linear Mixed Models - Action naming

``` r
# Only include trials for action naming (i.e., type==1)
PNactions_RT <- PN_RT %>%
  dplyr::filter(type==1)
  # Check
    # unique(PNactions_RT$Task.Name) #actions

## Create histogram for distribution
hist(PNactions_RT$RT, breaks = 100)
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### Check for Skewness seen the histograms

``` r
skewness(log(PNactions_RT$RT)) 
```

    ## [1] 0.3443259

Skewness for action naming is 0.242 –&gt; positively skewed but within
limits (Bryne, 2010; George & Mallery, 2010)

*Should we include random effects for ID and Trial.Number?*

``` r
#Base model with log-transformed reaction time
Mbase_act <- lm(log(RT) ~ 1, data=PNactions_RT)

#Base model with only ID/individual variability as random effect
Mrandom.ID_act <- lmer(log(RT) ~ 1 +(1|ID), data=PNactions_RT)
#Base model with only Trial.Number/trial variability as random effect
Mrandom.Trial_act <- lmer(log(RT) ~ 1 +(1|Trial.Number), data=PNactions_RT)
#Base model with both random effects
Mrandom.All_act <- lmer(log(RT) ~ 1 +(1|ID) + (1|Trial.Number), data=PNactions_RT)
```

AIC base model

``` r
#Obtain AIC values for each model
(AIC.base_act <- AIC(logLik(Mbase_act)))
```

    ## [1] 2660.319

AIC - only ID as random effect

``` r
(AIC.reID_act <- AIC(logLik(Mrandom.ID_act)))
```

    ## [1] 1773.477

AIC - only trial as random effect

``` r
(AIC.reTrial_act <- AIC(logLik(Mrandom.Trial_act)))
```

    ## [1] 1793.343

AIC - both ID and trial as random effect

``` r
(AIC.reBoth_act <- AIC(logLik(Mrandom.All_act)))
```

    ## [1] 603.5965

The AIC for the model including both random effects is lowest –&gt; we
justified inclusion of both Trial and Subject as random effects.

*Null model of reaction times for action naming with random effects
included*

``` r
M0_PNactRT <- lmer(RT ~ 1 +(1|ID) + (1|Trial.Number), data=PNactions_RT, REML=FALSE)
summary(M0_PNactRT)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: RT ~ 1 + (1 | ID) + (1 | Trial.Number)
    ##    Data: PNactions_RT
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  91855.2  91882.3 -45923.6  91847.2     6473 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4808 -0.6632 -0.1852  0.4395  5.0949 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev.
    ##  ID           (Intercept) 15602    124.9   
    ##  Trial.Number (Intercept) 16850    129.8   
    ##  Residual                 78421    280.0   
    ## Number of obs: 6477, groups:  ID, 90; Trial.Number, 79
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)  1053.35      19.99  153.51   52.69   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

*Unconditional model, i.e., the model without covariates/control
measures*

``` r
Muncond_PNactRT <- lmer(RT ~ Age.Category*CR.composite.before + (1|ID)  + (1|Trial.Number), data=PNactions_RT, REML=FALSE)
```

*Full model, i.e., model with covariates/control measures. RT not
log-transformed*

``` r
Mfull_PNact_RT <- lmer(RT ~ Age.Category*CR.composite.before + zSoP.comp + zWM.Score + zStroop.SRC + (1|ID)  + (1|Trial.Number), data=PNactions_RT, REML=FALSE)
# summary(Mfull_PNact_RT)

##Create a tidy output table for the fixed effects
(tidyMfull_PNactRT <- broom.mixed::tidy(Mfull_PNact_RT, effect = "fixed",conf.int=T, conf.level=0.95))
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["effect"],"name":[1],"type":["chr"],"align":["left"]},{"label":["term"],"name":[2],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["df"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["conf.low"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["conf.high"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"fixed","2":"(Intercept)","3":"1106.960993","4":"29.31150","5":"37.7654153","6":"137.82159","7":"1.428769e-74","8":"1049.002595","9":"1164.919391"},{"1":"fixed","2":"Age.Category2","3":"-89.101362","4":"32.46760","5":"-2.7443162","6":"89.09409","7":"7.332555e-03","8":"-153.612855","9":"-24.589869"},{"1":"fixed","2":"Age.Category3","3":"-61.155276","4":"41.07698","5":"-1.4887969","6":"89.22786","7":"1.400669e-01","8":"-142.771483","9":"20.460931"},{"1":"fixed","2":"CR.composite.before","3":"25.235092","4":"21.45189","5":"1.1763573","6":"89.24751","7":"2.425803e-01","8":"-17.387736","9":"67.857920"},{"1":"fixed","2":"zSoP.comp","3":"62.697742","4":"22.93882","5":"2.7332588","6":"89.69105","7":"7.554253e-03","8":"17.123626","9":"108.271857"},{"1":"fixed","2":"zWM.Score","3":"28.521179","4":"15.59787","5":"1.8285299","6":"86.87628","7":"7.090178e-02","8":"-2.481906","9":"59.524265"},{"1":"fixed","2":"zStroop.SRC","3":"-4.778792","4":"14.29728","5":"-0.3342448","6":"89.57035","7":"7.389762e-01","8":"-33.184692","9":"23.627107"},{"1":"fixed","2":"Age.Category2:CR.composite.before","3":"-31.673283","4":"30.32729","5":"-1.0443822","6":"89.10781","7":"2.991336e-01","8":"-91.931957","9":"28.585392"},{"1":"fixed","2":"Age.Category3:CR.composite.before","3":"-65.253042","4":"30.67124","5":"-2.1274995","6":"89.39233","7":"3.613197e-02","8":"-126.192451","9":"-4.313633"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
##Write tidy output data to .csv file
#write.csv(tidyMfull_PNactRT, file = "./Figures and Tables/PNact_lmerFull.csv", row.names=F)
```

#### Checking Assumptions LMER Actions

##### Assumption 1 - Linearity

``` r
plot(resid(Mfull_PNact_RT), actRTna.rm$RT)
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
We can assume linearity.

##### Assumption 2 - Homogeneity of Variance / Homoscedasticity

``` r
#Extracts the residuals and places them in a new column in RTna.rm
actRTna.rm$Mfull.Res <- residuals(Mfull_PNact_RT) 
#Takes the absolute values of the residuals
actRTna.rm$Abs.Mfull.Res <- abs(actRTna.rm$Mfull.Res) 
#Squares the absolute values to provide the more robust estimate
actRTna.rm$Mfull.Res2 <- actRTna.rm$Abs.Mfull.Res^2 

#ANOVA of the squared residuals
Levene.Mfull <- lm(Mfull.Res2 ~ ID, data=actRTna.rm) 
anova(Levene.Mfull) #Displays the results
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Df"],"name":[1],"type":["int"],"align":["right"]},{"label":["Sum Sq"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Mean Sq"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["F value"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>F)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"3.280479e+10","3":"32804789913","4":"1.463961","5":"0.2263451","_rn_":"ID"},{"1":"6475","2":"1.450934e+14","3":"22408244101","4":"NA","5":"NA","_rn_":"Residuals"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

p&gt;0.05,so we can assume homogeneity of variance/homoscedasticity

``` r
#Create plot for homogeneity of variance
plot(fitted(Mfull_PNact_RT), residuals(Mfull_PNact_RT),
     xlab = "Fitted Values", ylab="Residuals")
abline(h=0, lty=2, lwd=2, col="purple")
lines(smooth.spline(fitted(Mfull_PNact_RT), residuals(Mfull_PNact_RT)), col="red") 
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
Purple and red line roughly overlap, so we can assume homoscedasticity

##### Assumption 3 - Residuals are normally distributed

``` r
#Create qq plot of the full model
qqmath(Mfull_PNact_RT)
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

Where does the bulk on the right and the outliers on the left come from?

After checking outliers: outliers on the left are now gone and were
driven by a typo in the RT transcription of one participant

``` r
#Check whether the deviations from the QQplot are due to age category
dda <- cbind(augment(Mfull_PNact_RT), group=actRTna.rm$Age.Category)
sample_var <- "RT"
group_var  <- "Age.Category"

# code to compute the slope and the intercept of the qq-line per group

qqlines <- function(vec, group) {
    x <- qnorm(c(0.25, 0.75))    
    y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1] - slope * x[1]
    data.frame(slope, int, group)
}

slopedf <- do.call(rbind,lapply(unique(dda$group), function(grp) qqlines(dda[dda$group == grp,sample_var], grp)))

#Create ggplot of the qq-line per age group to check for differences between age groups
p <- ggplot(dda)+stat_qq(aes_string(sample=sample_var, colour=group_var)) + 
    geom_abline(data = slopedf, aes(slope = slope, intercept = int, colour = group)) +
    scale_colour_discrete(guide=guide_legend(title = "Age Group"), labels=c("Younger","Middle-Aged", "Older"))
p
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

The younger age category seems to have a bigger bulk at the right but
all age groups follow a similar pattern.

Try log transformation.

``` r
#Full model with log-transformed Reaction Times as outcome variable
logMfull_PNact_RT <- lmer(log(RT) ~ Age.Category*CR.composite.before + zSoP.comp + zWM.Score + zStroop.SRC + (1|ID)  + (1|Trial.Number), data=PNactions_RT, REML=FALSE)
```

*Checking the dimensionality of the variance-covariance matrices of
random effects assumed in a maximal LMM*

The number of principal components that cumulatively account for 100% of
the variance is a reasonably stringent criterion for settling on the
reduced dimensionality (Bates et al., 2015)

``` r
summary(rePCA(logMfull_PNact_RT))
```

    ## $ID
    ## Importance of components:
    ##                          [,1]
    ## Standard deviation     0.4471
    ## Proportion of Variance 1.0000
    ## Cumulative Proportion  1.0000
    ## 
    ## $Trial.Number
    ## Importance of components:
    ##                         [,1]
    ## Standard deviation     0.497
    ## Proportion of Variance 1.000
    ## Cumulative Proportion  1.000

To assess overfitting of the model, we conducted a principal components
analysis (PCA) of the random effects variance-covariance structure. The
PCA did not indicate overspecification of the random effects for ID or
Trial. Hence, we report the analysis using the full model with both
random effects.

*Checking variance explained by random factors:*

``` r
0.01186/(0.01186+0.01466+0.05933) #~13.8% of variance that's left over after the variance explained by our predictor variables is explained by ID (i.e. subject)
```

    ## [1] 0.1381479

``` r
0.01466/(0.01186+0.01466+0.05933) #~17.1% of variance that's left over after the variance explained by our predictor variables is explained by trial (i.e. stimuli)
```

    ## [1] 0.170763

*Recheck model fit after log transformation*

``` r
## Normality of residuals
qqmath(logMfull_PNact_RT)
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
Looks much better now. Except the bulk on the left and right. But we
will assume the assumption of normality of residuals is met.

*Data per participant:*

``` r
actdataPP <- PNactions_RT %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(meanRT = mean(RT, na.rm=T),
            sdRT = sd(RT, na.rm=T))

#To look more into depth for any outliers and deviations.
car::Boxplot(log(PNactions_RT$RT) ~ PNactions_RT$Age.Category, id.method="identify")
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

    ##  [1] "579"  "1195" "1264" "2181" "2433" "3129" "3968" "4571" "5819" "5903"
    ## [11] "2065" "4649"

Check each row in the table that was detected as outlier.

–&gt; these outliers don’t seem to strange seen the frequencies of the
words/pictures. A possibility would be the effect of online study.
Distractions from home (longer RTs), differences in microphone or
distance from microphone (causing either smaller or longer RTs than
average), maybe they were positively/negatively primed somehow.

*Histogram of transformed RTs for action naming*

``` r
hist(log(PNactions_RT$RT)) 
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

*Homoscedasticity with log-transformed reaction time*

``` r
plot(fitted(logMfull_PNact_RT), residuals(logMfull_PNact_RT),
     xlab = "Fitted Values", ylab="Residuals")
abline(h=0, lty=2, lwd=2, col="purple")
lines(smooth.spline(fitted(logMfull_PNact_RT), residuals(logMfull_PNact_RT)), col="red") 
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
Purple and red line roughly overlap, so we can assume homoscedasticity

### Results log RT action naming

``` r
#Quick overview full model outcome
glance(logMfull_PNact_RT)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["sigma"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["logLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["AIC"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["deviance"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["df.residual"],"name":[6],"type":["int"],"align":["right"]}],"data":[{"1":"0.2435731","2":"-283.8691","3":"591.7382","4":"673.0503","5":"567.7382","6":"6465"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
#Tidy model summary
(log_tidyMfull_PNactRT <- broom.mixed::tidy(logMfull_PNact_RT, effects = "fixed", conf.int=T, conf.level=0.95))
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["effect"],"name":[1],"type":["chr"],"align":["left"]},{"label":["term"],"name":[2],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["df"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["conf.low"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["conf.high"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"fixed","2":"(Intercept)","3":"6.966581805","4":"0.02841266","5":"245.1928361","6":"134.90237","7":"1.238867e-180","8":"6.9103899301","9":"7.022773680"},{"1":"fixed","2":"Age.Category2","3":"-0.085455746","4":"0.03184854","5":"-2.6831921","6":"89.26525","7":"8.689831e-03","8":"-0.1487355233","9":"-0.022175970"},{"1":"fixed","2":"Age.Category3","3":"-0.060084924","4":"0.04029051","5":"-1.4912921","6":"89.37204","7":"1.394061e-01","8":"-0.1401367233","9":"0.019966875"},{"1":"fixed","2":"CR.composite.before","3":"0.023107140","4":"0.02104090","5":"1.0982011","6":"89.39036","7":"2.750659e-01","8":"-0.0186981644","9":"0.064912445"},{"1":"fixed","2":"zSoP.comp","3":"0.063265749","4":"0.02249346","5":"2.8126280","6":"89.73178","7":"6.035878e-03","8":"0.0185767354","9":"0.107954762"},{"1":"fixed","2":"zWM.Score","3":"0.031039948","4":"0.01532112","5":"2.0259587","6":"87.53058","7":"4.581399e-02","8":"0.0005901741","9":"0.061489722"},{"1":"fixed","2":"zStroop.SRC","3":"-0.003444121","4":"0.01402071","5":"-0.2456453","6":"89.63816","7":"8.065184e-01","8":"-0.0313002391","9":"0.024411996"},{"1":"fixed","2":"Age.Category2:CR.composite.before","3":"-0.025942701","4":"0.02974878","5":"-0.8720593","6":"89.27729","7":"3.855153e-01","8":"-0.0850503633","9":"0.033164961"},{"1":"fixed","2":"Age.Category3:CR.composite.before","3":"-0.062334785","4":"0.03008104","5":"-2.0722284","6":"89.50364","7":"4.112085e-02","8":"-0.1221005366","9":"-0.002569034"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
## Write tidy table to .csv file
# write.csv(log_tidyMfull_PNactRT, file = "./Figures and Tables/logPNact_lmerFull.csv", row.names=F)
```

*Effect Size - R squared*

``` r
r2_nakagawa(logMfull_PNact_RT)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.338
    ##      Marginal R2: 0.042

Marginal R2 is the variance explained by fixed effects. Theoretical is
for binomial distributions. Conditional R2 is the variance explained by
the whole model.

4.3% of the variance in the data is explained by the fixed effects only.
33.7% of the variance is explained by the whole model.

## Picture Naming Objects

``` r
PNobjects_RT <- PN_RT %>%
  # Only include trials for object naming (i.e., type==2)
  dplyr::filter(type==2) %>%
  group_by(ID, Name) %>%
  slice(1) %>%
  ungroup()
  # Check
    # unique(PNobjects_RT$Task.Name) #objects

## Create histogram for distribution
hist(PNobjects_RT$RT, breaks = 100)
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
skewness(log(PNobjects_RT$RT))
```

    ## [1] 0.4341801

0.44 -&gt; positively skewed but within limits (Bryne, 2010; George &
Mallery, 2010)

### LMER for Objects

*Should we include Trial and ID as random effects?*

``` r
#Base model with log-transformed reaction time
Mbase_obj <- lm(log(RT) ~ 1, data=PNobjects_RT)
#Base model with only ID/individual variability as random effect
Mrandom.ID_obj <- lmer(log(RT) ~ 1 +(1|ID), data=PNobjects_RT)
#Base model with only Trial.Number/trial variability as random effect
Mrandom.Trial_obj <- lmer(log(RT) ~ 1 +(1|Trial.Number), data=PNobjects_RT)
#Base model with both random effects
Mrandom.All_obj <- lmer(log(RT) ~ 1 +(1|ID) + (1|Trial.Number), data=PNobjects_RT)
```

AIC base model

``` r
#Obtain AIC values for each model
(AIC.base_obj <- AIC(logLik(Mbase_obj)))
```

    ## [1] 683.5062

AIC - only ID as random effect

``` r
(AIC.reID_obj <- AIC(logLik(Mrandom.ID_obj)))
```

    ## [1] -1156.379

AIC - only trial as random effect

``` r
(AIC.reTrial_obj <- AIC(logLik(Mrandom.Trial_obj)))
```

    ## [1] 384.5348

AIC - both ID and trial as random effect

``` r
(AIC.reBoth_obj <- AIC(logLik(Mrandom.All_obj)))
```

    ## [1] -1705.234

The AIC for the model including both random effects is lowest –&gt; we
justified inclusion of both Trial and Subject as random effects.

*Null model of reaction times for object naming with random effects
included*

``` r
M0_PNobjRT <- lmer(RT ~ 1 +(1|ID) + (1|Trial.Number), data=PNobjects_RT, REML=FALSE)
summary(M0_PNobjRT)
```

    ## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
    ##   method [lmerModLmerTest]
    ## Formula: RT ~ 1 + (1 | ID) + (1 | Trial.Number)
    ##    Data: PNobjects_RT
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  77921.5  77948.2 -38956.7  77913.5     5852 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2846 -0.6189 -0.1896  0.3709  5.3786 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev.
    ##  ID           (Intercept) 13219    114.98  
    ##  Trial.Number (Intercept)  4125     64.22  
    ##  Residual                 32443    180.12  
    ## Number of obs: 5856, groups:  ID, 90; Trial.Number, 70
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)    
    ## (Intercept)   812.40      14.54 139.65   55.86   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

*Unconditional model, i.e., the model without covariates/control
measures*

``` r
Muncond_PNobjRT <- lmer(RT ~ Age.Category*CR.composite.before + (1|ID)  + (1|Trial.Number), data=PNobjects_RT, REML=FALSE)
```

*Full model, i.e., model with covariates/control measures. RT not
log-transformed*

``` r
Mfull_PNobj_RT <- lmer(RT ~ Age.Category*CR.composite.before + zSoP.comp + zWM.Score + zStroop.SRC + (1|ID)  + (1|Trial.Number), data=PNobjects_RT, REML=FALSE)
# summary(Mfull_PNobj_RT)

##Create a tidy output table for the fixed effects
(tidyMfull_PNobjRT <- broom.mixed::tidy(Mfull_PNobj_RT, effect = "fixed",conf.int=T, conf.level=0.95))
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["effect"],"name":[1],"type":["chr"],"align":["left"]},{"label":["term"],"name":[2],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["df"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["conf.low"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["conf.high"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"fixed","2":"(Intercept)","3":"787.681312","4":"22.69670","5":"34.7046599","6":"111.53600","7":"1.326328e-61","8":"742.708664","9":"832.6539598"},{"1":"fixed","2":"Age.Category2","3":"3.969733","4":"27.26954","5":"0.1455739","6":"89.74346","7":"8.845842e-01","8":"-50.208069","9":"58.1475343"},{"1":"fixed","2":"Age.Category3","3":"78.475206","4":"34.49993","5":"2.2746485","6":"89.87552","7":"2.530639e-02","8":"9.933787","9":"147.0166246"},{"1":"fixed","2":"CR.composite.before","3":"22.009806","4":"18.02272","5":"1.2212252","6":"90.01424","7":"2.251898e-01","8":"-13.795405","9":"57.8150180"},{"1":"fixed","2":"zSoP.comp","3":"43.191869","4":"19.23972","5":"2.2449326","6":"89.83591","7":"2.722724e-02","8":"4.967861","9":"81.4158765"},{"1":"fixed","2":"zWM.Score","3":"25.033731","4":"13.18853","5":"1.8981444","6":"89.92509","7":"6.088459e-02","8":"-1.167879","9":"51.2353414"},{"1":"fixed","2":"zStroop.SRC","3":"-16.938647","4":"11.99407","5":"-1.4122514","6":"89.78948","7":"1.613325e-01","8":"-40.767729","9":"6.8904344"},{"1":"fixed","2":"Age.Category2:CR.composite.before","3":"-39.233271","4":"25.47943","5":"-1.5398018","6":"89.86620","7":"1.271226e-01","8":"-89.853631","9":"11.3870894"},{"1":"fixed","2":"Age.Category3:CR.composite.before","3":"-52.147385","4":"25.75696","5":"-2.0245941","6":"89.99914","7":"4.587293e-02","8":"-103.318083","9":"-0.9766856"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

#### Checking Assumptions LMER Objects

##### Assumption 1 - Linearity

``` r
plot(resid(Mfull_PNobj_RT), objRTna.rm$RT)
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

We can assume linearity.

##### Assumption 2 - Homogeneity of Variance / Homoscedasticity

``` r
#Extracts the residuals and places them in a new column in RTna.rm
objRTna.rm$Mfull.Res <- residuals(Mfull_PNobj_RT) 
#Takes the absolute values of the residuals
objRTna.rm$Abs.Mfull.Res <- abs(objRTna.rm$Mfull.Res)
#Squares the absolute values to provide the more robust estimate
objRTna.rm$Mfull.Res2 <- objRTna.rm$Abs.Mfull.Res^2

#ANOVA of the squared residuals
Levene.Mfull <- lm(Mfull.Res2 ~ ID, data=objRTna.rm) 
anova(Levene.Mfull) #Displays the results 
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Df"],"name":[1],"type":["int"],"align":["right"]},{"label":["Sum Sq"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Mean Sq"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["F value"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Pr(>F)"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1.367399e+09","3":"1367398697","4":"0.2760868","5":"0.5992975","_rn_":"ID"},{"1":"5854","2":"2.899361e+13","3":"4952785342","4":"NA","5":"NA","_rn_":"Residuals"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

p&gt;0.05,so we can assume homogeneity of variance/homoscedasticity

``` r
#Create plot for homogeneity of variance
plot(fitted(Mfull_PNobj_RT), residuals(Mfull_PNobj_RT),
     xlab = "Fitted Values", ylab="Residuals")
abline(h=0, lty=2, lwd=2, col="purple")
lines(smooth.spline(fitted(Mfull_PNobj_RT), residuals(Mfull_PNobj_RT)), col="red") 
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->
Purple and red line roughly overlap, so we can assume homoscedasticity

##### Assumption 3 - Residuals are normally distributed

``` r
#QQ plot to check normality of residuals
qqmath(Mfull_PNobj_RT)
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

Where does the bulk on the right come from? (CHECK OUTLIERS)

``` r
#Check whether the deviations from the QQplot are due to age category
dda <- cbind(augment(Mfull_PNobj_RT), group=objRTna.rm$Age.Category)
sample_var <- "RT"
group_var  <- "Age.Category"

# code to compute the slope and the intercept of the qq-line 

qqlines <- function(vec, group) {
    x <- qnorm(c(0.25, 0.75))    
    y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1] - slope * x[1]
    data.frame(slope, int, group)
}

slopedf <- do.call(rbind,lapply(unique(dda$group), function(grp) qqlines(dda[dda$group == grp,sample_var], grp)))

#Create ggplot of the qq-line per age group to check for differences between age groups
p <- ggplot(dda)+stat_qq(aes_string(sample=sample_var, colour=group_var)) + 
    geom_abline(data = slopedf, aes(slope = slope, intercept = int, colour = group))+
      scale_colour_discrete(guide=guide_legend(title = "Age Group"), labels=c("Younger","Middle-Aged", "Older"))

p
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

The Older age group seems to have a bigger bulk at the right but all age
groups follow a similar pattern.

Try log transformation.

``` r
#Full model with log-transformed Reaction Times as outcome variable
logMfull_PNobj_RT <- lmer(log(RT) ~ Age.Category*CR.composite.before + zSoP.comp + zWM.Score + zStroop.SRC + (1|ID)  + (1|Trial.Number), data=PNobjects_RT, REML=FALSE)
```

*Checking the dimensionality of the variance-covariance matrices of
random effects assumed in a maximal LMM*

The number of principal components that cumulatively account for 100% of
the variance is a reasonably stringent criterion for settling on the
reduced dimensionality (Bates et al., 2015)

``` r
summary(rePCA(logMfull_PNobj_RT))
```

    ## $ID
    ## Importance of components:
    ##                          [,1]
    ## Standard deviation     0.5959
    ## Proportion of Variance 1.0000
    ## Cumulative Proportion  1.0000
    ## 
    ## $Trial.Number
    ## Importance of components:
    ##                          [,1]
    ## Standard deviation     0.3725
    ## Proportion of Variance 1.0000
    ## Cumulative Proportion  1.0000

To assess overfitting of the model, we conducted a principal components
analysis (PCA) of the random effects variance-covariance structure. The
PCA did not indicate overspecification of the random effects for ID or
Trial. Hence, we report the analysis using the full model with both
random effects.

*Checking variance explained by random factors:*

``` r
0.014247 /(0.014247 + 0.005567  +0.040128  ) #~23.8% of variance that's left over after the variance explained by our predictor variables is explained by ID (i.e. subject)
```

    ## [1] 0.2376798

``` r
 0.005567  /(0.014247 + 0.005567  +0.040128  ) #~9.3% of variance that's left over after the variance explained by our predictor variables is explained by trial (i.e. stimuli)
```

    ## [1] 0.09287311

*Recheck model fit after log transformation*

``` r
## Normality of residuals
qqmath(logMfull_PNobj_RT)
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->
Looks much better now. Except the outliers on the right. We will assume
normality of residuals.

*Data per participant:*

``` r
objdataPP <- PNobjects_RT %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(meanRT = mean(RT, na.rm=T),
            sdRT = sd(RT, na.rm=T))

#To look more into depth for any outliers and deviations.
car::Boxplot(log(PNobjects_RT$RT) ~ PNobjects_RT$Age.Category, id.method="identify")
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

    ##  [1] "1597" "5762" "5719" "3356" "2105" "5437" "4334" "4700" "4362" "3311"
    ## [11] "2987" "3070" "3811" "5187" "1672" "3640" "5385" "3530" "1097" "3619"
    ## [21] "1380" "941"  "3639" "3168" "1750" "4288" "4293" "5065"

Check each row in the table that was detected as outlier.

*Histogram of transformed RTs for object naming*

``` r
hist(log(PNobjects$RT),breaks=50) 
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

*Homoscedasticity with log-transformed reaction time*

``` r
plot(fitted(logMfull_PNobj_RT), residuals(logMfull_PNobj_RT),
     xlab = "Fitted Values", ylab="Residuals")
abline(h=0, lty=2, lwd=2, col="purple")
lines(smooth.spline(fitted(logMfull_PNobj_RT), residuals(logMfull_PNobj_RT)), col="red") 
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->
Purple and red line roughly overlap, so we can assume homoscedasticity

### Results RT object naming

``` r
#Quick overview full model outcome
glance(logMfull_PNobj_RT)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["sigma"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["logLik"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["AIC"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["deviance"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["df.residual"],"name":[6],"type":["int"],"align":["right"]}],"data":[{"1":"0.2003207","2":"875.7637","3":"-1727.527","4":"-1647.425","5":"-1751.527","6":"5844"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
#Tidy model summary
(log_tidyMfull_PNobjRT <- broom.mixed::tidy(logMfull_PNobj_RT, effects = "fixed", conf.int=T, conf.level=0.95))
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["effect"],"name":[1],"type":["chr"],"align":["left"]},{"label":["term"],"name":[2],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["df"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["conf.low"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["conf.high"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"fixed","2":"(Intercept)","3":"6.645487993","4":"0.02842932","5":"233.7547610","6":"108.53501","7":"1.616834e-148","8":"6.5891393032","9":"6.701836683"},{"1":"fixed","2":"Age.Category2","3":"-0.005120024","4":"0.03446558","5":"-0.1485547","6":"89.81809","7":"8.822380e-01","8":"-0.0735938033","9":"0.063353755"},{"1":"fixed","2":"Age.Category3","3":"0.076478713","4":"0.04360035","5":"1.7540848","6":"89.92050","7":"8.282296e-02","8":"-0.0101420385","9":"0.163099466"},{"1":"fixed","2":"CR.composite.before","3":"0.025294329","4":"0.02277479","5":"1.1106284","6":"90.02860","7":"2.696858e-01","8":"-0.0199515729","9":"0.070540231"},{"1":"fixed","2":"zSoP.comp","3":"0.059619330","4":"0.02431539","5":"2.4519173","6":"89.88986","7":"1.614245e-02","8":"0.0113117540","9":"0.107926907"},{"1":"fixed","2":"zWM.Score","3":"0.033778657","4":"0.01666689","5":"2.0266920","6":"89.95895","7":"4.565470e-02","8":"0.0006667607","9":"0.066890553"},{"1":"fixed","2":"zStroop.SRC","3":"-0.020497550","4":"0.01515870","5":"-1.3521971","6":"89.85375","7":"1.797059e-01","8":"-0.0506136215","9":"0.009618521"},{"1":"fixed","2":"Age.Category2:CR.composite.before","3":"-0.037428680","4":"0.03220060","5":"-1.1623597","6":"89.91359","7":"2.481653e-01","8":"-0.1014016307","9":"0.026544270"},{"1":"fixed","2":"Age.Category3:CR.composite.before","3":"-0.063649194","4":"0.03254863","5":"-1.9555109","6":"90.01657","7":"5.362431e-02","8":"-0.1283125614","9":"0.001014173"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
## Write tidy table to .csv file
# write.csv(log_tidyMfull_PNobjRT, file = "./Figures and Tables/logPNobj_lmerFull.csv", row.names=F)
```

*Effect Size - R squared*

``` r
r2_nakagawa(logMfull_PNobj_RT)
```

    ## # R2 for Mixed Models
    ## 
    ##   Conditional R2: 0.395
    ##      Marginal R2: 0.096

Marginal R2 is the variance explained by fixed effects. Theoretical is
for binomial distributions. Conditional R2 is the variance explained by
the whole model.

10.5% of the variance in the data is explained by the fixed effects
only. 38.7% of the variance is explained by the whole model.

## Visualising significant predictors

``` r
#Create labels for legend per age group
Ages = as_labeller(c(`1`="Younger", `2`="Middle-Aged", `3`="Older"))

#Colour Palette - colourblind friendly
cbbPalette <- c("#999999", "#E69F00", "#56B4E9")
```

*Relationship RT and Cognitive Processing speed for reaction time
(outcome) of picture naming Actions*

``` r
(plot.PNact_zSOP <- ggplot(PNactions_RT, aes(x=zSoP.comp, y=RT, colour=as.factor(Age.Category))) +
   geom_jitter(width = 0.25, size=0.8, show.legend =F) +
   geom_smooth(method = "lm", formula = y~x, fill="white", colour="black", show.legend=F) +
   labs(x = "Cognitive Processing Speed (z-distribution)",
        y= "Reaction time (in ms)",
        title = "Relationship between Cognitive Processing Speed 
        and Picture Naming Reaction Time Actions") +
   facet_grid(~Age.Category, labeller=labeller(Age.Category=Ages)) +
    theme(text = element_text(size = 14),
          panel.background  = element_rect(fill="white"),
          plot.background = element_rect(fill = "white"),
          strip.background = element_rect(fill="white"),
          axis.line.x = element_line(color="black"),
         axis.line.y = element_line(color="black")) +
         scale_colour_manual(values = cbbPalette))
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->
*Relationship RT and Cognitive Processing Speed for reaction time
(outcome) of picture naming Objects*

``` r
(plot.PNobj_zSOP <- ggplot(PNobjects_RT, aes(x=zSoP.comp, y=RT, colour=as.factor(Age.Category))) +
   geom_jitter(size=0.8, width = 0.25, show.legend=F) +
   geom_smooth(method = "lm", formula = y~x, fill="white", size=1, colour="black") +
   labs(x = "Cognitive Processing Speed (z-distribution)",
        y= "Reaction time (in ms)",
        title = "The relationship between Cognitive Processing Speed
        and Picture Naming Reaction Time Objects")+
   facet_grid(~Age.Category, labeller=labeller(Age.Category=Ages)) +
   theme(text = element_text(size = 14),
          panel.background  = element_rect(fill="white"),
          plot.background = element_rect(fill = "white"),
          strip.background = element_rect(fill="white"),
          axis.line.x = element_line(color="black"),
         axis.line.y = element_line(color="black")) +
         scale_colour_manual(values = cbbPalette))
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

*Relationship Reaction Times and Cognitive Reserve (CR) for the period
preceding the COVID-19 pandemic for reaction time (outcome) of picture
naming Actions*

``` r
#Save figure as tiff file
# tiff(file="./Figures and Tables/Figure_1.tiff",
# res = 300, family = "sans", width = 7, height=4.5, units="in")

(plot.PNact_CR <- ggplot(PNactions_RT, aes(x=CR.composite.before, y=RT, shape=as.factor(Age.Category),colour = as.factor(Age.Category))) +
   geom_jitter(width = 0.5, height=0.25, size=1, show.legend = F, alpha=.8) +
   geom_smooth(method = "lm", formula = y~x, fill="white", colour="black", show.legend = F, size=.8) +
   labs(x = "\n CR Score Before COVID-19 (Z-Scores)",
        y= "Reaction Time (in ms) \n",
        title = "Relationship between CR pre-Covid-19 and Action Naming RT") +
      facet_grid(.~Age.Category, labeller=labeller(Age.Category=Ages)) +
    theme(text = element_text(size = 14),
          panel.background  = element_rect(fill="white"),
          plot.background = element_rect(fill = "white"),
          strip.background = element_rect(fill="white"),
          axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black")) +
  scale_colour_manual(values = cbbPalette))
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

``` r
# dev.off()
```

*Relationship Reaction Times and Cognitive reserve for the period
preceding the COVID-19 pandemic for reaction time (outcome) of picture
naming Objects*

``` r
# png(file="./Figures and Tables/RelationCR-PNobjRT.png",
# width=600, height=350)

(plot.PNobj_CR <- ggplot(PNobjects_RT, aes(x=CR.composite.before, y=RT, colour=as.factor(Age.Category))) +
   geom_jitter(width = 0.25, size=0.8, show.legend = F) +
   geom_smooth(method = "lm", formula = y~x, fill="white", colour="black", show.legend = F) +
   labs(x = "CR score before Covid-19 (z-distribution)",
        y= "Reaction time (in ms)",
        title = "Relationship between CR pre-Covid-19 and Object Naming RT") +
   facet_grid(~Age.Category, labeller=labeller(Age.Category=Ages))+
       theme(text = element_text(size = 14),
          panel.background  = element_rect(fill="white"),
          plot.background = element_rect(fill = "white"),
          strip.background = element_rect(fill="white"),
          axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black")) +
  scale_colour_manual(values = cbbPalette))
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

``` r
# dev.off()   
```

### Model comparisons for the CR measure preciding and coinciding with the COVID-19 pandemic

The effect of the COVID-19 pandemic on CR and subsequent behavioural
performance

*Action Naming*

``` r
#Full Action Naming log model with CR coinciding the COVID-19 pandemic
logMfull_PNact_RT.during <- lmer(log(RT) ~ Age.Category*CR.composite.during + zSoP.comp + zWM.Score + zStroop.SRC + (1|ID)  + (1|Trial.Number), data=PNactions_RT, REML=FALSE)
# summary(logMfull_PNact_RT.during)
```

*Comparing the Action Naming models: preceding vs. coinciding the
COVID-19 pandemic*

``` r
anova(logMfull_PNact_RT, logMfull_PNact_RT.during)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["npar"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["AIC"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["logLik"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["deviance"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[8],"type":["lgl"],"align":["right"]}],"data":[{"1":"12","2":"591.7382","3":"673.0503","4":"-283.8691","5":"567.7382","6":"NA","7":"NA","8":"NA","_rn_":"logMfull_PNact_RT"},{"1":"12","2":"592.2969","3":"673.6090","4":"-284.1484","5":"568.2969","6":"0","7":"0","8":"NA","_rn_":"logMfull_PNact_RT.during"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

There are barely any differences between the two models. The AIC is a
tiny bit lower for the CR composite score before the pandemic, meaning
that that model fits the data slightly better.

*Object Naming*

``` r
#Full Object Naming log model with CR coinciding the COVID-19 pandemic
logMfull_PNobj_RT.during <- lmer(log(RT) ~ Age.Category*CR.composite.during + zSoP.comp + zWM.Score + zStroop.SRC + (1|ID)  + (1|Trial.Number), data=PNobjects_RT, REML=FALSE)
# summary(logMfull_PNobj_RT.during)
```

*Comparing the Object Naming models: preceding vs. coinciding the
COVID-19 pandemic*

``` r
anova(logMfull_PNobj_RT, logMfull_PNobj_RT.during)
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["npar"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["AIC"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["logLik"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["deviance"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Chisq"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["Df"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Pr(>Chisq)"],"name":[8],"type":["lgl"],"align":["right"]}],"data":[{"1":"12","2":"-1727.527","3":"-1647.425","4":"875.7637","5":"-1751.527","6":"NA","7":"NA","8":"NA","_rn_":"logMfull_PNobj_RT"},{"1":"12","2":"-1728.639","3":"-1648.537","4":"876.3196","5":"-1752.639","6":"1.111837","7":"0","8":"NA","_rn_":"logMfull_PNobj_RT.during"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

There are barely any differences between the two models. The AIC is a
tiny bit lower for the CR composite score during the pandemic, meaning
that that model fits the data slightly better. In the model with CR
during, the interaction term between CR score and being an older adult
is not trending anymore.

*Descriptives Average CR before and during Covid-19*

``` r
CompareCR <- PN_all %>% #dataset with both object and action naming data
  group_by(Age.Category) %>%
  #Create means for CR scores preceding and coinciding with the COVID-19 pandemic
  summarise(CR.before = mean(CR.composite.before),
            CR.during = mean(CR.composite.during)) %>%
  #Convert from wide to long format
  pivot_longer(cols = CR.before:CR.during, names_to = "Period",values_to = "CRscore") %>%
  #Recode age groups as categorical values
    dplyr::mutate(across(Age.Category, ~case_when(
                                    . == 1 ~"Younger", 
                                    . == 2 ~"Middle-Aged",
                                    . == 3 ~"Older")))

#Summary table with mean CR scores
CompareCR
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["Age.Category"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Period"],"name":[2],"type":["chr"],"align":["left"]},{"label":["CRscore"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"Younger","2":"CR.before","3":"0.043013671"},{"1":"Younger","2":"CR.during","3":"-0.017388026"},{"1":"Middle-Aged","2":"CR.before","3":"-0.054959810"},{"1":"Middle-Aged","2":"CR.during","3":"-0.056598316"},{"1":"Older","2":"CR.before","3":"0.034564152"},{"1":"Older","2":"CR.during","3":"-0.006141751"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

*Visualisation Average CR before and during Covid-19*

``` r
#Save as png file
# png(file="./Figures and Tables/CR_Before-vs-During.png",
# width=350, height=200)

(Barplot_CompareCR <-ggplot(CompareCR, aes(x=as.factor(Period), y=CRscore, fill=as.factor(Period))) +
  stat_summary(geom="bar", fun=mean, position="dodge", show.legend = F) +
  scale_fill_manual(values = c("midnightblue", "magenta"), guide=guide_legend(title = "Period"), labels=c("Before Covid-19","During Covid-19"))+
  coord_cartesian(ylim = c(-.06,.06)) +
  scale_y_continuous(breaks = seq(-.05,.05, 0.025)) +
  labs(title = "CR scores before and during Covid-19",
       x="Before and during Covid-19",
       y="Average CR score (z-distribution)") +
  theme_grey() +
    facet_grid(~Age.Category)+
           theme(text = element_text(size = 14),
          panel.background  = element_rect(fill="white"),
          plot.background = element_rect(fill = "white"),
          strip.background = element_rect(fill="white"),
          axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))+
   geom_hline(yintercept=0))
```

![](R-Code-Full-Analysis-Picture-Naming-Reaction-Times_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
# dev.off()
```

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-R-Matrix" class="csl-entry">

Bates, Douglas, and Martin Maechler. 2019. *Matrix: Sparse and Dense
Matrix Classes and Methods*. <http://Matrix.R-forge.R-project.org/>.

</div>

<div id="ref-R-lme4" class="csl-entry">

Bates, Douglas, Martin Maechler, Ben Bolker, and Steven Walker. 2020.
*Lme4: Linear Mixed-Effects Models Using Eigen and S4*.
<https://github.com/lme4/lme4/>.

</div>

<div id="ref-lme42015" class="csl-entry">

Bates, Douglas, Martin Mächler, Ben Bolker, and Steve Walker. 2015.
“Fitting Linear Mixed-Effects Models Using <span
class="nocase">lme4</span>.” *Journal of Statistical Software* 67 (1):
1–48. <https://doi.org/10.18637/jss.v067.i01>.

</div>

<div id="ref-BarbaraM.Byrne2001SEMW" class="csl-entry">

Byrne, Barbara M. 2001. *Structural Equation Modeling with AMOS: Basic
Concepts, Applications, and Programming*. Multivariate Applications
Series. Taylor; Francis.

</div>

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

<div id="ref-10.5555/1942062" class="csl-entry">

George, Darren, and Paul Mallery. 2010. *SPSS for Windows Step by Step:
A Simple Guide and Reference 18.0 Update*. 11th ed. USA: Prentice Hall
Press.

</div>

<div id="ref-R-purrr" class="csl-entry">

Henry, Lionel, and Hadley Wickham. 2020. *Purrr: Functional Programming
Tools*. <https://CRAN.R-project.org/package=purrr>.

</div>

<div id="ref-R-fs" class="csl-entry">

Hester, Jim, and Hadley Wickham. 2020. *Fs: Cross-Platform File System
Operations Based on Libuv*. <https://CRAN.R-project.org/package=fs>.

</div>

<div id="ref-lmerTest2017" class="csl-entry">

Kuznetsova, Alexandra, Per B. Brockhoff, and Rune H. B. Christensen.
2017. “<span class="nocase">lmerTest</span> Package: Tests in Linear
Mixed Effects Models.” *Journal of Statistical Software* 82 (13): 1–26.
<https://doi.org/10.18637/jss.v082.i13>.

</div>

<div id="ref-R-lmerTest" class="csl-entry">

Kuznetsova, Alexandra, Per Bruun Brockhoff, and Rune Haubo Bojesen
Christensen. 2020. *lmerTest: Tests in Linear Mixed Effects Models*.
<https://github.com/runehaubo/lmerTestR>.

</div>

<div id="ref-R-sjPlot" class="csl-entry">

Lüdecke, Daniel. 2021. *sjPlot: Data Visualization for Statistics in
Social Science*. <https://strengejacke.github.io/sjPlot/>.

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

<div id="ref-R-hablar" class="csl-entry">

Sjoberg, David. 2020. *Hablar: Non-Astonishing Results in r*.
<https://davidsjoberg.github.io/>.

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

<div id="ref-knitr2014" class="csl-entry">

Xie, Yihui. 2014. “Knitr: A Comprehensive Tool for Reproducible Research
in R.” In *Implementing Reproducible Computational Research*, edited by
Victoria Stodden, Friedrich Leisch, and Roger D. Peng. Chapman;
Hall/CRC. <http://www.crcpress.com/product/isbn/9781466561595>.

</div>

<div id="ref-knitr2015" class="csl-entry">

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton,
Florida: Chapman; Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-R-knitr" class="csl-entry">

———. 2021. *Knitr: A General-Purpose Package for Dynamic Report
Generation in r*. <https://yihui.org/knitr/>.

</div>

</div>
