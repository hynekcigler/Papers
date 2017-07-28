


## NOTES: This analysis was performed by Hynek Cigler with these package version:
# R: 3.3.3
# psych: 1.7.5
# semTools: 0.4-14
# lavaan: 0.5-23.1097
# mirt: 1.25


# Load data and packages --------------------------------------------------

setwd("c:/Users/Hynek/Dropbox/MU/Články jiné/Maximizing/Analýza/") ## set working directory
data <- read.csv2("komplet_adult_1.csv") ## read data
names(data)[1] <- "ID" ## properly ID column name

## load packages
library(psych)
library(mirt)

allitems <- c(1:34)+1 ## column with MI items
shortitems <- c(1,3,4,6,9,12,13,14,17,22,24,26,27,29,32)+1 ## columns with items passed to shortened version of MI

s_items <- c(1:10) ## Satisfaction items of the full version
dd_items = c(11:22) ## decision difficulty items of the full version
as_items <- c(23:34) ## alternative search items of the full version

describe(data[,allitems]) ## basic descriptives

set.seed(2582015) ## set sead for selecting respondents for study 1
data$resps <- rbinom(nrow(data), 1, 2/3) ## 1 = select to data set 1; 0 = don't select to data set 1
table(data$resps);table(data$resps)/sum(nrow(data))*100 ## subtotal N



# Study 1 -----------------------------------------------------------------

# * Analysis with data set 1 ------------------------------------------------------------

## full model
model_irt34 <- mirt.model("
satis = 1-10 ## items 1-10
decis = 11-22 ## items 11-22
search = 23-34 ## items 23-34
COV = satis*decis, satis*search, decis*search
")

irt1 <- mirt(data=data[data$resps==1,allitems], model=model_irt34, itemtype = "graded", SE=T) ## complete model for Data Set 1
irt1item <- itemfit(irt1) ## item fit
irt1fit <- M2(irt1) ## model fit
irt1coef <- coef(irt1, simplify=T) ## model parameters
summary(irt1) ## summary stats

irt1coef
coef(irt1, simplify=F, CI=.95)$GroupPars ## are latent trait corrs sig. at p < .05?
coef(irt1, simplify=F, CI=.99)$GroupPars ## are latent trait corrs sig. at p < .01?
coef(irt1, simplify=F, CI=.999)$GroupPars ## are latent trait corrs sig. at p < .001?

write.csv2(cbind(irt1coef$items, irt1item), "irt1items.csv") ## all the items parameters

irt1scores <- fscores(irt1, full.scores = TRUE, full.scores.SE = TRUE, method="EAP") ## expected a-posteriori scores
empirical_rxx(irt1scores) ## IRT reliability

## cronbach's alpha for each scale:
alpha(data[data$resps==1,s_items+1])$total
alpha(data[data$resps==1,dd_items+1])$total
alpha(data[data$resps==1,as_items+1])$total

## Inspection of residual matrix
irt1resid1 <- residuals(irt1, type="LDG2", df.p = T) ## print residual matrices (p-values and df)
irt1resid1 ## print residual matrices (chisq and Cramer's V)

sum(pchisq(abs(irt1resid1[lower.tri(irt1resid1)]), 16, lower.tail = F) < .01) ## number of all signifficant LD
sum(pchisq(abs(irt1resid1[lower.tri(irt1resid1)]), 16, lower.tail = F) < .01)/sum(lower.tri(irt1resid1)) ## relative number of all signifficant LD

sum(pchisq(irt1resid1[lower.tri(irt1resid1)][irt1resid1[lower.tri(irt1resid1)] > 0], 16, lower.tail = F) < .01) ## number of signifficant LD with higher dependece
sum(pchisq(irt1resid1[lower.tri(irt1resid1)][irt1resid1[lower.tri(irt1resid1)] > 0], 16, lower.tail = F) < .01)/sum(lower.tri(irt1resid1)) ## relative number of the same

## selected residual relations:
irt1resid1[10,dd_items]; median(irt1resid1[10,dd_items])
irt1resid1[5,as_items]; median(irt1resid1[10,dd_items])
irt1resid1[25,26]
irt1resid1[5,23]
irt1resid1[29,30]
irt1resid1[23, 31]
irt1resid1[16,18]
irt1resid1[7,8]
irt1resid1[15,16]

## visual inspection (you can change item number)
itemplot(irt1, item=10, drop.zeros = T, type = "trace", CE=T)
itemplot(irt1, item=10, drop.zeros = T, type = "trace", CE=F)

itemplot(irt1, item=11, drop.zeros = T, type = "trace", CE=T)
itemplot(irt1, item=11, drop.zeros = T, type = "trace", CE=F)


## The same model, but for both Data Sets 1 and 2 (needed below)
irt1b <- mirt(data=data[,allitems], model=model_irt34, itemtype = "graded", SE=T) ## complete model for both Data Sets


#* Cross-validation of the short MI form --------------------------------------------------------

#** Shortened model, Data set 1 -------------------------------------------

model_irt15 <- mirt.model("
satis = 1-5 ## items 1-10
decis = 6-10 ## items 11-22
search = 11-15 ## items 23-34
COV = satis*decis, satis*search, decis*search
")

irt2 <- mirt(data=data[data$resps==1,shortitems], model=model_irt15, itemtype = "graded", SE=T) ## shortened model for Data Set 1
irt2item <- itemfit(irt2) ## item fit
irt2fit <- M2(irt2) ## model fit
irt2coef <- coef(irt2, simplify=T) ## model parameters
summary(irt2) ## summary stats


#** Shortened model, Data set 2 -------------------------------------------

irt3 <- mirt(data=data[data$resps==0,shortitems], model=model_irt15, itemtype = "graded", SE=T) ## shortened model for Data Set 2
irt3item <- itemfit(irt3) ## item fit
irt3fit <- M2(irt3) ## model fit
irt3coef <- coef(irt3, simplify=T) ## model parameters
summary(irt3) ## summary stats


#** multigroup invariance checked ------------------------------------------------------------

irt4a <- multipleGroup(data[,shortitems], model=model_irt15, itemtype = "graded", SE=T, group=as.character(data$resps)) ## configural
irt4b <- multipleGroup(data[,shortitems], model_irt15, group = as.character(data$resps), 
                       invariance=c('slopes')) # metric
irt4c <- multipleGroup(data[,shortitems], model_irt15, group = as.character(data$resps), 
                       invariance=c('slopes', 'intercepts', 'free_var','free_means')) # scalar2
irt4d <- multipleGroup(data[,shortitems], model_irt15, group = as.character(data$resps), 
                       invariance=c('slopes', 'intercepts', 'free_var')) # scalar1
irt4e <- multipleGroup(data[,shortitems], model_irt15, group = as.character(data$resps), 
                       invariance=c('slopes', 'intercepts')) # strict parallel
M2(irt4a)
M2(irt4b)
M2(irt4e)
anova(irt4a,irt4b) ## model comparison
anova(irt4b,irt4e) ## model comparison



#** Both Data Sets 1 and 2 merged to one sample -------------------------------------------------------------

irt5 <- mirt(data=data[,shortitems], model=model_irt15, itemtype = "graded", SE=T) ## shortened model for the full sample
irt5fit <- M2(irt5) ## model fit
summary(irt5) ## summary stats
irt5fit

irt5item <- itemfit(irt5) ## item fit
irt5coef <- coef(irt5, simplify=T) ## model parameters


coef(irt5, simplify=F, CI=.95)$GroupPars ## are latent trait cors sig. at p < .05?
coef(irt5, simplify=F, CI=.99)$GroupPars ## are latent trait cors sig. at p < .01?
coef(irt5, simplify=F, CI=.999)$GroupPars ## are latent trait cors sig. at p < .001?

write.csv2(cbind(irt5coef$items, irt5item), "irt5items.csv") ## all the items parameters

irt5scores <- fscores(irt5, full.scores = TRUE, full.scores.SE = TRUE, method="EAP") ## expected a-posteriori scores
empirical_rxx(irt5scores) ## IRT reliability

## cronbach's alpha for each scale:
alpha(data[,shortitems][,1:5])$total
alpha(data[,shortitems][,6:10])$total
alpha(data[,shortitems][,11:15])$total



#* Correlation to full version -----------------------------------------------

irt1bscores <- fscores(irt1b, full.scores = TRUE, full.scores.SE = TRUE, method="EAP") ## expected a-posteriori scores of the full scale
colnames(irt1bscores) <- c("full1", "full2", "full3", "SE_full1", "SE_full2", "SE_full3") ## rename variables

cor(cbind(irt1bscores[,1:3], irt5scores[,1:3])) ## correlation estimates
corr.test(cbind(irt1bscores[,1:3], irt5scores[,1:3])) ## correlation estimates with p-values




#* Reliability of the shortened scale -----------------------------------------------------------

library(lavaan)
library(semTools)

CFAmodel <- "
S =~ MIS1+MIS3+MIS4+MIS6+MIS9
DD =~ MIDD2+MIDD3+MIDD4+MIDD7+MIDD12
AS =~ MIAS2+MIAS4+MIAS5+MIAS7+MIAS10
"

cfa1 <- cfa(model=CFAmodel, data=data, ordered = names(data))
summary(cfa1, std=T, ci=T, fit=T)
reliability(cfa1) ## Be careful, alpha is Ordinal alpha coefficient, which is hardly to interpret (see Zumbo, B. D., Gadermann, A., & Zeisser, C. (2007). Ordinal Versions of Coefficients Alpha and Theta for Likert Rating Scales. Journal of Modern Applied Statistical Methods. Retrieved from http://digitalcommons.wayne.edu/jmasm/vol6/iss1/4)



# Study 2 -----------------------------------------------------------------

#* unidimensional models -------------------------------------------------

## SHS
SHSmodel <- "
SHS =~ SHS1+SHS2+SHS3+SHS4
"
cfa_shs <- cfa(model=SHSmodel, data=data, ordered = names(data))
summary(cfa_shs, std=T,fit=T)
reliability(cfa_shs)

## LOTR
LOTRmodel <- "
LOTR =~ LOTR1+LOTR2+LOTR3+LOTR4+LOTR5+LOTR6
"
cfa_lotr <- cfa(model=LOTRmodel, data=data, ordered = names(data))
summary(cfa_lotr, std=T,fit=T)
reliability(cfa_lotr)

## GSES
GSESmodel <- "
GSES =~ GSES1+GSES2+GSES3+GSES4+GSES5+GSES6+GSES7+GSES8+GSES9+GSES10
"
cfa_gses <- cfa(model=GSESmodel, data=data, ordered = names(data))
summary(cfa_gses, std=T,fit=T)
reliability(cfa_gses)

## Regret
Rmodel <- "
R =~ R1+R2+R3+R4+R5
"
cfa_r <- cfa(model=Rmodel, data=data, ordered = names(data))
summary(cfa_r, std=T,fit=T)
reliability(cfa_r)

modificationindices(cfa_r, sort.=T) ## MI foor Regret scale
residuals(cfa_r, type="cor") ## Residual correlations for Regret scale

cfa_r2 <- cfa(model=paste0(Rmodel, "R4 ~~ NA*R5"), data=data, ordered = names(data)) ## Regret model revised
summary(cfa_r2, std=T,fit=T) 
lavTestLRT(cfa_r, cfa_r2, method="satorra.bentler.2010")
reliability(cfa_r2)



#* Full model with shortened SMI ------------------------------------------------------------

FULLmodel <- "
S =~ MIS1+MIS3+MIS4+MIS6+MIS9
DD =~ MIDD2+MIDD3+MIDD4+MIDD7+MIDD12
AS =~ MIAS2+MIAS4+MIAS5+MIAS7+MIAS10

SHS =~ SHS1+SHS2+SHS3+SHS4
LOTR =~ LOTR1+LOTR2+LOTR3+LOTR4+LOTR5+LOTR6
GSES =~ GSES1+GSES2+GSES3+GSES4+GSES5+GSES6+GSES7+GSES8+GSES9+GSES10
R =~ R1+R2+R3+R4+R5

R4 ~~ NA*R5
"


cfa_full <- cfa(model=FULLmodel, data=data, ordered = names(data))
summary(cfa_full, std=T,fit=T)
reliability(cfa_full)
lavInspect(cfa_full, what="std")$psi ## latent correlations


# * Full model with original MI ----------------------------------------------------

FULLmodel2 <- "
S =~ MIS1 + MIS2 + MIS3 + MIS4 + MIS5 + MIS6 + MIS7 + MIS8 + MIS9 + MIS10
DD =~ MIDD1 + MIDD2 + MIDD3 + MIDD4 + MIDD5 + MIDD6 + MIDD7 + MIDD8 + MIDD9 + MIDD10 + MIDD11 + MIDD12
AS =~ MIAS1 + MIAS2 + MIAS3 + MIAS4 + MIAS5 + MIAS6 + MIAS7 + MIAS8 + MIAS9 + MIAS10 + MIAS11 + MIAS12

SHS =~ SHS1+SHS2+SHS3+SHS4
LOTR =~ LOTR1+LOTR2+LOTR3+LOTR4+LOTR5+LOTR6
GSES =~ GSES1+GSES2+GSES3+GSES4+GSES5+GSES6+GSES7+GSES8+GSES9+GSES10
R =~ R1+R2+R3+R4+R5

R4 ~~ NA*R5
"


cfa_full2 <- cfa(model=FULLmodel2, data=data, ordered = names(data))
summary(cfa_full2, std=T,fit=T)
nullRMSEA(cfa_full2)
reliability(cfa_full2)
lavInspect(cfa_full2, what="std")$psi ## latent correlations



