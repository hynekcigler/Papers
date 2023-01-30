###########################################################################
###                                                                     ###
### (c) 2023 | Hynek Cígler                                             ###
### contact: cigler@fss.muni.cz                                         ###
### Department of Psychology & Institute for Psychological Research     ###
### Faculty of Social Studies, Masaryk University                       ###
###                                                                     ###
### Quantification of Events in the Social Sciences:                    ###
### Neglected Heritage of Ferguson's Committee                          ###
###                                                                     ###
### Invited speak at PhD Existence, 01/30 2023, Olomouc                 ###
###                                                                     ###
######### Mon Jan 30 10:14:46 2023 ########################################


# Packages and data ---------------------------------------------------------------------------

library(ShinyItemAnalysis)
library(psych)
library(lavaan)
library(semTools)
source("https://raw.githubusercontent.com/hynekcigler/Functions/master/extract.R")

dat <- ShinyItemAnalysis::HeightInventory
dat <- dat[!names(dat) %in% c("TopShelfEasy", "NotBusLegsEnoughSpace")]

ordinal <- names(dat)[1:24]


# Describe ------------------------------------------------------------------------------------

alpha(dat[dat$Gender == "F", ordinal])$total
alpha(dat[dat$Gender == "M", ordinal])$total

# CFA -----------------------------------------------------------------------------------------

mods <- paste0("\n", 
               "TallerThanM ~~ NotSmallerThanM", "\n", 
               "TallerThanF ~~ NotSmallerThanW", "\n", 
               "ShortBed ~~ ShortBlanket")

## Unidimensional CFA
mod1 <- paste0("H =~ ", paste(names(dat)[1:24], collapse = "+"), mods)


cfa1 <- cfa(mod1, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T)
summary(cfa1, std=T, fit = T)

## Two-factors
mod2 <- paste0("P =~ ", paste(names(dat)[1:12], collapse = "+"), "\n", 
               "N =~ ", paste(names(dat)[13:24], collapse = "+"), "\n", 
               "P ~~ N", mods)
cfa2 <- cfa(mod2, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T)
summary(cfa2, std=T, fit = T)

## S-1 bifactor
mod3 <- paste0("H =~ ", paste(names(dat)[1:24], collapse = "+"), "\n", 
               "N =~ ", paste(names(dat)[13:24], collapse = "+"), "\n", 
               "H ~~ 0*N", mods)
cfa3 <- cfa(mod3, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T)

## bifactor
mod4 <- paste0("H =~ ", paste(names(dat)[1:24], collapse = "+"), "\n", 
               "P =~ ", paste(names(dat)[1:12], collapse = "+"), "\n", 
               "N =~ ", paste(names(dat)[13:24], collapse = "+"), "\n", 
               "H ~~ 0*P + 0*N \n P ~~ 0*N", mods)
cfa4 <- cfa(mod4, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T)

## 2-factors EFA
mod_efa2 <- paste0("efa('efa1')*F1 + efa('efa1')*F2 =~ ", paste(names(dat)[1:24], collapse = "+"), mods)
efa2 <- cfa(mod_efa2, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T)
summary(efa2, fit=T, std=T)
lavInspect(efa2, what="std")$"F"$lambda

semtext(list(cfa1, cfa2, cfa3, cfa4, efa2), output = "table", cfi = T, 
        modelnames = c("1F", "2F", "S-1 BI", "BI", "EFA"))
semtext(efa2)


# SEM -----------------------------------------------------------------------------------------

## 2F
mod2_cm_A <- paste0(mod2, "\n", 
                    "P =~ HeightCM", "\n",  
                    "N =~ HeightCM", "\n", 
                    "HeightCM ~~ 0*HeightCM")
mod2_cm_B <- paste0(mod2, "\n", 
                    "P =~ HeightCM", "\n",  
                    "HeightCM ~~ 0*HeightCM")
sem2_A <- cfa(mod2_cm_A, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T)
sem2_B <- cfa(mod2_cm_B, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T)

semtext(list(sem2_A, sem2_B))
modindices(sem2_B, sort. = T, maximum.number = 40)
lavInspect(sem2_A, what="std")$"F"$lambda
lavInspect(sem2_B, what="std")$"F"$lambda


## EFA1
mod_cm_efa2_A <- paste0("efa('efa1')*F1 + efa('efa1')*F2 =~ ", paste(names(dat)[1:25], collapse = "+"), 
                      mods, "\n", 
                      "HeightCM ~~ 0*HeightCM"
                      )
mod_cm_efa2_A <- paste0("efa('efa1')*F1 + efa('efa1')*F2 =~ ", paste(names(dat)[1:25], collapse = "+"), 
                        mods, "\n" #, 
                        # "HeightCM ~~ 0*HeightCM"
                        )
sem2_cm_efa_A <- cfa(mod_cm_efa2_A, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T)
summary(sem2_cm_efa_A, fit=T, std=T, rsquare=T)
lavInspect(sem2_cm_efa_A, what="std")$"F"$lambda
modindices(sem2_cm_efa_A, sort. = T, maximum.number = 30)


## EFA2
mod_cm_efa2_B <- paste0("F1 =~ ", paste(names(dat)[1:25], collapse = "+"), "\n", 
                        "F2 =~ ", paste(names(dat)[1:24], collapse = "+"), 
                        mods, "\n", 
                        "F1 ~~ 0*F2", "\n",
                        "HeightCM ~~ 0*HeightCM")

sem2_cm_efa_B <- cfa(mod_cm_efa2_B, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T, rotation="none")
summary(sem2_cm_efa_B, fit=T, std=T, rsquare=T)
modindices(sem2_cm_efa_B, sort. = T, maximum.number = 30)


## EFA3
mod_cm_efa2_C <- paste0("efa('efa1')*F1 + efa('efa1')*F2 =~ ", paste(names(dat)[1:24], collapse = "+"),
                        "\n",
                        paste0(paste(names(dat)[1:24], collapse = "+"), " ~ HeightCM"), 
                        mods, "\n")
sem2_cm_efa_C <- cfa(mod_cm_efa2_C, dat, group = "Gender", ordered = ordinal, std.lv=T, verbose = T, 
                     fixed.x = F, conditional.x = F)

summary(sem2_cm_efa_C, std=T, fit=T)

semtext(list(sem2_A, sem2_B, sem2_cm_efa_A, sem2_cm_efa_B, sem2_cm_efa_C), output = "table", cfi = T)

lavTestLRT(sem2_cm_efa_A, sem2_cm_efa_B, sem2_cm_efa_C)



lavInspect(sem2_cm_efa_A, what="std")$"F"$lambda
lavInspect(sem2_cm_efa_B, what="std")$"F"$lambda

scores1 <- lavPredict(sem2_cm_efa_B)

semtext(list(sem2_cm_efa_A, sem2_cm_efa_B, sem2_cm_efa_C), output = "table", cfi = T)


# Weber-Fechner -------------------------------------------------------------------------------

x <- seq(0,5, by=.01)
weber <- function(x, k) {
  k*log(x)
}
fechner <- function(x, k, I0) {
  k*log(x/I0)
}
stevens <- function(x, k, a) {
   k * x**a
}



# Stevens power law plot ----------------------------------------------------------------------

plot(x, stevens(x, k = 1, a=.33), type = "l", col = yarrr::piratepal("basel")[1], lwd=3, 
     main = "Stevensův exponenciální zákon", xlab = "stimulus", ylab = "vjem", ylim = c(0,3))
lines(x, stevens(x, k = 1, a=.67), lwd = 3, col = yarrr::piratepal("basel")[2])
lines(x, stevens(x, k = 1, a=1), lwd = 3, col = yarrr::piratepal("basel")[3])
lines(x, stevens(x, k = 1, a=1.4), lwd = 3, col = yarrr::piratepal("basel")[4])
lines(x, stevens(x, k = 1, a=3.5), lwd = 3, col = yarrr::piratepal("basel")[5])
legend("bottomright", c("jasnost (5° ve tmě); a=.33", "hlasitost (3000 Hz); a=.67", 
                        "délka (úsečka); a=1", 
                        "slaná chuť; a=1.4", "elektrický proud (prst); a=3.5"), 
       lwd = 3, col =  yarrr::piratepal("basel"), inset = .01, cex = .8)
