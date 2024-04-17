#LMM linear mixed models
library(nlme)
library(lattice)
library(ggplot2)

data("Orthodont")
head(Orthodont)
dim(Orthodont)
sum(is.na(Orthodont))

plot(Orthodont, ylab=("distance"))
pairs(Orthodont)

lmList.obj <- lmList(distance ~ age | Subject, data=Orthodont)

summary(lmList.obj)


plot(intervals(lmList.obj))

plot(lmList.obj, resid(., type="pool") ~ fitted(.) | Sex, abline=0,
     id=0.05)

plot(lmList.obj, Subject ~ resid(.))

plot(lmList.obj, distance ~ fitted(.) | Subject, abline = c(0,1))

pairs(lmList.obj, id=0.01, adj=0.5)

lmList.obj <- lmList(distance ~ I(age-11)| Subject, data=Orthodont)

coef(lmList.obj)

plot(intervals(lmList.obj))

model <- lm(distance~age + Sex + age*Sex, data=Orthodont)
summary(model)

plot(model, which = 2)

boxplot(resid(model)~Subject, data=Orthodont, las= 2)
abline(a=0, b=0)

#install.packages("lme4")
library(lme4)
lmm <- lmer(distance ~ Sex * age + (1|Subject), data = Orthodont)
summary(lmm)

VarCorr(lmm)
fixef(lmm)

anova(lmm, model, test="Chisq")
