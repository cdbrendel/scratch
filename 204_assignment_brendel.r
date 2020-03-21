rm(list=ls(all=TRUE))
library(car); library(effects); library(lme4); library(MuMIn)

ilogit <- function (x) {
   1/(1 + exp(-x))
}

df <- read.csv("./204_assignment_brendel.csv", header = TRUE)
df$CENT <- factor(df$CENT)
df$SA_POS <- relevel(df$SA_POS, 'BEFORE')
## Exploration
summary(df)

plot(df$SA_POS)
# Many more instances of SA_POS = BEFORE than AFTER. This is consistent with Sapp (2019)'s claims that prenominal sá is more frequent overall.

plot(df$SA_POS ~ df$CENT)
# Frequency of SA_POS = AFTER is much higher up to and including 15th century texts, whereafter it is consistently lower (except for potential outliers in 19th century?).

plot(df$SA_POS ~ df$HAS_RC)
# This looks like a fairly clear preference for SA_POS = AFTER when the NP contains a dependent relative clause, which seems to be what we'd expect.

plot(df$TEXT)
plot(df$SA_POS ~ df$TEXT)
# It looks like some texts contribute many more cases of an RC with sá than other texts. Additionally, while most texts show SA_POS = AFTER is more frequent, they seem to differ greatly in the starkness of the difference in frequency between the two levels.


## Without random effects (bad model)
summary(glm(SA_POS ~ HAS_RC, data=df))

## With random effects
LEVEL1 <- df$CENT
LEVEL2 <- df$CENT:df$TEXT
m.1 <- glmer(SA_POS~1+HAS_RC+(1|LEVEL1) + (1|LEVEL2), data=df, family="binomial")
summary(m.1, correlation=FALSE)

# Can CENT be deleted?
m.2 <- glmer(SA_POS~1+HAS_RC+(1|LEVEL2), data=df, family="binomial")
anova(m.1, m.2)
# No; significant difference

# Can CENT/TEXT be deleted?
m.3 <- glmer(SA_POS~1+HAS_RC+(1|LEVEL1), data=df, family="binomial")
anova(m.1, m.3)
# No.

# Making model more readable now.
m.1r <- glmer(SA_POS~1+HAS_RC+(1|CENT/TEXT), data=df, family="binomial")

m.final <- m.1r

MuMIn::r.squaredGLMM(m.final)
# Well, the random effects help, but even the conditional R^2 is still not fantastic...


m.null <- glmer(SA_POS ~ 1 + (1|CENT/TEXT), family=binomial, data=df)
anova(m.final, m.null, test="Chisq")
# Still doing significantly better than the null model

LMERConvenienceFunctions::relLik(m.final, m.null)


# compute the predicted probabilities of the highest interaction with the effects package and store them in preds.hyp.sep
plot(ia.sep <- effect("HAS_RC", m.final, type="response", grid=TRUE))
preds.hyp.sep <- data.frame(ia.sep$x, PREDICTIONS=ilogit(ia.sep$fit), LOWER=ilogit(ia.sep$lower), UPPER=ilogit(ia.sep$upper))
preds.hyp.sep.split <- split(preds.hyp.sep, preds.hyp.sep$HAS_RC); preds.hyp.sep.split




df$PREDS.NUM.MM <- fitted(m.final) # or predict(model.final, type="response")
df$PREDS.CAT.MM <- factor(ifelse( # make predictions: if
   df$PREDS.NUM.MM>=0.5,          # the pred. prob. of MATCH=="you" is >-0.5
   levels(df$SA_POS)[2],           # then predict "you"
   levels(df$SA_POS)[1]))          # otherwise predict "I"

table(df$SA_POS, df$PREDS.CAT.MM)
# There are quite a few false positives here.
matrix(c("true negatives", "false negatives", "false positives", "true positives"),
       ncol=2, dimnames=list(DATA=levels(df$SA_POS), PREDICTIONS=sort(unique(df$PREDS.CAT.MM))))
### accuracy:
(5216+144) / length(df$PREDS.CAT.MM) # 86.19% accuracy; not bad!

### precision:
5216/(5216+730) # 87.73%

### recall/sensitivity:

5216/(5216+129) # 97.59%

baseline.1 <- max(prop.table(table(df$SA_POS)))   # 85.95%
baseline.2 <- sum(prop.table(table(df$SA_POS))^2) # 75.84%
# Mm... it barely clears the first baseline, but does do better than the second baseline.

plot(allEffects(m.final),
     ylab="Predicted probability of 'BEFORE'", ylim=c(0,1),
     type="response", grid=TRUE)
