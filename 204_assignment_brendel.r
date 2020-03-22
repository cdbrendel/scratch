### Background

## Description
# In Old Icelandic, a particular demonstrative marker, _sá_, can occur or after before its head noun. Sapp (2019) has argued that postnominal _sá_ is associated with relative clause (RC) constructions, such as  Maður sá [er ég kenni]. 'The man [that I know]', and that this demonstrative grammaticalized into a true relative pronoun. He claims that 65% of all _sá_ occur with a RC in the same NP, and that 91% percent of _sá_ with an RC show the postnominal order. The co-occurrence of a postnominal demonstrative marker with an adjacent RC is implicated as the primary factor in grammaticalization of _sá_ to a relative pronoun.

# This project is a first attempt to quantify Sapp (2019)'s purely descriptive frequency counts, based on my own analysis of the Icelandic Parsed Historical Corpus (Wallenbeg et al. 2011), which is also the source for Sapp's data. I generated this dataset with my processing functions I wrote in Python for this corpus. I intend to test one claim he makes: that postnominal _sá_ (SA_POS) occurs more frequently in an NP with an RC (HAS_RC). Additionally, he claims that when _sá_ is preceded by an adjective, _sá_ functions as a relative pronoun. In other words, we would expect SA_POS = AFTER and ADJ_PREV = Y to predict HAS_RC = Y. As this is a corpus study, century of publication (CENT) and the particular work from which a case is sourced (TEXT) are treated as nested random effects.

## Issues:
# There are a few flaws from the outset that I am aware about at the beginning of this analysis that I have not addressed. One is that Sapp (2018) and I get different amounts of raw data--here, I collect _sá_ lemmas whose part-of-speech is DEM and where a N head is present in the NP (ie, where _sá_ is not substantive), n = 2725 (n = 3659 before removing substantive uses). Sapp, however, finds n = 4371 total instances of _sá_ in this period, a number which I cannot replicate even if I assume he is only looking for matching lemmas (i.e., more broadly than I am, potentially matching mistagged or dubiously-tagged isntances). Even if I search for the lemma in the lamest way possible--by literally using GNU grep directly on the raw tagged files (thereby factoring out any errors in my Python code)--I can only find a maximum 3725 instances of _sá_. This is a topic I hope to discuss with you next quarter; yes, I am using custom Python libraries I made, and he is instead using an older Java program (CorpusSearch2, developed for the Penn corpus), and this discrepency could be explained by an error in my code (or in the program he's using, or in the queries he constructs, which he does not publish in full), but I can't imagine that my simple-stupid grep check would *under*estimate the number of occurrences. I may not be a great statistician (lol), but I think I'm anal/proficient enough at programming and text processing to trust my data selection fairly well; I'd love to get your input next quarter on what could be going on here.

# Another large issue with my analysis here is that the genre of text should be taken into account (especially texts which were translations, which might be based on Latin constructions which show a similar relativization strategy), but I was unsure how to represent a nested structure where two factors subsume another, but are not cleanly nested in each other. (Of course, I could and should have asked you earlier and gotten your advice before submitting this). What I mean is that by knowing TEXT, you know CENT (CENT/TEXT), and by knowing TEXT, you know GENRE (GENRE>TEXT), but by knowing GENRE, you do NOT know CENT (so NOT CENT>GENRE>TEXT), since all of the various genres of text occur sporadically throughout the entire corpus. Yet CENT>GENRE is probably still a relevant effect to include, since a Bible translation in 1150 may be quite different from a Bible translation in 1480. So, I have left the genre of the texts out of this model as I frankly do not know how to handle a complicated structure like this; I hope to revisit this project in Corpus Ling next quarter, though I know that ideally this issue should have been addressed should have been in this project, as well.

## Sources
# Sapp, Christopher D. 2019. Arrested development: Case attraction as a transitional stage from Old Icelandic demonstrative to relative sá. Language 95. e1–e40.
# Wallenberg, J., A. Ingason, E. Sigurðsson, and E. Rögnvaldsson. 2011. Icelandic Parsed Historical Corpus.


### Setup
rm(list=ls(all=TRUE))
library(car); library(effects); library(lme4); library(MuMIn)
ilogit <- function (x) {
   1/(1 + exp(-x))
}



### Data preparation
df <- read.csv("./204_assignment_brendel.csv", header = TRUE)
df$CENT <- factor(df$CENT)
df$SA_POS <- relevel(df$SA_POS, 'BEFORE')

## Exploration
summary(df)

plot(df$SA_POS)
# Many more instances of SA_POS = BEFORE than AFTER. This is consistent with Sapp (2019)'s claims that prenominal sá is more frequent overall. 

plot(df$HAS_RC)
# Most _sá_, regardless of order with respect to its head N, occur with an RC.

plot(df$SA_POS ~ df$CENT)
plot(df$HAS_RC ~ df$CENT)
# Some variance here, but this strikes me as more or less consistent.

plot(df$TEXT)
plot(df$SA_POS ~ df$TEXT)
plot(df$HAS_RC ~ df$TEXT)
# Some texts contribute many, many more cases of sá than others. I have no reason a priori that a text with relatively fewer _sá_s should not display the same pattern, if one exists: i.e., I don't think a random slope is justified here a priori, but a random intercept seems to make sense to me here as I think it should account for the difference in frequency between text? 

plot(df$HAS_RC ~ df$SA_POS)
# It looks like NPs with and without RCs occur about equally frequently regardless of order of _sá_. When SA_POS = AFTER, it looks like we would clearly predict HAS_RC = Y.

plot(df$ADJ_PREV)
# Yikes. Very uneven distribution. By far, most have no previous adjective. But the ones that do are expected to predict RCs.

plot(df$HAS_RC ~ df$ADJ_PREV)
# Yes, ADJ_PREV = Y seems to occur overwhelmingly with NPs that have an RC.

### Modeling
## Baselines
baseline.1 <- max(prop.table(table(df$HAS_RC)))   # 58.79%
baseline.2 <- sum(prop.table(table(df$HAS_RC))^2) # 51.54%

## Without random effects (bad, basic model--mostly doing so I can compare to lmer later)
summary(m.1.lrm <- glm(HAS_RC ~ SA_POS * ADJ_PREV, family="binomial", data=df))
# This suggests SA_POS is the only thing that matters; when SA_POS = AFTER, we'd predict HAS_RC = Y. Other effects seem insignificant. 
drop1(m.1.lrm, test="Chisq")
m.2.lrm <- update(m.1.lrm, ~. -SA_POS:ADJ_PREV)
drop1(m.2.lrm, test="Chisq")
summary(m.2.lrm)

pchisq(m.2.lrm$deviance, m.2.lrm$df.residual,lower.tail=FALSE) # Looking like a lot of overdispersion here


predictions.num.lrm <- df$PREDS.LRM <- fitted(m.2.lrm)
predictions.cat.lrm <- ifelse(predictions.num.lrm>=0.5, "Y", "N")
table(df$HAS_RC, predictions.cat.lrm)

sum(df$HAS_RC==predictions.cat.lrm) / length(predictions.cat.lrm) # Accuracy: 60.84%, higher than baselines (58.79% and 51.54%)
sum(dbinom(sum(df$HAS_RC==df$predictions.cat.lrm):length(df$predictions.cat.lrm), length(df$predictions.cat.lrm), baseline.1))
sum(dbinom(sum(df$HAS_RC==df$predictions.cat.lrm):length(df$predictions.cat.lrm), length(df$predictions.cat.lrm), baseline.2))
# These look alright, but with that overdispersion and that accuracy, can we do any better with a mixed effects model?
# At this point, I'm going to switch to a mixed effects model. 

## With random effects
LEVEL1 <- df$CENT
LEVEL2 <- df$CENT:df$TEXT
m.1.lmer <- glmer(HAS_RC~SA_POS*ADJ_PREV + (1|LEVEL1) + (1|LEVEL2), data=df, family="binomial")
summary(m.1.lmer, correlation=FALSE)

# Can CENT be deleted?
m.2.lmer <- glmer(HAS_RC~SA_POS*ADJ_PREV              + (1|LEVEL2), data=df, family="binomial")
summary(m.2.lmer)
anova(m.1.lmer, m.2.lmer)
# Sure.

# Making model more readable from now on now.
m.2.lmer <- glmer(HAS_RC~SA_POS*ADJ_PREV+(1|CENT/TEXT), data=df, family="binomial")
drop1(m.2.lmer, test="Chisq")

m.3.lmer <- glmer(HAS_RC~SA_POS+ADJ_PREV+(1|CENT/TEXT), data=df, family="binomial")
drop1(m.3.lmer, test="Chisq")
# Stop selection. It looks like, unlike in the plain fixed-effects model, when the random effect is included, the model is (just barely) significantly better keeping ADJ_PREV in.

m.lmer.final <- m.3.lmer
fixef(m.lmer.final)
ranef(m.lmer.final)
fixef(m.lmer.final)[1] + ranef(m.lmer.final)[[1]] 
summary(m.lmer.final)
# ... but ADJ_PREV is not significant overall. So, HAS_RC is the only significant effect here.  
 
### Validation and model exploration
## How much variance is accounted for by model?
MuMIn::r.squaredGLMM(m.lmer.final) # R2m = 0.2380687  R2c = 0.2721464
# Well, that's a pretty terrible R2. At least the random effects are contributing something...


# Is this any better than the other model?
anova(m.lmer.final, m.2.lrm) # ***
# Seems so.

## Compare to null model
m.null <- glmer(HAS_RC ~ 1 + (1|CENT/TEXT), family=binomial, data=df)
anova(m.lmer.final, m.null, test="Chisq")
LMERConvenienceFunctions::relLik(m.lmer.final, m.null) # 3.289e+88
# Still doing significantly better than the null model

# Record predictions from lmer model (both with and without random effects, which I'll use variously througohut)
df$PREDSwithoutRAND <- predict(m.lmer.final, re.form=NA,   type="response") # no re
predictions.num <- df$PREDSwithRAND <- predict(m.lmer.final, re.form=NULL, type="response") # with re
predictions.cat <- ifelse(predictions.num>=0.5, "Y", "N")


## Accuracy and comparison with baselines
sum(df$HAS_RC==predictions.cat) / length(predictions.cat) # Accuracy: 67.01%
# This is better than both baseline 1 (58.79%) and baseline 2 (51.54%), though there is clearly a lot missing from this model.
sum(dbinom(sum(df$HAS_RC==df$predictions.cat):length(df$predictions.cat), length(df$predictions.cat), baseline.1)) # 1
sum(dbinom(sum(df$HAS_RC==df$predictions.cat):length(df$predictions.cat), length(df$predictions.cat), baseline.2)) # 1
 
# C-statistic
as.numeric(Hmisc::somers2(fitted(m.lmer.final), with(df, as.numeric(get(as.character(formula(m.lmer.final)[2]))))-1))[1] # C=0.7321575; okay, but could be better

improve.text <- table(df$TEXT, (predictions.cat==df$HAS_RC) & (predictions.cat.lrm!=df$HAS_RC))
   plot(log(rowSums(improve.text)), prop.table(improve.text, 1)[,2], type="n", ylim=c(0, 1), xlab="Predicted HAS_RC in TEXT", ylab="How much the ME/MLM is better in %")
      grid(); text(log(rowSums(improve.text)), prop.table(improve.text, 1)[,2], rownames(improve.text), cex=0.8, font=3)
# Curious that it would be so much better for some texts, but not better at all for others... 

## Explore residuals against predictors and against fitted
stripchart(residuals(m.lmer.final) ~ df$SA_POS, method="jitter")
plot(m.lmer.final, type=c("p", "smooth"), id=0.05)
plot(m.lmer.final, resid(.) ~ fitted(.) | TEXT, abline=c(h=0), lty=1, type=c("p", "smooth"))
# Okay, wow, disgusting. There is clearly a lot of remaining structure in several of the texts (gragas, for example, shows a lot of structure in the latter plot; as in improve.text above, it's also one of the texts whose HAS_RC are predicted much better by the mixed effects model rather than the fixed effects). I suppose this is the distribution of data in my independent variables coming back to bite me. I'm not sure what else to do here, or why some of the texts in particular are so poorly fit (I know the model overall isn't great, but I must be missing something...). I don't know how to solve this, honestly.


### Visualization
## Check how the mixed effect model improves predictions from the fixed effects model
ftable(df$HAS_RC, predictions.cat, predictions.cat.lrm)
plot(jitter(predictions.num.lrm, amount=0.05), predictions.num, xlim=c(0, 1), ylim=c(0, 1), pch=16, col=ifelse(predictions.cat==df$HAS_RC, "darkgreen", "red"),
     xlab="Predicted p of HAS_RC = Y without random effects", ylab="Predicted p of HAS_RC =  Y with random effects")
   abline(h=0.5, lty=2); abline(v=0.5, lty=2); abline(0, 1)
   text(c(0,0,1,1), c(0.25,0.75,0.25,0.75), c("models agree","models disagree","models disagree","models agree"), srt=90)


## Plotting the only significant fixed effect
plot(effect("SA_POS", m.lmer.final, type="response", ylim=c(0, 1), grid=TRUE))
# for (i in 1:24) {
#   abline(ranef(m.lmer.final)$TEXT[i,1], coef(sa.pos))
# }
# The goal with this for loop was to draw the slope for each TEXT random intercept (and then color it according to each TEXT), but I can't figure out how to win the fight with this plot function in time to submit this project. I tried doing plot(effect(...), type="n") so that I could keep the device open to add the ablines, but I'm guessing plot() here is being delegated to some lmer-specific plotting function which eats the argument (it expects type="response", etc.). 

### Summary
# Ultimately, the mixed effects model seems to confirm Sapp (2019)'s claim that the position of _sá_ in relation to the head noun (SA_POS) significantly (p < 2e-16) predicts the appearance of an RC (HAS_RC) in the same noun phrase, though even the confidence interval of HAS_RC when SA_POS = BEFORE still includes both types of clause structure. The presence of a preceding adjective (ADJ_PREV = Y) has no significant (p = 0.120) effect on the occurrence of an RC--indeed, almost all observations which feature a previous adjective also show an RC (HAS_RC = Y), but the effect is not significant across the surveyed period, although the model which incorporated the effect was significantly better than the model without*. So, to me, it seems like some other key predictor[s] is/are missing from this model; certainly SA_POS = AFTER is correlated with HAS_RC = Y, but this information alone is not enough to unambiguously discriminate between the levels of HAS_RC.

# Although this association seems strong, the model built here has several significant problems, some of which I anticipated in my introduction above. One "new" problem includes the overall explanatory power of the model: the variance explained (R2c = 0.2721464) is not great, and though the model is more accurate (67.01%) than the baselines calculated (58.79% & 51.54%), much structure remains in the residuals, especially in certain texts. Although the random effect of Century/Text (CENT/TEXT) significantly improved the model over the fixed effects model, it's clear that there are idiosyncrasies of the texts that I have not accounted for in the model above. Additionally, though the model performs well, I imagine it is overfitted and not very robust--I have to get this submitted to you now, but I plan to split this up into training and testing subsets after I fix the other fundamental problems with this model. I realize now I should have divided the data into different sets during my experimental design to test the robustness of the model.

# * I have to confess, this is hard for me to intuitively grasp. If the effect is not significant in the overall model and is involved in no other interactions, would it have been better to just drop it anyway and accept the impact to fit? I understand the variance is better explained with ADJ_PREV in the model, but it feels like overfitting. 