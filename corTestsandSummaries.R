LIWC_anorexia <- read.csv("LIWC_anorexia.csv")
LIWC_obesity <- read.csv("LIWC_obesity.csv")

cor.test(LIWC_anorexia$Clout, LIWC_anorexia$pronoun) # yes, p-value = 0.0004786
cor.test(LIWC_anorexia$Analytic, LIWC_anorexia$health) # yes, p-value = 0.04891
cor.test(LIWC_anorexia$Analytic, LIWC_anorexia$Period) # yes, p-value = 0.02965
cor.test(LIWC_anorexia$Analytic, LIWC_anorexia$Authentic) # yes, p-value = p-value = 0.008853
cor.test(LIWC_anorexia$Analytic, LIWC_anorexia$negemo) # yes, p-value = 0.04116
cor.test(LIWC_anorexia$negate, LIWC_anorexia$Authentic) # yes, p-value is strong 0.005493

cor.test(LIWC_obesity$Clout, LIWC_obesity$AllPunc) # yes, p-value = 0.004498
cor.test(LIWC_obesity$Analytic, LIWC_obesity$Tone) # yes, p-value = 0.0196
cor.test(LIWC_obesity$Analytic, LIWC_obesity$health) # yes, p-value = 0.009501
cor.test(LIWC_obesity$Analytic, LIWC_obesity$negemo) # yes, p-value = 0.005646


modelOneA <- lm(Analytic ~Period+Authentic+health+pronoun+negate, data=LIWC_anorexia )
summary(modelOneA)

modelOneO <- lm(Clout ~Period+Authentic+health+pronoun+negemo, data=LIWC_anorexia )
summary(modelOneO)


plot(LIWC_anorexia$Authentic, LIWC_anorexia$Negate, xlab="Authentic", ylab="negate", main ="Authentic vs. negate")
abline(lm(LIWC_anorexia$negate ~ LIWC_anorexia$Authentic), col = "blue")

plot(LIWC_anorexia$Analytic, LIWC_anorexia$negemo, xlab="Analytic", ylab="negemo", main ="Analytic vs. negemo")
abline(lm(LIWC_anorexia$negemo ~ LIWC_anorexia$Analytic), col = "blue")
