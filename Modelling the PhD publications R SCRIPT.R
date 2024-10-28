#CATEGORICAL DATA FINAL EXAM
#Lumia, D'Antoni, Bongiovanni

library(pscl)
library(ggplot2)
library(dplyr)
library(vcd)
library(VGAM)
library(DHARMa)
library(MASS)
library(lmtest)

str(bioChemists)
data(bioChemists)

#Article production by 915 graduate students in biochemistry Ph.D. programs:
# - art, count of articles produced during last 3 years of Ph.D.;
# - fem, factor indicating gender of student, with levels Men and Women;
# - mar, factor indicating marital status of student, with levels Single and Married;
# - kid5, number of children aged 5 or younger;
# - phd, prestige of Ph.D. department;
# - ment, count of articles produced by Ph.D. mentor during last 3 years

summary(bioChemists)


########################
# EXPLORATORY ANALYSIS #
########################

palette <- c("darkcyan", "#66CD00","#FFD700","#CD4F39","#8B2252")

#MARGINAL DISTRIBUTION OF THE NUMBER OF ARTICLES PUBLISHED BY THE PHD STUDENT
bioChemists|>
  ggplot(aes(x=art))+
  geom_bar(fill="darkcyan", col="white",linewidth=.3)+
  #scale_x_continuous(breaks = 0:20) +
  labs(y= "FREQUENCY", 
       x= "NUMBER OF ARTICLES")+
  theme_minimal()

#NUMBER OF ARTICLES PUBLISHED BY THE PHD STUDENT | SEX
bioChemists|>
  ggplot(aes(x = fem, y = art, fill=fem)) +
  geom_boxplot()+
  scale_fill_manual(values = c("#104E8B", "#66CD00")) +
  theme(legend.position = "none")+
  labs(y = "NUMBER OF ARTICLES PUBLISHED",
       x = "SEX")

#NUMBER OF ARTICLES PUBLISHED BY THE PHD STUDENT | MARITAL STATUS
bioChemists|>
  ggplot(aes(x = mar, y = art, fill=mar)) +
  geom_boxplot()+
  scale_fill_manual(values = c("darkcyan", "#FFD700")) +
  theme(legend.position = "none")+
  labs(y = "NUMBER OF ARTICLES PUBLISHED",
       x = "MARITAL STATUS")

#NUMBER OF ARTICLES PUBLISHED BY THE PHD STUDENT | NUMBER OF KIDS
bioChemists|>
  ggplot(aes(x = as.factor(kid5), y = art, fill=as.factor(kid5))) +
  geom_boxplot()+
  scale_fill_manual(values = palette[1:4]) +
  theme(legend.position = "none")+
  labs(y = "NUMBER OF ARTICLES PUBLISHED",
       x = "NUMBER OF KIDS")

#NUMBER OF ARTICLES PUBLISHED BY THE PHD STUDENT & PHD PRESTIGE
bioChemists|>
  ggplot(aes(x = phd, y = art)) +
  geom_point(col="darkcyan")

#NUMBER OF ARTICLES PUBLISHED BY THE PHD STUDENT & BY THE MENTOR | SEX
bioChemists |>
  ggplot(aes(x = ment, y = art, color = as.factor(fem))) +
  geom_point() +
  scale_color_manual(values = c("#104E8B", "#66CD00"), name="SEX")+
  labs(y = "NUMBER OF ARTICLES PUBLISHED BY THE STUDENT",
       x = "NUMBER OF ARTICLES PUBLISHED BY THE MENTOR")

#NUMBER OF ARTICLES PUBLISHED BY THE PHD STUDENT & BY THE MENTOR | MARITAL STATUS
bioChemists |>
  ggplot(aes(x = ment, y = art, color = as.factor(mar))) +
  geom_point() +
  scale_color_manual(values = c("darkcyan", "#FFD700"), name="MARITAL STATUS")+
  labs(y = "NUMBER OF ARTICLES PUBLISHED BY THE STUDENT",
       x = "NUMBER OF ARTICLES PUBLISHED BY THE MENTOR")


######################################
# DISTRIBUTIONAL ASSUMPTION VALIDITY #
######################################
#Distplot plots the number of occurrences (counts) against the distribution 
#metameter of the specified distribution. If the distribution fits the data, 
#the plot should show a straight line. In these plots, the open points show 
#the observed count metameters; the filled points show the confidence interval 
#centers, and the dashed lines show the conf_level confidence intervals for each point.

distplot(bioChemists$art, type="poisson")   #Poisson distribution
distplot(bioChemists$art, type="nbinomial") #NB distribution


###############
# POISSON GLM #
###############
pois_glm_s <- glm(art ~ (fem+mar+kid5+phd+ment)^2, 
                family = poisson (link = "log"),
                data = bioChemists)
summary(pois_glm_s)

(pois_glm<-stepAIC(pois_glm_s, direction = "backward"))
summary(pois_glm)
#AIC: 3307.6

pois_glm_opt <- glm(art ~ (fem+mar+kid5+phd+ment+fem:phd), 
                  family = poisson (link = "log"),
                  data = bioChemists)
summary(pois_glm_opt)
AIC(pois_glm_opt)
#AIC: 3310.5


#GOODNESS OF FIT
#For each individual, i.e. for each conditional distribution,
#given the conditional expectation predicted by the Poisson glm, 
#we compute the probability to observe each count value.
#The expected frequency is then computed.

pred_p<-predict.glm(object=pois_glm_opt, type="response")

observed_counts <- table(bioChemists$art)[1:11]

expected_counts_p<-vector()
for (i in 0:19){
  expected_counts_p[i+1]<-round(sum(dpois(x=i,lambda=pred_p)))
}
expected_counts_p <- expected_counts_p [1:11]

#Comparison between observed and predicted counts.
barplot(rbind(observed_counts, expected_counts_p), beside = TRUE, col = c("darkcyan", "#66CD00"),
        names.arg = 0:10, ylim = c(0, 300),
        xlab = "NUMBER OF ARTICLES PUBLISHED", ylab = "FREQUENCY",
        main = "OBSERVED vs EXPECTED COUNTS POISSON GLM",
        legend.text = c("Observed", "Expected"), args.legend = list(x = "topright"))

#TESTING FOR ZERO-INFLATION AND OVERDISPERSION
#This function compares the observed number of zeros with the zeros expected 
#from simulations.
(t<-testZeroInflation(pois_glm_opt))
t$statistic
# ratioObsSim 
# 1.429879

testOverdispersion(pois_glm_opt, type="PearsonChisq")
#dispersion = 1.81
#p-value < 2.2e-16

#It is necessary to consider a model able to handle overdispersion.


#####################
# NEGATIVE BINOMIAL #
#####################
nb_glm_s <- glm.nb(art ~ (fem+mar+kid5+phd+ment)^2, 
                 link = "log",
                 data = bioChemists)

(nb_glm<-stepAIC(nb_glm_s, direction = "backward"))
#AIC: 3131

summary(nb_glm)
#Theta:  2.321 <- overdispersion parameter

nb_glm_opt <- glm.nb(art ~ fem + mar + kid5 + phd + ment + kid5:phd , 
                   link = "log",
                   data = bioChemists)
summary(nb_glm_opt)
AIC(nb_glm_opt)
#AIC: 3132.571


#GOODNESS OF FIT
pred_nb<-predict(object=nb_glm_opt, type="response")

expected_counts_nb<-vector()
for (i in 0:19){
  expected_counts_nb[i+1]<-round(sum(dnbinom(x=i,mu=pred_nb, size=nb_glm_opt$theta)))
}
expected_counts_nb <- expected_counts_nb[1:11]

#Comparison between observed and predicted counts.
barplot(rbind(observed_counts, expected_counts_nb), beside = TRUE, col = c("darkcyan", "#66CD00"),
        names.arg = 0:10, ylim = c(0, 300),
        xlab = "NUMBER OF ARTICLES PUBLISHED", ylab = "FREQUENCY",
        main = "OBSERVED vs EXPECTED COUNTS NEGATIVE-BINOMIAL GLM",
        legend.text = c("Observed", "Expected"), args.legend = list(x = "topright"))

#TESTING FOR ZERO-INFLATION AND OVERDISPERSION
a<-testZeroInflation(nb_glm_opt)
a$statistic
a$p.value

testOverdispersion(nb_glm_opt, type="PearsonChisq")
#The model was able to handle the overdispersion problem.


###################################
# ZERO INFLATED NEGATIVE BINOMIAL #
###################################

m_nbin_s<-zeroinfl(art ~ (fem+ mar+ kid5+ phd+ ment+ kid5:phd + phd:ment) | (fem+ mar+ kid5+ phd+ ment+ kid5:phd + phd:ment) ,
                 dist = "negbin",
                 link = "logit",
                 data = bioChemists)

(m_nbin<-stepAIC(m_nbin_s))
#AIC: 3122.0

summary(m_nbin)

summary(zeroinfl(art ~ (fem+ mar+ kid5+ phd+ ment+ kid5:phd + phd:ment) | (fem+ mar+ kid5+ phd+ ment+ phd:ment) ,
         dist = "negbin",
         link = "logit",
         data = bioChemists))
#AIC 3121.533

summary(zeroinfl(art ~ (fem+ mar+ kid5+ phd+ ment+ kid5:phd + phd:ment) | (mar+ kid5+ phd+ ment+ phd:ment) ,
                 dist = "negbin",
                 link = "logit",
                 data = bioChemists))
#AIC 3119.569

summary(zeroinfl(art ~ (fem+ kid5+ phd+ ment+ kid5:phd + phd:ment) | (mar+ kid5+ phd+ ment+ phd:ment) ,
                 dist = "negbin",
                 link = "logit",
                 data = bioChemists))
#AIC 3118.494

#Optimal model
ZINB<- zeroinfl(art ~ (fem+ kid5+ phd+ ment+ kid5:phd) | (mar+ kid5+ phd+ ment+ phd:ment) ,
                   dist = "negbin",
                   link = "logit",
                   data = bioChemists)

vuong(ZINB, nb_glm_opt)
# Raw                    2.047196 model1 > model2 0.020319
# AIC-corrected          1.132949 model1 > model2 0.128618
# BIC-corrected         -1.069894 model2 > model1 0.142334
#It do not work better than the previous one. 


#GOODNESS OF FIT
pred_zinb<-predict(ZINB, type="prob")
expected_counts_zinb<-round(colSums(pred_zinb))
expected_counts_zinb <- expected_counts_zinb[1:11]

#Comparison between observed and predicted counts.
barplot(rbind(observed_counts, expected_counts_zinb), beside = TRUE, col = c("darkcyan", "#66CD00"),
        names.arg = 0:10, ylim = c(0, 300),
        xlab = "ARTICLE", ylab = "FREQUENCY",
        main = "OBSERVED vs EXPECTED COUNTS ZERO-INFLATED NEGATIVE-BINOMIAL",
        legend.text = c("Observed", "Expected"), args.legend = list(x = "topright"))
#It slightly overestimate the number of zeros.


############################
# HURDLE NEGATIVE BINOMIAL #
############################

hurdle_nb_s <- hurdle(art ~ (fem+mar+kid5+phd+ment+kid5:phd) | (fem+mar+kid5+phd+ment+kid5:phd), 
                     data = bioChemists, 
                     dist = "negbin",
                     link = "logit",
                     zero.dist = "binomial")

stepAIC(hurdle_nb_s)
#AIC 3129.9

summary(hurdle_nb_s)

summary(hurdle(art ~ (fem+mar+kid5+phd+ment) | (fem+mar+kid5+phd+ment), 
               data = bioChemists, 
               dist = "negbin",
               link = "logit",
               zero.dist = "binomial"))
AIC(hurdle(art ~ (fem+mar+kid5+phd+ment) | (fem+mar+kid5+phd+ment), 
               data = bioChemists, 
               dist = "negbin",
               link = "logit",
               zero.dist = "binomial"))
#AIC: 3131.193


summary(hurdle(art ~ (fem+mar+kid5+ment) | (fem+mar+kid5+ment), 
               data = bioChemists, 
               dist = "negbin",
               link = "logit",
               zero.dist = "binomial"))
AIC(hurdle(art ~ (fem+mar+kid5+ment) | (fem+mar+kid5+ment), 
           data = bioChemists, 
           dist = "negbin",
           link = "logit",
           zero.dist = "binomial"))
#AIC: 3127.275


summary(hurdle(art ~ (fem+kid5+ment) | (mar+kid5+ment), 
               data = bioChemists, 
               dist = "negbin",
               link = "logit",
               zero.dist = "binomial"))
AIC(hurdle(art ~ (fem+kid5+ment) | (mar+kid5+ment), 
           data = bioChemists, 
           dist = "negbin",
           link = "logit",
           zero.dist = "binomial"))
#AIC: 3126.712


hurdle_nb_opt <- hurdle(art ~ (fem+kid5+ment) | (mar+kid5+ment), 
                        data = bioChemists, 
                        dist = "negbin",
                        link = "logit",
                        zero.dist = "binomial")


#GOODNESS OF FIT
pred_hurdle_nb<-predict(hurdle_nb_opt, type="prob")
expected_counts_hurdle_nb<-round(colSums(pred_hurdle_nb))
expected_counts_hurdle_nb <- expected_counts_hurdle_nb[1:11]

#Comparison between observed and predicted counts.
barplot(rbind(observed_counts, expected_counts_hurdle_nb), beside = TRUE, col = c("darkcyan", "#66CD00"),
        names.arg = 0:10, ylim = c(0, 300),
        xlab = "ARTICLE", ylab = "FREQUENCY",
        main = "OBSERVED vs EXPECTED HURDLE NEGATIVE-BINOMIAL",
        legend.text = c("Observed", "Expected"), args.legend = list(x = "topright"))

#FINAL COMPARISONS
vuong(hurdle_nb_opt, ZINB)         #HURDLE vs ZINB
vuong(hurdle_nb_opt, nb_glm_opt)   #HURDLE vs NB GLM

