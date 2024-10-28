#HIERARCHICAL BAYESIAN MODELLING WITH HEALTH COUNT DATA: 
#Malignant Melanoma Mortality in the European Community

#AIMS OF THE ANALYSIS:
# - understand a (super)population rate level of mortality;
# - understand which nations need some intervention policies to reduce the mortality risk of malignant melanoma;
# - (possibly) investigate the impact of UV radiation exposure on the mortality.

##########################################
# PREPROCESSING AND EXPLORATORY ANALYSIS #
##########################################

setwd("D:/Desktop/HOMEWORK 3 BAYES")
data<-read.table("Homework_3_b_mmmec.txt", col.names = c("NATION", "REGION", "COUNTY", "DEATHS", "EXPECTED_DEATHS", "1", "UVB"))
str(data)

#The dataset is made up of 7 variables:
# 1. Nation
#   1 = Belgium
#   2 = W. Germany
#   3 = Denmark
#   4 = France
#   5 = UK
#   6 = Italy
#   7 = Ireland
#   8 = Luxembourg
#   9 = Netherlands
# 2. Region ID
# 3. County ID
# 4. Number of male deaths due to MM during 1971-1980
# 5. Number of expected deaths
# 6. Constant = 1
# 7. Measure of the UVB dose reaching the earth's surface in each county and centered

library(dplyr)
library(ggplot2)

data <- data |>
  select(-X1) |>
  mutate_at(vars(NATION, REGION, COUNTY), as.factor) 

data$NATION<-factor(data$NATION, levels = levels(data$NATION), 
                    labels = c("BELGIUM", "W. GERMANY", "DENMARK", "FRANCE", "UK",
                               "ITALY", "IRELAND", "LUXEMBOURG", "NETHERLANDS"))

#DISTRIBUTION OF DEATHS IN EACH COUNTY | NATION
library(viridis)
data |>
  ggplot(aes(x = NATION, y = DEATHS, fill=NATION)) +
  geom_boxplot() +
  facet_wrap(~NATION, scales = "free") +
  scale_fill_viridis(discrete=T) +
  theme(legend.position = "none") +
  theme(strip.text = element_blank()) +
  xlab(" ")+ylab(" ")

quantile(data[data$NATION=="ITALY", "DEATHS"])

#Since we are interested in exploring the european and the national level
#we can do an exploratory analysis before aggregating data.

(summary <- data |>
  group_by(NATION) |>
  summarise(OBSERVED = sum(DEATHS),
            var_NATIONAL_DEATHS = var(DEATHS),
            EXPECTED = sum(EXPECTED_DEATHS),
            var_NATIONAL_EDEATHS = var(EXPECTED_DEATHS)))

#DEATHS
library(reshape2)
summary[, c(1,2,4)] |>
  melt(id.vars="NATION") |>
  ggplot(aes(x=variable, y=value, fill=factor(variable)))+
  geom_bar(stat="identity", position="dodge")+ 
  scale_fill_manual(values = c("#66CD00", "darkcyan"), name = "DEATHS") +
  facet_wrap(~NATION, scales = "free_y") +
  xlab(" ")+ylab(" ") +
  geom_text(aes(label = round(value)), position = position_dodge(width = 0.8), vjust = -0.25)


#UVB
library(viridis)
data |>
  ggplot(aes(x = NATION, y = UVB, fill=NATION)) +
  geom_boxplot() +
  facet_wrap(~NATION, scales = "free") +
  scale_fill_viridis(discrete=T) +
  theme(legend.position = "none") +
  theme(strip.text = element_blank()) +
  xlab(" ")+ylab(" ")

data_nations <- data |>
  group_by(NATION) |>
  summarise(OBSERVED_DEATHS = sum(DEATHS),
            EXPECTED_DEATHS = sum(EXPECTED_DEATHS),
            UVB_AVG = mean(UVB))
  

###############################
# HIERARCHICAL BAYESIAN MODEL #
###############################

library("bayesplot")
library("rstan") 
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#library("rstanarm")

#We want to take into account the hierarchy that naturally exists in out data.
#We add another level of variability.
#The model assumes that the Poisson mean follows a gamma distribution, and the gamma  
#distribution has hyperparameters that are also modeled as gamma distributions.

#Yj|λj~Pois(λj)    <- SAMPLING DISTRIBUTION of the number of deaths in the jth nation
#λj|ab~Gamma(a, b) <- PRIOR DISTRIBUTION OF THE EXPECTED number of deaths in the jth nation
#a~Gamma(A, B)     <- HYPERPRIOR OF THE HYPERPARAMETER a
#b~Gamma(C, D)     <- HYPERPRIOR OF THE HYPERPARAMETER b

#λj   <- PARAMETER 
#a, b <- HYPERPARAMETERS

#Hyperparameter values:
#In practice, the hyperparameters are often chosen based on prior knowledge 
#or empirical evidence.
#by fixing the moments of the gamma distribution that generates each λj
#it is possible to find the values of a and b.
# |E[λj|ab]   = a/b 
# |Var[λj|ab] = a/b2

(lambda_mu<-mean(data_nations$OBSERVED_DEATHS)) #average number of deaths in each country
(lambda_var<-var(data_nations$OBSERVED_DEATHS))  #variability in the number of deaths across each country

#solve(matrix(c(1, -lambda_mu, 1, -lambda_var), nrow = 2), c(0, 0))

(b=lambda_mu/lambda_var)
(a=b*lambda_mu)
#λj|ab~Gamma(a=1.198, b=0.001)

# |E[b] =~ 0.001
# |E[a] =~ 1.198
a_var<-50
b_var<-0.0025

(B=a/a_var)
(A=B*a)

(D=b/b_var)
(C=D*b)

values <- list(N=nrow(data_nations), 
               y = data_nations$OBSERVED_DEATHS, 
               A = A,    # a hyperprior
               B = B,
               C = C,    # b hyperprior
               D = D)


#Stan using NUTS 
HierarchicalGammaPoisson <- " 
data {
  int<lower=0> N;
  int<lower=0> y[N];
  real<lower=0> A;    
  real<lower=0> B; 
  real<lower=0> C;
  real<lower=0> D;
}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0> lambda[N];
}
model {
alpha ~ gamma(A, B);
beta ~ gamma(C, D);
// implicit joint distributions 
lambda ~ gamma(alpha, beta);
y ~ poisson(lambda);
}
"
fit <- stan(model_code = HierarchicalGammaPoisson, 
            data = values,
            iter = 5000, chains = 1, 
            cores = 6, 
            thin = 4)

#Updated lambda 
cbind(data_nations$OBSERVED_DEATHS,
  data_nations$EXPECTED_DEATHS,
  round(get_posterior_mean(fit), 2)[3:11,])

#Updated lambda 
hx<-cbind(round(data_nations$EXPECTED_DEATHS,2),
          round(get_posterior_mean(fit), 2)[3:11,])
rownames(hx)<-c("BELGIUM", "W. GERMANY", "DENMARK", "FRANCE", "UK",
                "ITALY", "IRELAND", "LUXEMBOURG", "NETHERLANDS")
colnames(hx)<-c("Prior EV","Posterior EV")
hx
hx2<-as.data.frame(hx)
hx2




plot(fit, par = c( "beta"))
plot(fit, par = c("alpha"))
plot(fit,par="lambda")
plot(fit,par=c("lambda[2]","lambda[4]","lambda[5]","lambda[6]"))
plot(fit,par=c("lambda[1]","lambda[3]","lambda[9]"))
plot(fit,par=c("lambda[7]","lambda[8]"))


#distribuzioni dei lambda posterior
dev.off()
par(mfrow=c(1,3))
plot(density(m1[,3]),main="BELGIUM",col="blue")
abline(v=hx2[1,2],col="blue")
abline(v=hx2[1,1],col="red")
legend()
plot(density(m1[,4]),main="WEST GERMANY",xlim=c(1700,3100),col="blue")
abline(v=hx2[2,2],col="blue")
abline(v=hx2[2,1],col="red")
plot(density(m1[,5]),main="DENMARK",xlim=c(300,780),col="blue")
abline(v=hx2[3,2],col="blue")
abline(v=hx2[3,1],col="red")

plot(density(m1[,6]),main="FRANCE",xlim=c(1300,2800),col="blue")
abline(v=hx2[4,2],col="blue")
abline(v=hx2[4,1],col="red")
plot(density(m1[,7]),main="UK",col="blue")
abline(v=hx2[5,2],col="blue")
abline(v=hx2[5,1],col="red")
plot(density(m1[,8]),main="ITALY",xlim=c(1300,1900),col="blue")
abline(v=hx2[6,2],col="blue")
abline(v=hx2[6,1],col="red")

plot(density(m1[,9]),main="IRELAND",xlim=c(40,130),col="blue")
abline(v=hx2[7,2],col="blue")
abline(v=hx2[7,1],col="red")
plot(density(m1[,10]),main="LUXEMBOURG",col="blue")
abline(v=hx2[8,2],col="blue")
abline(v=hx2[8,1],col="red")
plot(density(m1[,11]),main="NETHERLANDS",xlim=c(400,600),col="blue")
abline(v=hx2[9,2],col="blue")
abline(v=hx2[9,1],col="red")
legend("topright", legend = c("Posterior Lambda","Prior Lambda (Expected Value)"), lty=7,col = c("blue","red"))

#nuova osservazione per il decennio successivo in Italia
set.seed(1234)
rpois(1,get_posterior_mean(fit)[6,])

#nuova osservazione per una nuova nazione
m1<-as.matrix(fit)

set.seed(1234)
(alpha1<-sample(x = m1[,1],size=1 ))
(beta1<-sample(x = m1[,2],size=1 ))

(lambda1<-rgamma(n=1,shape=alpha1,rate=beta1))

(y1<-rpois(1,lambda1))




acf(m1[,1])

samples = extract(fit, c("alpha","beta"))
library(ggplot2)
library(GGally)
ggpairs(data.frame(log_alpha = (samples$alpha), 
                   log_beta = (samples$beta)),
        lower = list(continuous='density')) + theme_bw()
acf(m)

library(rstan)
stan_plot()







