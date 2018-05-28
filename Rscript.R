library(tidyverse)
sc2017 <- read_csv("data/sc2017ip.csv")

sc2017 %>% filter(type == "X") %>% 
  mutate(HIT = ifelse(events %in%
             c("single", "double", "triple", "home_run"),
                      1, 0),
         phi = ifelse(stand == "L", 
               - plate_x, plate_x)) -> sc2017_ip

# only look at hitters with at least 200 balls in play

sc2017_ip %>% group_by(batter) %>%
  summarize(N = n()) -> S
inner_join(sc2017_ip, S, by="batter") %>%
  filter(N >= 200) -> sc_regular

# for each player -- plot speed against distance
# from center

sc_regular %>% 
  mutate(dist = sqrt((phi - 0.4) ^ 2 +
                       (plate_z - 2.9) ^ 2)) -> 
  sc_regular

# have 286 players of this type -- some exploratory work

# many regressions

library(broom)
regressions <- sc_regular %>%
  group_by(batter) %>%
  do(tidy(lm(launch_speed ~ dist,
             data=.)))

spread(select(regressions, batter,
              term, estimate),
       term, estimate) -> Individual_est
names(Individual_est) <- c("Batter", "beta0", "beta1")

ggplot(Individual_est, aes(beta0, beta1)) +
  geom_point()

############################

####### JAGS work

# data work -- work with a sample of players

Batters <- unique(sc_regular$batter)
set.seed(693)
N_batters <- 50  # here we have 50 players
Batters_s <- sample(Batters, size=N_batters, replace=TRUE)
sc_sample <- filter(sc_regular, 
                    batter %in% Batters_s)

sc_sample <- filter(sc_sample,
                    is.na(dist) == FALSE)
J <- length(Batters_s)
N <- dim(sc_sample)[1]

# here we are creating data for JAGS run

j <- rep(0, N)
for(k in 1:J){
  ind <- sc_sample$batter == Batters_s[k]
  j[ind] <- rep(k, sum(ind))
}
y <- sc_sample$launch_speed
x <- sc_sample$dist

mean <- c(0, 0)
Omega <- diag(c(.1, .1))
prec <- diag(c(1.0E-6, 1.0E-6))

# start JAGS

library(rjags)

modelString = "
model {
for(i in 1:N){
mu.y[i] <- alpha[j[i]]  + beta[j[i]] * x [i]
y[i] ~ dnorm(mu.y[i], tau)
}
for (j in 1:J){
alpha[j] <- B[j, 1]
beta[j] <- B[j, 2]
B[j, 1:2] ~ dmnorm(mu.beta[], Tau.B[,])
}
tau ~ dgamma(0.001, 0.001)
mu.beta[1:2] ~ dmnorm(mean[1:2],prec[1:2 ,1:2])
Tau.B[1:2 , 1:2] ~ dwish(Omega[1:2 ,1:2 ], 2)
}
"
writeLines(modelString, con="normalreffmodel.bug")

jags <- jags.model('normalreffmodel.bug',
                   data = list('y' = y, "j" = j, 
                               'x' = x,
                               "N" = N, "J" = J,
                               "mean" = mean,
                               "prec" = prec,
                               "Omega" = Omega),
                   n.chains = 1,
                   n.adapt = 100)

update(jags, 5000)

posterior <- coda.samples(jags,
                          c("alpha", "beta", "tau",
                            "mu.beta", "Tau.B"),
                          n.iter=10000,
                          progress.bar="gui")

S <- summary(posterior)

Estimates <- data.frame(Batter = Batters_s,
             A_bayes = S$statistics[4 + 1:N_batters, 1],
             B_bayes = S$statistics[4 + (N_batters + 1):
                               (2 * N_batters), 1])

dall <- merge(Estimates, Individual_est, by="Batter")

bb_data <- sc_sample %>% group_by(batter) %>% 
  summarize(N = n())

dall <- inner_join(dall, bb_data, 
            by=c("Batter" = "batter"))

ggplot(dall, aes(beta0, beta1)) +
  geom_point() +
  geom_point(aes(A_bayes, B_bayes), color="red")

ggplot(dall, aes(N, beta0)) + geom_point() +
  geom_point(aes(N, A_bayes), color = "red")

# some diagnostics ...

library(bayesplot)
mcmc_areas(posterior,
           pars= c("sigma[2]", "sigma[3]"))
mcmc_intervals(posterior,
               regex_pars = c("beta\\[[1-2]"))
mcmc_trace(posterior, pars = "Tau.B[1,1]")
mcmc_trace(posterior, pars = "sigma[2]")
mcmc_trace(posterior, pars = "sigma[1]")
mcmc_trace(posterior, pars = c("mu.alpha", "mu.beta"))

