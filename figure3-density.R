### SCRIPT INFO ###############################################################
# Purpose:      Fit distribution to empirical data number of events
# Date created: 09 July 2020
# Last update:  09 July 2020

### 1. LOAD LIBRARIES AND DATA ################################################
library(data.table)
library(fitdistrplus)

# Data online available at:
# https://github.com/CommonEconomist/replication-material/tree/master/david-vs-goliath
d <- fread("fitted-values.csv") 
x <- log(d$N)

### 2. FIT DISTRIBUTION #######################################################
#lognormal
logn <- fitdist(x, "lnorm", method = "mme")
plot(logn)

#exponential
expo <- fitdist(x, "exp", method = "mme")
plot(expo)

#simulate data
logn_sim <- rlnorm(length(x), 
                   meanlog = logn$estimate[1],
                   sdlog = logn$estimate[2])

expo_sim <- rexp(length(x), rate = expo$estimate[1])

### 3. PLOT DATA ##############################################################
par(mar = c(5,5,2,2), bty = "n", las = 1, pty = "s", cex.lab=2)

#plot
plot(density(x), lwd = 2, xlim = c(0, 12), axes = FALSE, 
     xlab = "log(number of events)", ylab = "Density", main = "")
lines(density(logn_sim), lty = 3, lwd = 2)
lines(density(expo_sim), lty = 2, lwd = 2, col = "grey")

#axes
axis(1, tick = FALSE, cex.axis = 1.5)
axis(2, tick = FALSE, cex.axis = 1.5)

#legend
legend(10, .45,legend=c("Empirical","Log-normal", "Exponential"),
       col=c("black","black", "grey"),
       text.width=.4,lty=c(1,3, 2),bty="n",lwd=5,
       y.intersp=c(0.4),x.intersp=c(.3),xjust=1,cex=1.5)

###---FIN---###