#STAT 695 Project

library(ggplot2)
library(dplyr)

#further data manipulation
#getting pre christmas stuff
{
  
  preXM <- lapply(dir("prexmas", full.names = T), function(file){
    print(file)
    tryCatch({
      ans <- read.csv(file, stringsAsFactors=FALSE)
      subset(ans, etype=="shot")
    }, error=function(e){print(e); return(NULL)})
  })
  
  preXMa <- preXM[unlist(lapply(preXM, ncol))==32]
  pre.x <- do.call(rbind,preXMa)
  pre.x <- pre.x[!is.na(pre.x$x),]
  row.names(pre.x) <- NULL
  
  write.csv(pre.x, "nbaplayers_pre.x.csv")
  pre.x <- read.csv("nbaplayers_pre.x.csv")
  pre.x$points[is.na(pre.x$points)] <- 0
  
  pre.x2 <- pre.x
  pre.x2$result <- as.integer(as.character(pre.x2$result) == "made")
  pre.x2$n <- 1
  
  pre.x_fgs <- aggregate(result ~ player + team, data=pre.x2, FUN=sum)
  pre.x_n <- aggregate(n ~ player + team, data=pre.x2, FUN=sum)
  pre.x_fgs <- cbind(pre.x_fgs, n=pre.x_n$n)
  
  range(pre.x_fgs$result)
  
  pre.x_fgs.bnd <- pre.x_fgs[pre.x_fgs$n > 150,]
  table(pre.x_fgs.bnd$team)
  range(pre.x_fgs.bnd$result)
  
  #get games played for each team pre xmas
  teamgms.pre.x <- c(28, 27, 27, 27, 30, 29, 29, 29, 28, 29, 27, 28, 27, 28, 26, 27, 30, 29, 27, 28, 28, 29, 28, 29, 31, 28, 26, 31, 29, 27)
  teamnms.x <- unique(pre.x_fgs.bnd$team)
  
  pre.x_fgs.bnd$games_played <- sapply(pre.x_fgs.bnd$team, function(x){teamgms.pre.x[as.integer(x)]})
  
  #figuring out whether to use the logit or not
  p <- pre.x_fgs.bnd$result/pre.x_fgs.bnd$n
  pre.x_fgs.bnd$p <- p
  hist(p)
  hist(log(p/(1-p)))
  #p should be beta, but the logit should be normal, so it makes more sense to use the logit
  #at the end, we can transform pack to p with exp(y)/(1 + exp(y))
  pre.x_fgs.bnd$y <- log(p/(1-p))
  
  #to get the estimate of sigma for the logit (y), we need first the standard error for the sample proportion (p(1-p)/n) for each player.
  #then we transform; sigma = sqrt(p(1-p)/n) / (p(1-p))
  se_phat <- (p*(1-p))/pre.x_fgs.bnd$n
  smpl_sd <- se_phat/(p*(1-p))
  pre.x_fgs.bnd$smpl_sd <- smpl_sd
  
  #final pre-all star data set
  pre.x_dat <- pre.x_fgs.bnd
  names(pre.x_dat) <- c("player", "team", "p_fg", "p_fga", "gp", "p_p", "p_y", "p_sigma")
  
  #get teams field goals
  pre.x_tmfgs <- aggregate(result ~ team, data=pre.x2, FUN=sum)
  pre.x_tmn <- aggregate(n ~ team, data=pre.x2, FUN=sum)
  pre.x_tmfgs <- cbind(pre.x_tmfgs, n=pre.x_tmn$n)
  names(pre.x_tmfgs) <- c("team", "tm_fg", "tm_fga")
  pre.x_tmfgs$gp <- teamgms.pre.x
  tm.p <- pre.x_tmfgs$tm_fg/pre.x_tmfgs$tm_fga
  pre.x_tmfgs$tm_p <- tm.p
  pre.x_tmfgs$tm_y <- log(tm.p/(1-tm.p))
  se_tmphat <- (tm.p*(1-tm.p))/pre.x_tmfgs$tm_fga
  smpl_tmsd <- se_tmphat/(tm.p*(1-tm.p))
  pre.x_tmfgs$tm_rho <- smpl_tmsd
  
  
  # Get Post christmas Data
  postXM <- lapply(dir("postxmas", full.names = T), function(file){
    print(file)
    tryCatch({
      ans <- read.csv(file, stringsAsFactors=FALSE)
      subset(ans, etype=="shot")
    }, error=function(e){print(e); return(NULL)})
  })
  
  postXMa <- postXM[unlist(lapply(postXM, ncol))==32]
  post.x <- do.call(rbind,postXMa)
  post.x <- post.x[!is.na(post.x$x),]
  row.names(post.x) <- NULL
  
  write.csv(post.x, "nbaplayers_post.x.csv")
  post.x <- read.csv("nbaplayers_post.x.csv")
  
  post.x2 <- post.x
  post.x2$result <- as.integer(as.character(post.x2$result) == "made")
  post.x2$n <- 1
  
  post.x_fgs <- aggregate(result ~ player + team, data=post.x2, FUN=sum)
  post.x_n <- aggregate(n ~ player + team, data=post.x2, FUN=sum)
  post.x_fgs <- cbind(post.x_fgs, n=post.x_n$n)
  
  #get remaining games played for each team
  teamgms.post.x <- 82 - teamgms.pre.x
  
  #get teams post field goals
  post.x_tmfgs <- aggregate(result ~ team, data=post.x2, FUN=sum)
  post.x_tmn <- aggregate(n ~ team, data=post.x2, FUN=sum)
  post.x_tmfgs <- cbind(post.x_tmfgs, n=post.x_tmn$n)
  names(post.x_tmfgs) <- c("team", "tm_fg", "tm_fga")
  post.x_tmfgs$gp <- teamgms.post.x
  tm.p2 <- post.x_tmfgs$tm_fg/post.x_tmfgs$tm_fga
  post.x_tmfgs$tm_p <- tm.p2
  post.x_tmfgs$tm_y <- log(tm.p2/(1-tm.p2))
  se_tmphat2 <- (tm.p2*(1-tm.p2))/post.x_tmfgs$tm_fga
  post.smpl_tmsd <- se_tmphat2/(tm.p2*(1-tm.p2))
  post.x_tmfgs$tm_rho <- post.smpl_tmsd
  
  #finishing up the players
  post.x_fgs$gp_remaining <- sapply(post.x_fgs$team, function(x){teamgms.post.x[as.integer(x)]})
  post.x_fgs$p_p_remaining <- post.x_fgs$result/post.x_fgs$n
  post.x_fgs$p_y_remaining <- log(post.x_fgs$p_p_remaining/(1-post.x_fgs$p_p_remaining))
  
  #final post-xmas data set
  post.x_dat <- post.x_fgs
  names(post.x_dat) <- c("player", "team", "p_fg_remaining", "p_fga_remaining", "gp_remaining", "p_p_remaining", "p_y_remaining")
  
  #putting the post-christmas data onto the pre
  #Greg Oden didn't play after Christmas, so we must remove him from the pre_data
  player_data <- pre.x_dat
  row.names(player_data) <- NULL
  player_data <- player_data[-which(as.character(player_data$player)=="Greg Oden"),]
  
  for(i in 1:length(player_data$player)){
    player_data$p_fga_remaining[i] <- post.x_dat$p_fga_remaining[which(as.character(post.x_dat$player)==as.character(player_data$player[i]))]
    player_data$gp_remaining[i] <- post.x_dat$gp_remaining[which(as.character(post.x_dat$player)==as.character(player_data$player[i]))]
    player_data$p_p_remaining[i] <- post.x_dat$p_p_remaining[which(as.character(post.x_dat$player)==as.character(player_data$player[i]))]
    player_data$p_y_remaining[i] <- post.x_dat$p_y_remaining[which(as.character(post.x_dat$player)==as.character(player_data$player[i]))]
  }
  
  row.names(player_data) <- NULL
  
}

#1. use MH to sample team variances and player variances jointly
#2. use sampled team variances to sample team means
#3. use team means/variances to sample posterior team means
#4. use posterior team means and sampled player variances to sample player means

#data set up, assuming players used play in all team games
n_k <- player_data$gp
y_j.k <- data.frame(team = player_data$team, y_j.k = player_data$p_y)
y_..k <- pre.x_tmfgs$tm_y
sigma_j.k_sq <- data.frame(team = player_data$team, sigma_j.k_sq = (player_data$p_sigma^2))
K <- 30
J <- length(y_j.k$y_j.k)

#1.

#joint log likelihood for team and player variances (tau^2 and nu^2)
#there will be one tau and thirty nu's

ll_tau_nu_sq <- function(tau_sq, nu_sq){
  
  ll_joint <- 0
  
  for(kk in 1:K){
    
    psi_hat <- sum(y_j.k$y_j.k[as.numeric(y_j.k$team) == kk] / (tau_sq + nu_sq[kk] + sigma_j.k_sq$sigma_j.k_sq[as.numeric(sigma_j.k_sq$team) == kk]))
    
    term1 <- -0.5 * sum(log(tau_sq + nu_sq[kk] + sigma_j.k_sq$sigma_j.k_sq[as.numeric(sigma_j.k_sq$team) == kk])) - 0.5 * log(sum(1 / (tau_sq + nu_sq[kk] + sigma_j.k_sq$sigma_j.k_sq[as.numeric(sigma_j.k_sq$team) == kk])))
    term2 <- -0.5 * sum((y_j.k$y_j.k[as.numeric(y_j.k$team) == kk] - psi_hat)^2 / (tau_sq + nu_sq[kk] + sigma_j.k_sq$sigma_j.k_sq[as.numeric(sigma_j.k_sq$team) == kk]))
    
    ll_joint.tmp <- term1 + term2
    
    ll_joint <- ll_joint + ll_joint.tmp
  }
  
  return(ll_joint)
  
}

#log posterior for tau and nu under joint prior p(tau_sq, nu_sq) \propto 1/sqrt(tau_sq + nu_sq)

lpost_tau_nu <- function(tau, nu){
  
  #scl_sq <- 25
  tau_sq <- tau^2
  nu_sq <- nu^2
  ljprior <- 0
  
  for(kk in 1:K){
    
    #lprior <- -log(1 + (tau_sq + nu_sq[kk]) / scl_sq)
    lprior <- -log(sqrt(tau_sq + nu_sq[kk]))
    ljprior <- ljprior + lprior
    
  }
  
  return(ll_tau_nu_sq(tau_sq, nu_sq) + ljprior)
  
}

#Metropolis Hastings for the team and player variances

mh_tau_nu <- function(init = rep(1, (K + 1)), iter = 100000, prop_sd = .1){
  
  #run Metropolis Hastings to sample tau
  var_smpls <- matrix(NA, nrow = iter, ncol = (K + 1))
  var_smpls[1,] <- init
  lpost_curr <- lpost_tau_nu(tau = var_smpls[1,1], nu = var_smpls[1,2:(K + 1)])
  acc <- 0
  
  for(ii in 2:iter){
    
    var_smpls[ii,] <- var_smpls[ii-1,]
    var_new <- var_smpls[ii-1,] + prop_sd * rnorm(K + 1)
    lpost_new <- lpost_tau_nu(tau = abs(var_new[1]), nu = abs(var_new[2:(K + 1)]))
    
    if(log(runif(1)) < (lpost_new - lpost_curr)){
      
      lpost_curr <- lpost_new
      var_smpls[ii,] <- var_new
      acc <- acc + 1
      
    }
    
  }
  
  return(list(smpls = abs(var_smpls), acc = acc/iter))
  
}

mh_tau_nu_smpls <- mh_tau_nu()

for(i in 1:(K+1)){
  plot(mh_tau_nu_smpls$smpls[,i], main = ifelse(i==1, "tau", paste("nu", i-1)), type = "l")
}

#2-3.

#get posterior of team means
#will use a 10% burn-in

mh_psi <- function(ndraws = 10^4){
  
  post_draws_tau <- sample(mh_tau_nu_smpls$smpls[-c(1:10000),1], ndraws, replace = TRUE)
  post_draws_nu <- matrix(NA, nrow = ndraws, ncol = K)
  
  for(i in 1:K){
    
    post_draws_nu[,i] <- sample(mh_tau_nu_smpls$smpls[-c(1:10000),(i+1)], ndraws, replace = TRUE)
    
  }
  
  post_draws_mu <- rep(NA, ndraws)
  post_draws_psi <- matrix(NA, nrow = ndraws, ncol = K)
  
  for(ii in 1:ndraws){
    
    post_draws_mu[ii] <- sum((post_draws_tau[ii]^2 + post_draws_nu[ii,]^2)^(-1) * y_..k) / sum((post_draws_tau[ii]^2 + post_draws_nu[ii,]^2)^(-1)) + sqrt(1 / sum(1 / (post_draws_tau[ii]^2 + post_draws_nu[ii,]^2))) * rnorm(1)
    post_draws_psi[ii,] <- (post_draws_tau[ii]^(-2) * post_draws_mu[ii] + post_draws_nu[ii,]^(-2) * y_..k) / (post_draws_tau[ii]^(-2) + post_draws_nu[ii,]^(-2)) + sqrt(1 / (post_draws_tau[ii]^(-2) + post_draws_nu[ii,]^(-2))) * rnorm(length(post_draws_psi[ii,]))
    
  }
  
  post_means_psi <- colMeans(post_draws_psi)
  return(list(tau = post_draws_tau, nu = post_draws_nu, mu = post_draws_mu, psi = post_draws_psi))
  
}

mh_smpls <- mh_psi()

#team shrinkage in terms of logit

plot_team_shrinkage <- function(post_draws_psi) {
  post_means_psi <- colMeans(post_draws_psi)
  
  plot(post_means_psi, rep(0,K), 
       main="Shrinkage Plot for Sample Team logit(Field Goal Pct.)", 
       ylim=c(0,15), 
       xlim=c(min(c(post_means_psi, y_..k)), max(c(post_means_psi, y_..k))), 
       ylab="", xlab="logit(Field Goal Pct.)", yaxt="n")
  axis(at=c(0,10), labels=c("Posterior", "Sample"), side=2, tick=FALSE)
  points(y_..k, rep(10,K))
  points((sum(teamgms.pre.x*y_..k)/sum(teamgms.pre.x)), 0, pch=3, lwd=5)
  for(i in 1:K)
  {
    segments(post_means_psi[i], 0, (y_..k[i]), 10)
    segments(y_..k[i],10,y_..k[i],(10 + 0.3*(colMeans(mh_tau_nu_smpls$smpls)[(i+1)])^0.5), lwd=3)
  }
}

plot_team_shrinkage(mh_smpls$psi)

#team shrinkage in terms of field goal percentage, should look the same as above just on a different scale

plot_team_shrinkage2 <- function(post_draws_psi) {
  post_means_psi <- exp(colMeans(post_draws_psi))/(1 + exp(colMeans(post_draws_psi)))
  
  plot(post_means_psi, rep(0,K), 
       main="Shrinkage Plot for Sample Team Field Goal Pct.", 
       ylim=c(0,15), 
       xlim=c(min(c(post_means_psi, exp(y_..k)/(1 + exp(y_..k)))), max(c(post_means_psi, exp(y_..k)/(1 + exp(y_..k))))), 
       ylab="", xlab="Field Goal Pct.", yaxt="n")
  axis(at=c(0,10), labels=c("Posterior", "Sample"), side=2, tick=FALSE)
  points(exp(y_..k)/(1 + exp(y_..k)), rep(10,K))
  points((sum(teamgms.pre.x*exp(y_..k)/(1 + exp(y_..k)))/sum(teamgms.pre.x)), 0, pch=3, lwd=5)
  for(i in 1:K)
  {
    segments(post_means_psi[i], 0, ((exp(y_..k)/(1 + exp(y_..k)))[i]), 10)
    segments((exp(y_..k)/(1 + exp(y_..k)))[i],10,(exp(y_..k)/(1 + exp(y_..k)))[i],(10 + 0.3*(colMeans(mh_tau_nu_smpls$smpls)[(i+1)])^0.5), lwd=3)
  }
}

plot_team_shrinkage2(mh_smpls$psi)

#checking performance of model at predicting team means
{
  
  post_means_psi <- colMeans(mh_smpls$psi)
  true_means_psi <- post.x_tmfgs$tm_y
  sum((post_means_psi - true_means_psi)^2)/30
  samp_means_psi <- pre.x_tmfgs$tm_y
  sum((samp_means_psi - true_means_psi)^2)/30
  
  #actual shrinkage plot (i.e. what it should look like with perfect recovery)
  true_means_psi <- exp(true_means_psi)/(1 + exp(true_means_psi))
  plot(true_means_psi, rep(0,K), 
       main="Shrinkage Plot for Observed Team Field Goal Pct.", 
       ylim=c(0,15), 
       xlim=c(min(c(true_means_psi, exp(y_..k)/(1 + exp(y_..k)))), max(c(true_means_psi, exp(y_..k)/(1 + exp(y_..k))))), 
       ylab="", xlab="Field Goal Pct.", yaxt="n")
  axis(at=c(0,10), labels=c("Observed", "Sample"), side=2, tick=FALSE)
  points(exp(y_..k)/(1 + exp(y_..k)), rep(10,K))
  points((sum(teamgms.pre.x*exp(y_..k)/(1 + exp(y_..k)))/sum(teamgms.pre.x)), 0, pch=3, lwd=5)
  for(i in 1:K)
  {
    segments(true_means_psi[i], 0, ((exp(y_..k)/(1 + exp(y_..k)))[i]), 10)
    segments((exp(y_..k)/(1 + exp(y_..k)))[i],10,(exp(y_..k)/(1 + exp(y_..k)))[i],(10 + 0.3*(colMeans(mh_tau_nu_smpls$smpls)[(i+1)])^0.5), lwd=3)
  }
  
}

#4.

#get posterior player means given team and player variances nested within team means

mh_theta <- function(ndraws = 10^4){
  
  post_draws_tau <- rep(NA, ndraws)
  post_draws_tau <- sample(mh_tau_nu_smpls$smpls[-c(1:10000),1], ndraws, replace = TRUE)
  post_draws_nu <- matrix(NA, nrow = ndraws, ncol = K)
  
  for(i in 1:K){
    
    post_draws_nu[,i] <- sample(mh_tau_nu_smpls$smpls[-c(1:10000),(i+1)], ndraws, replace = TRUE)
    
  }
  
  post_draws_psi <- matrix(NA, nrow = ndraws, ncol = K)
  
  for(i in 1:K){
    
    post_draws_psi[,i] <- sample(mh_smpls$psi[,i], ndraws, replace = TRUE)
    
  }
  
  post_draws_theta <- matrix(NA, nrow = ndraws, ncol = 0)
  
  for(kk in 1:K){
    
    post_draws_nu.k <- post_draws_nu[,kk]
    post_draws_psi.k <- post_draws_psi[,kk]
    post_draws_theta.k <- matrix(NA, nrow = ndraws, ncol = length(y_j.k$y_j.k[as.numeric(y_j.k$team) == kk]))
    
    for(ii in 1:ndraws){
      
      post_draws_theta.k[ii,] <- ((post_draws_tau[ii]^2 + post_draws_nu.k[ii]^2)^(-1) * post_draws_psi.k[ii] + sigma_j.k_sq$sigma_j.k_sq[as.numeric(sigma_j.k_sq$team) == kk]^(-1) * y_j.k$y_j.k[as.numeric(y_j.k$team) == kk]) / ((post_draws_tau[ii]^2 + post_draws_nu.k[ii]^2)^(-1) + sigma_j.k_sq$sigma_j.k_sq[as.numeric(sigma_j.k_sq$team) == kk]^(-1)) + sqrt(1 / ((post_draws_tau[ii]^2 + post_draws_nu.k[ii]^2)^(-1) + sigma_j.k_sq$sigma_j.k_sq[as.numeric(sigma_j.k_sq$team) == kk]^(-1))) * rnorm(length(post_draws_theta.k[ii,]))
      
    }
    
    post_draws_theta <- cbind(post_draws_theta, post_draws_theta.k)
    
  }
  
  return(list(nu = post_draws_nu, psi = post_draws_psi, theta = post_draws_theta))
  
}

mh_player_smpls <- mh_theta()

#player logit field goal pct. shrinkage

plot_player_shrinkage <- function(post_draws_theta) {
  post_means_theta <- exp(colMeans(post_draws_theta)) / (1 + exp(colMeans(post_draws_theta)))
  obs_p <- exp(y_j.k$y_j.k)/(1 + exp(y_j.k$y_j.k))
  
  plot(post_means_theta, rep(0,J), 
       main="Shrinkage Plot for Player Field Goal Pct.", 
       ylim=c(0,15),
       xlim=c(min(c(post_means_theta, obs_p)), max(c(post_means_theta, obs_p))), 
       ylab="", xlab="Field Goal Pct.", yaxt="n")
  axis(at=c(0,10), labels=c("Posterior", "Sample"), side=2, tick=FALSE)
  points(obs_p, rep(10,J))
  points((sum(n_k*obs_p)/sum(n_k)), 0, pch=3, lwd=5)
  for(i in 1:J)
  {
    segments(post_means_theta[i], 0, (obs_p[i]), 10)
    segments(obs_p[i],10,obs_p[i],(10 + 0.3*(sigma_j.k_sq$sigma_j.k_sq[i])^0.5), lwd=3)
  }
}

plot_player_shrinkage(mh_player_smpls$theta)

#true player shrinkage plot (i.e. what it should look like with perfect recovery)
true_means_theta <- exp(player_data$p_y_remaining)/(1 + exp(player_data$p_y_remaining))
plot(true_means_theta, rep(0,J), 
     main="Shrinkage Plot for Observed Player Field Goal Pct.", 
     ylim=c(0,15), 
     xlim=c(min(c(true_means_theta, exp(y_j.k$y_j.k)/(1 + exp(y_j.k$y_j.k)))), max(c(true_means_theta, exp(y_j.k$y_j.k)/(1 + exp(y_j.k$y_j.k))))), 
     ylab="", xlab="Field Goal Pct.", yaxt="n")
axis(at=c(0,10), labels=c("Observed", "Sample"), side=2, tick=FALSE)
points(exp(y_j.k$y_j.k)/(1 + exp(y_j.k$y_j.k)), rep(10,J))
points((sum(n_k*exp(y_j.k$y_j.k)/(1 + exp(y_j.k$y_j.k)))/sum(n_k)), 0, pch=3, lwd=5)
for(i in 1:J)
{
  segments(true_means_theta[i], 0, ((exp(y_j.k$y_j.k)/(1 + exp(y_j.k$y_j.k)))[i]), 10)
  segments((exp(y_j.k$y_j.k)/(1 + exp(y_j.k$y_j.k)))[i],10,(exp(y_j.k$y_j.k)/(1 + exp(y_j.k$y_j.k)))[i],(10 + 0.3*(sigma_j.k_sq$sigma_j.k_sq[i])^0.5), lwd=3)
}

#look at performance
sum((y_j.k$y_j.k-player_data$p_y_remaining)^2)/192
sum((colMeans(mh_player_smpls$theta)-player_data$p_y_remaining)^2)/192
