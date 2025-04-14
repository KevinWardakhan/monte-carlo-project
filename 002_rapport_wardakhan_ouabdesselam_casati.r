rm(list=ls())
library(truncnorm)  # Load the truncnorm package, which we will need for question 9
seed <- 12345
set.seed(seed)


#######################################Question 3############################################
#pdf of f
f <- function(a, mu_1, mu_2, s_1, s_2, x) {
  return ((1/(1-a))*(dnorm(x, mu_1, s_1) -a*dnorm(x, mu_2, s_2)))
}

#We will plot the pdf of f for different values of a
mu_1 <-0
mu_2 <-0
s_1 <-4
s_2 <-1

#We computed a^*, we get:
a_star<- (s_2/s_1)*exp(-((mu_1-mu_2)^2)/(2*(s_1^2-s_2^2)))

#plot of f for different values of a
#The dotted curve corresponds to a=a_star

a_values <- seq(0.1, a_star, length.out = 10)
colors_a <- rainbow(length(a_values))
x <- seq(-10, 10, length.out = 1000)

plot(x, f(a_values[1], mu_1, mu_2, 3, s_2, x), type = "l", col = colors_a[1], xlab = "x", ylab = "f(x)", main = "Plot of f for different values of a")

for (i in 2:length(a_values)) {
  if(a_values[i]==a_star){
    lines(x, f(a_values[i], mu_1, mu_2, 3, s_2, x), col = colors_a[i], lty = 2)  
  }
  else{
    lines(x, f(a_values[i], mu_1, mu_2, 3, s_2, x), col = colors_a[i])
  }
}
legend("topright", legend = paste("a =", round(a_values, 2)), col = colors_a, lty = 1)

#plot of f for different values of s_2
a=0.2
s_2_values <- seq(1, s_1, length.out = 10)
s_2_values<-s_2_values[1:length(s_2_values)-1] #We remove the last value because it is equal to s_1 and we need s_1>s_2
colors_s2 <- rainbow(length(s_2_values))

plot(x, f(a, mu_1, mu_2, s_2_values[1], s_2, x), type = "l", col = colors_s2[1], xlab = "x", ylab = "f(x)", main = "Plot of f for different values of s_2 with a=0.2")

for (i in 2:length(s_2_values)) {
  lines(x, f(a, mu_1, mu_2, s_2_values[i], s_2, x), col = colors_s2[i])
}
legend("topright", legend = paste("s_2 =", round(s_2_values,2)), col = colors_s2, lty = 1)

#We set from now on
mu_1=0
mu_2=1
s_1=3
s_2=1
a=0.2 
#######################################Question 4############################################
# Function CDF F(x)
F <- function(x, a, mu_1, mu_2, s_1, s_2) {
  F1 <- pnorm(x, mean = mu_1, sd = s_1)  # CDF for the first normale
  F2 <- pnorm(x, mean = mu_2, sd = s_2)  # CDF for the second normale
  return((F1 - a * F2)/(1-a))
}

# Step 1: Compute the CDF over a grid of values.
create_cdf_table <- function(a, mu_1, mu_2, s_1, s_2, grid_size = 1000) {
  x_vals <- seq(-10, 10, length.out = grid_size)  # Grille of x
  cdf_vals <- F(x_vals, a, mu_1, mu_2, s_1, s_2)  # Values of the CDF
  return(list(x = x_vals, cdf = cdf_vals))  # Return a table of CDF
}

# Step 2: Approximate the inverse of the CDF using interpolation.
inv_cdf_approx <- function(u, cdf_table) {
  # Interpolation of the inverse of the CDF
  return(approx(cdf_table$cdf, cdf_table$x, xout = u)$y)
}

# Step 3: Generate random variables using the approximated inverse of the CDF.
inv_cdf<- function(n, a, mu_1, mu_2, s_1, s_2) {
  cdf_table <- create_cdf_table(a, mu_1, mu_2, s_1, s_2)  # Create the table of CDF
  u_vals <- runif(n)  # generate n uniform values in (0, 1)
  samples <- sapply(u_vals, inv_cdf_approx, cdf_table = cdf_table)  # Generate the samples
  return(samples)
}


#######################################Question 5############################################
# Generate 10,000 samples
n <- 10000
samples <- inv_cdf(n, a, mu_1, mu_2, s_1, s_2)

# Visualize the histogram of the samples
hist(samples, probability = TRUE, breaks = 50, main = "Histogram of samples via CDF Inversion", col = "lightgreen")

# Superimpose the theoretical density function
curve(f(a, mu_1, mu_2, s_1, s_2, x), add = TRUE, col = "blue", lwd = 2)

#######################################Question 7############################################
###Accept-Reject Random Variable simulation###
M=1/(1-a) #theorical acceptance rate
n=10000

accept_reject<-function(n){
  samples <- numeric(n)
  count <- 1
  while (count <= n) {
    U <- runif(1)
    X <- rnorm(1, mu_1, s_1)
    R <- f(a, mu_1, mu_2, s_1, s_2, X) / (M * dnorm(X, mu_1, s_1))
    if (U <= R) {
      samples[count] <- X
      count <- count + 1
    }
  }
  return(samples)
}


samples <- accept_reject(n)

hist(samples, probability = TRUE, breaks = 50, main = "Histogram of the samples with Accept-Reject Method", col = "lightblue")
curve(f(a, mu_1, mu_2, s_1, s_2, x), add = TRUE, col = "blue", lwd = 2)

#######################################Question 8############################################
# We will plot the acceptance rate as a function of a

# Initialize parameters
mu_1 <- 0
mu_2 <- 1
s_1 <- 3
s_2 <- 1

# Define a function to calculate acceptance rate for a specific value of a
calculate_acceptance_rate <- function(a, n = 10000) {
  M <- 1 / (1 - a)  # Theoretical acceptance rate
  samples <- numeric(n)
  count <- 1
  attempts <- 0  # Count attempts
  
  while (count <= n) {
    U <- runif(1)
    X <- rnorm(1, mu_1, sqrt(s_1))
    R <- f(a, mu_1, mu_2, s_1, s_2, X) / (M * dnorm(X, mu_1, sqrt(s_1)))
    attempts <- attempts + 1  # Increment attempts
    if (U <= R) {
      samples[count] <- X
      count <- count + 1
    }
  }
  
  # Calculate acceptance rate
  acceptance_rate <- n / attempts
  return(acceptance_rate)
}

# Values of a from 0 (excluded) to a_star
a_values <- seq(0.01, a_star, length.out = 100)  # Avoiding 0 because division by zero

# Store the acceptance rates
acceptance_rates <- sapply(a_values, calculate_acceptance_rate)

# Plot the acceptance rate
par(mfrow = c(2, 1))
plot(a_values, acceptance_rates, type = "l", col = "red", lwd = 2, main = "Acceptance Rate as a Function of a", xlab = "a", ylab = "Acceptance Rate")
legend("topright", legend = c("Empirical Acceptance Rate"), col = c("red"), lty = c(1, 1))
plot(a_values, (1 - a_values), col = "blue")
legend("topright", legend = c("Theoretical Acceptance Rate"), col = c("blue"), lty = c(1, 1))



#######################################Question 11############################################

#We had set from now on
mu_1=0
mu_2=1
s_1=3
s_2=1
a=0.2 

Z=1/(1-a)  #Normalization constant

#pdf of f
f <- function(a, mu_1, mu_2, s_1, s_2, x) {
  return ((dnorm(x, mu_1, s_1)-a*dnorm(x, mu_2, s_2))/(1-a))
}

f1<-function(x, mu, sigma) {
  return(dnorm(x,mu_1,s_1))
}

integrable_f<-function(x){  #Function that we will integrate to get the normalization constant
  return(f(a,mu_1,mu_2,s_1,s_2,x))
}



F <- function(x, a, mu_1, mu_2, s_1, s_2) {
  F1 <- pnorm(x, mean = mu_1, sd = s_1)  # CDF for the first normale
  F2 <- pnorm(x, mean = mu_2, sd = s_2)  # CDF for the second normale
  return((F1 - a * F2)/(1-a))
}

k=10 #number of strata
#We partition the support of f in k intervals

x_neg=-10
x_plus=10 #the support of f is [x_neg,x_plus]

partition=seq(-8,8,length=k+1)  #We create D1,...,Dk
partition=c(x_neg,partition,x_plus)  #We add D0_neg and D0_pos

integral_f1_D0=integrate(f1,lower=-Inf,upper=partition[2])$value+integrate(f1,lower=partition[k+2],upper=Inf)$value

g_max<-function(x){   #The function that multiplied with M_max, dominates f
  if ((x>=partition[1] && x<=partition[2] )||(x>partition[k+2] && x<=partition[k+3])){ # We are in D0
    return(f1(x,mu_1,s_1)/integral_f1_D0)
  }
  else{  #We are in D1,...,Dk
    for (i in 1:k){
      if (x>=partition[i+1] && x<=partition[i+2]){ # We are in Di
        return(1/(abs(partition[i+1]-partition[i+2])))
      }
    }
  }
}

M_max<-function(k){   #Theorical acceptance rate
  M<-numeric(k)
  for(i in 1:k){
    sup_f1_i <- max(dnorm(partition[i+1], mu_1, s_1), dnorm(partition[i + 2], mu_1, s_1))
    inf_f2_i <- min(dnorm(partition[i+1], mu_2, s_2), dnorm(partition[i + 2], mu_2, s_2))
    M[i]=Z*(sup_f1_i-a*inf_f2_i)*abs(partition[i+2]-partition[i+1])
    M_last=Z*integral_f1_D0
    return(max(c(M,M_last)))
  }
}

theorical_acceptance_rate=M_max(k)


stratified<-function(n){
  n_count=0
  number_of_simulations=0
  sample_final=numeric(n)
  M<-M_max(k)
  while(n_count<n){
    U=runif(1)
    number_of_simulations=number_of_simulations+1
    X=runif(1,x_neg,x_plus)
    if(U<=f(a,mu_1,mu_2,s_1,s_2,X)/g_max(X)*M){
      sample_final[n_count+1]=X
      n_count=n_count+1
    }
  }
  cat("Empirical acceptance rate:",n/number_of_simulations,"\n")
  cat("Theorical acceptance rate:",theorical_acceptance_rate,"\n")
  return(sample_final)
}
#We test the stratified accept-reject method, and plot the results

n=10000
simulation = stratified(n)

hist(simulation, breaks = 50, probability = TRUE, 
     main = "Histogram of the simulation by stratified accept-reject method", xlab = "x")

x = seq(-10, 10, 0.01)
y = f(a, mu_1, mu_2, s_1, s_2, x)
lines(x, y, col = "red")

#######################################Question 15############################################
par(mfrow = c(1, 1))

#Parameters fixed 
mu_1=0
mu_2=1
s_1=3
s_2=1
a=0.2 

empirical_cdf <- function(x, samples) {
  return(mean(samples <= x))
}

# CDF of f, with our fixed parameters a, mu_1, mu_2, s_1, s_2  
F_X <- function(x) {
  F1 <- pnorm(x, mu_1, s_1)
  F2 <- pnorm(x, mu_2, s_2)
  return((F1-a*F2)/(1-a))
}


plot_cdf_convergence <- function() {
  # Sample sizes to compare
  n_values <- c(100, 1000, 10000)  
  # Grid of x values for the CDF
  x_vals <- seq(-15, 15, length.out = 100)  
  
  # Create an empty plot
  plot(NULL, xlim = c(-10, 10), ylim = c(0, 1), xlab = "x", ylab = "F(x)",
       main = "Convergence of F_n(x) to F_X(x)")

  # Add the theoretical CDF
  curve(F_X, from = -10, to = 10, col = "red", lwd = 2, add = TRUE)

  # Add empirical CDFs for each n
  for (n in n_values) {
    samples <- accept_reject(n) #We use accept-reject method to sample Xn
    F_MC_vals <- sapply(x_vals, function(x) mean(samples <= x)) #We cannot use empirical_cdf because it takes two arguments
    lines(x_vals, F_MC_vals, lty = 2, lwd = 2, col = rainbow(length(n_values))[which(n_values == n)])
  }

  # Add a legend
  legend("bottomright", legend = c("F_X(x) (Theoretical)", paste0("F_n(x), n=", n_values)),
         col = c("red", rainbow(length(n_values))), lwd = 2, lty = c(1, rep(2, length(n_values))))
}

# Execute visualization 
plot_cdf_convergence()

#######################################Question 17############################################
n=1000
alpha=0.05
q=qnorm(1-alpha/2)

#Confidence interval for x=1
confidence_interval<-function(x,n){
  sample_Xn=accept_reject(n)
  Fn=empirical_cdf(x,sample_Xn)
  a=Fn-q*sqrt(Fn*(1-Fn)/n)
  b=Fn+q*sqrt(Fn*(1-Fn)/n)
  return(c(a,b))
}

show_confidence_interval<-function(x,n){
  A=confidence_interval(x,n)[1]
  B=confidence_interval(x,n)[2]
  cat("Confidence interval at level 95% for x=",x,"and n=",n,": [",A,",",B,"]\n")
  cat("Theorical value of F_X(",x,"):",F_X(x),"\n")
}


#Confidence interval for x=1
x=1
show_confidence_interval(x,n)
#Confidence interval for x=-15
x=-15
show_confidence_interval(x,n)

#Compute the number of trials needed to get a confidence interval of width eps

#Precision of the confidence interval
eps=0.01
number_of_simulations<-function(x,eps){  
  N=floor(q^2*F_X(x)*(1-F_X(x))/(eps^2))+1
  return(N)
}

cat("We need",number_of_simulations(1,eps),"simulations to get a confidence interval of width", eps, "for x=1\n")
cat("We need",number_of_simulations(-15,eps),"simulations to get a confidence interval of width", eps ,"for x=-15\n")

plot_F_behavior <- function() {
  x_values <- seq(-20, 2, length.out = 100)
  F_values <- sapply(x_values, function(x) F_X(x))

  plot(x_values, F_values, type = "l", main = "Behavior of F(x)",
       xlab = "x", ylab = "F(x)", col = "blue")
  abline(v = c(1, -15), col = "red", lty = 2)  # Mark x = 1 and x = -15
}

# Execute the visualization
plot_F_behavior()

cat("\nExplanation:\n")
cat("For x = -15, F(x) is so close to 0 that the confidence interval is trivially narrow, requiring n = 1.\n")
cat("For x = 1, F(x) lies in a region where the distribution is more variable, requiring a higher n to ensure precision.\n")

#######################################Question 21############################################

empirical_quantile<-function(u,Xn){
  order_stat=sort(Xn)
  n=length(Xn)
  return(order_stat[ceiling(u*n)])
}

#Compute the empirical quantile for u and n given
check_empirical_quantile<-function(n,u){
  Xn=accept_reject(n)
  cat("Empirical quantile for u=",u," and n=",n,":",empirical_quantile(u,Xn),"\n")
}

u_values=seq(0,1,0.01)

y=sapply(u_values, function(u){empirical_quantile(u,accept_reject(1000))}) #empirical quantile for n=1000
y1=sapply(u_values, function(u){return(inv_cdf_approx(u,create_cdf_table(a,mu_1,mu_2,s_1,s_2)))}) #We use the inverse CDF method to get the quantile

plot(u_values,y,type="l",col="blue",xlab="u",ylab="Empirical quantile",main="Empirical vs Theoretical quantiles a function of u",lwd=2)
lines(u_values,y1 ,col="red",lwd=2)
legend("topleft",legend=c("Empirical quantile","Theoretical quantile"),col=c("blue","red"),lty=1)

#######################################Question 22############################################

u_values=c(0.1,0.2,0.4,0.5,0.9,0.99,0.999,0.9999)
alpha=0.05
q=qnorm(1-alpha/2)

number_of_simulations_quantile<-function(u,eps){
  Qu=inv_cdf_approx(u,create_cdf_table(a,mu_1,mu_2,s_1,s_2))
  N=ceiling(q^2*u*(1-u)/((eps^2)*integrable_f(Qu)^2))
  return(N)
}


show_simulations_quantile<-function(U,eps){
  for(u in U){
    cat("We need",number_of_simulations_quantile(u,eps),"simulations to get a confidence interval of width", eps, "for u=",u,"\n")
  }
}

show_simulations_quantile(u_values,0.01)

#######################################Question 24############################################
n=10000
alpha=0.05
q=qnorm(1-alpha/2)
eps=0.01

u=0.3 #We choose u=0.3

#We set these parameters for rest of the project

accept_reject_quantile<-function(q,n){
  Xn=accept_reject(n)
  delta_hat=mean(Xn>=q)
  return(delta_hat)
}


accept_reject_quantile(u,n)


#######################################Question 25############################################


confidence_interval_delta<-function(u,n,eps){
  delta_hat_reject=accept_reject_quantile(u,n)
  a=delta_hat_reject-q*sqrt(delta_hat_reject*(1-delta_hat_reject)/n)
  b=delta_hat_reject+q*sqrt(delta_hat_reject*(1-delta_hat_reject)/n)
  N=ceiling(q^2*delta_hat_reject*(1-delta_hat_reject)/(eps^2))
  return(c(a,b,N))
}

show_confidence_interval_delta<-function(u,n,eps){
  delta=1-F_X(u) #Theorical value of delta
  res=confidence_interval_delta(u,n,eps)
  A=res[1]
  B=res[2]
  cat("Confidence interval at level 95% for u=",u,"and n=",n," for the accept-reject method : [",A,",",B,"]\n")
  #cat("Theorical value of delta :",delta,"\n")
  cat("Number of simulations needed to get a confidence interval of width",eps," and for u=",u," : ",res[3],"\n")
}

#Test the confidence interval

show_confidence_interval_delta(u,n,eps)  

#######################################Question 27############################################
f <- function(x, mu_1, mu_2, s_1, s_2, a) {
  (dnorm(x, mean = mu_1, sd = s_1) - a * dnorm(x, mean = mu_2, sd = s_2)) / (1 - a)
}
g <- function(x, mu0, gamma) {
  1 / (pi * gamma * (1 + ((x - mu0) / gamma)^2))
}

mu0 <- 1  # Location parameter for the Cauchy distribution
gamma_values <- c(2, sqrt(7), 3, sqrt(10), 4)  # Different scale parameters
colors <- c("red", "green", "purple", "orange", "brown") 


x_vals <- seq(-10, 10, length.out = 1000)


f_vals <- f(x_vals, mu_1, mu_2, s_1, s_2, a)


plot(x_vals, f_vals, type = "l", col = "blue", lwd = 2,
     ylab = "Density", xlab = "x",
     main = "Comparison of f(x) and g(x) for different values of gamma")

# Overlay g(x) for each gamma
for (i in 1:length(gamma_values)) {
  gamma <- gamma_values[i]
  g_vals <- g(x_vals, mu0, gamma)  # Calculate g(x) values
  lines(x_vals, g_vals, col = colors[i], lwd = 2, lty = 2)
}

legend("topright", 
       legend = c("f(x)", paste("g(x), gamma =", round(gamma_values, 2))), 
       col = c("blue", colors), 
       lty = c(1, rep(2, length(gamma_values))), 
       lwd = 2)

#######################################Question 28############################################
u=0.3
n=10000   
eps=0.01
mu_1=0
mu_2=1
s_1=3
s_2=1
a=0.2 
q=qnorm(1-alpha/2)

f <- function(x, mu_1, mu_2, s_1, s_2, a) {
  (dnorm(x, mean = mu_1, sd = s_1) - a * dnorm(x, mean = mu_2, sd = s_2)) / (1 - a)
}

gamma <- sqrt(10)
IS_quantile <- function(u, n, epsilon=NULL) {
  
  Xn <- rcauchy(n, u, gamma)
  
  weights <- (f(a, mu_1, mu_2, s_1, s_2, Xn) / dcauchy(Xn,u, gamma)) * (Xn >= u)
  
  delta_IS <- mean(weights)
  
  var_delta_IS <- (mean(weights^2) - delta_IS^2) / n
  
  IC_inf <- delta_IS - (1.96 / sqrt(n)) * sqrt(var_delta_IS)
  IC_sup <- delta_IS + (1.96 / sqrt(n)) * sqrt(var_delta_IS)
  
  if (!is.null(epsilon)) {
    repeat {
      error_margin <- 0.5 * (IC_sup - IC_inf)
      if (error_margin <= epsilon) break
      
      n <- n + 1
      new_sample <- rcauchy(1, u, gamma)
      Xn <- c(Xn, new_sample)
      
      weights <- (f(a, mu_1, mu_2, s_1, s_2, Xn) / dcauchy(Xn, u, gamma)) * (Xn >= u)
      delta_IS <- mean(weights)
      var_delta_IS <- (mean(weights^2) - delta_IS^2) / n
      
      IC_inf <- delta_IS - (1.96 / sqrt(n)) * sqrt(var_delta_IS)
      IC_sup <- delta_IS + (1.96 / sqrt(n)) * sqrt(var_delta_IS)
    }
  }
  
  return(list(delta_IS = delta_IS, CI = c(IC_inf, IC_sup), n = n))
}

#result <- IS_quantile(u = 0.3, n = 10000, epsilon = 0.01) There is a bug in the block of code, so we will not run it
#Sometimes it works, sometimes it doesn't and we do not find the bug. We will just show the code and the expected output
#in the pdf file

#cat("Estimation of delta_IS :", result$delta_IS, "\n")
#cat("95% Confidence Interval :", result$CI[1], "to", result$CI[2], "\n")

#######################################Question 31############################################
u=0.3
n=10000
eps=0.01
mu_1=0
mu_2=1
s_1=3
s_2=1
a=0.2 
q=qnorm(1-alpha/2)

f <- function(a, mu_1, mu_2, s_1, s_2, x) {
  return ((1/(1-a))*(dnorm(x, mu_1, s_1) -a*dnorm(x, mu_2, s_2)))
}

phi<-function(x,q){
  return(ifelse(x>=q,1,0))
}

s_mu_1<-function(x,mu_1,s_1,mu_2,s_2){
  return((x-mu_1)*dnorm(x,mu_1,s_1)/(s_1^2*f(a,mu_1,mu_2,s_1,s_2,x)*(1-a)))
}


expectance_s_mu_1<-function(mu_1,s_1,mu_2,s_2){
  return(integrate(function(x){s_mu_1(x,mu_1,s_1,mu_2,s_2)*f(a,mu_1,mu_2,s_1,s_2,x)},-1,1)$value)
}

variance_s_mu_1<-function(mu_1,s_1,mu_2,s_2){
  Xn=accept_reject(n)
  return(mean(sapply(Xn,function(x){(s_mu_1(x,mu_1,s_1,mu_2,s_2)-expectance_s_mu_1(mu_1,s_1,mu_2,s_2))^2})))
}

CV_quantile<-function(q,n){
  Xn=accept_reject(n)
  b_star=-cov(Xn,s_mu_1(Xn,mu_1,s_1,mu_2,s_2))/var(s_mu_1(Xn,mu_1,s_1,mu_2,s_2))
  phi_1=sapply(Xn,function(x){phi(x,q)})
  s_mu_1=sapply(Xn,function(x){s_mu_1(x,mu_1,s_1,mu_2,s_2)})
  return(mean(phi_1+b_star*(s_mu_1-expectance_s_mu_1(mu_1,s_1,mu_2,s_2))))
}

confidence_interval_delta_CV<-function(u,n,eps){
  delta_hat_CV=CV_quantile(u,n)
  a=delta_hat_CV-q*sqrt(delta_hat_CV*(1-delta_hat_CV)/n)
  b=delta_hat_CV+q*sqrt(delta_hat_CV*(1-delta_hat_CV)/n)
  N=ceiling(q^2*delta_hat_CV*(1-delta_hat_CV)/(eps^2))
  return(c(a,b,N))
}

show_confidence_interval_delta_CV<-function(u,n,eps){
  delta=1-F_X(u) #Theorical value of delta
  res=confidence_interval_delta_CV(u,n,eps)
  A=min(res[1],res[2])
  B=max(res[1],res[2])
  cat("Confidence interval at level 95% for u=",u,"and n=",n," with the CV estimator: [",A,",",B,"]\n")
  #cat("Theorical value of delta :",delta,"\n")
  cat("Number of simulations needed to get a confidence interval of width",eps," and for u=",u," : ",res[3],"\n")
}

show_confidence_interval_delta_CV(u,n,eps)
