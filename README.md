# Monte Carlo Methods Project — Université Paris Dauphine

This repository contains the code and final report for our Monte Carlo simulation project, completed as part of the M1 Quantitative Methods course at Université Paris Dauphine - PSL.

## Project Members

- Wardakhan Kévin
- Erwan Ouabdesselam
- Matteo Casati

## Project Overview

The aim of this project is to explore and compare various Monte Carlo methods for simulating and estimating statistical properties of a non-standard probability distribution defined as a weighted mixture of two Gaussian distributions:

$$
f(x) \propto f_1(x) - a f_2(x)
$$

where:
- \(f_1(x)\) and \(f_2(x)\) are normal densities,
- \(a > 0\) controls the balance between the two densities.

Throughout the project, we implemented and analysed several methods to simulate random variables from this distribution and to estimate key statistics.

## Methods Implemented

- Inverse CDF Method  
  Numerical inversion of the cumulative distribution function for random variable generation.

- Accept-Reject Method  
  Using a proposal distribution and computing the acceptance rate.

- Stratified Accept-Reject Sampling  
  Partitioning the support of the distribution to improve sampling efficiency.

- Estimation of the CDF and Quantiles  
  Construction of empirical CDFs, quantile estimation, and confidence intervals.

- Precision Study  
  Estimating the required sample size to achieve a given precision in CDF or quantile estimation.

- Advanced: Importance Sampling and Control Variates (optional for future extension)
