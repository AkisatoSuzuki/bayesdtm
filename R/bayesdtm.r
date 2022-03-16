#' Bayesian Decision-Theoretic Models to Compute Expected Losses
#' under an Intervention and the Status Quo
#'
#' This package implements Bayesian decision-theoretic models to compute expected losses under
#' an intervention (in causal inference senses) and those under no intervention (i.e., the status
#' quo), across different ratios of the cost of the intervention to the cost of an undesirable
#' outcome.
#'
#' This package implements Bayesian decision-theoretic models to compute expected losses under
#' an intervention (in causal inference senses) and those under no intervention (the status quo),
#' across different ratios of the cost of the intervention to the cost of an undesirable outcome.
#'
#' Currently there are functions for outputs from Bayesian logistic regression and Bayesian
#' linear probability models, where the causal binary loss function model, introduced in
#' Suzuki (2022), is implemented.
#'
#' For a full example from estimating posterior samples to using the package, please see
#' \href{https://akisatosuzuki.github.io/bayesdtm.html}{https://akisatosuzuki.github.io/bayesdtm.html}.
#'
#' For a theoretical rationale for using Bayesian decision-theoretic models such as the causal binary
#' loss function model, please see the paper:
#'
#' \href{https://doi.org/10.1080/2330443X.2022.2050328}{Suzuki, Akisato. 2022. "Policy Implications of
#' Statistical Estimates: A General Bayesian Decision-Theoretic Model for Binary Outcomes."
#' Statistics and Public Policy, https://doi.org/10.1080/2330443X.2022.2050328.}
#'
#' If you use this package, please cite the following items:
#'
#' Suzuki, Akisato. 2022. "Policy Implications of Statistical Estimates: A General Bayesian
#' Decision-Theoretic Model for Binary Outcomes." Statistics and Public Policy,
#' https://doi.org/10.1080/2330443X.2022.2050328.
#'
#' Suzuki, Akisato. 2022. "bayesdtm: Bayesian Decision-Theoretic Models to Compute Expected Losses
#' under an Intervention and the Status Quo." R package version 1.0.0.
#'
#' @section Author(s):
#' Author & Maintainer: Akisato Suzuki (\email{akisato.suzuki@@gmail.com})
#'
#' @section bayesdtm functions:
#' loss_logit: Causal binary loss function model for Bayesian logistic regression
#'
#' loss_lpms: Causal binary loss function model for Bayesian linear probability models
#'
#' @docType package
#' @name bayesdtm
NULL
