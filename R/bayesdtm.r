#' A Bayesian Decision-Theoretic Model to Compute Expected Losses with and without an Intervention
#'
#' This package implements a Bayesian decision-theoretic model to compute expected losses given
#' an intervention (in causal inference senses) and those without it across different ratios of
#' the cost of the intervention to the cost of the status quo, and returns the plot of
#' these expected losses.
#'
#' This package implements a Bayesian decision-theoretic model to compute expected losses given
#' an intervention and those without it across different ratios of the cost of the intervention
#' to the cost of the status quo, and returns the plot of these expected losses.
#' Currently there is only a function for outputs from Bayesian logistic regression.
#'
#' For a full example from estimating posterior samples to using the package, please see
#' \href{https://akisatosuzuki.github.io/bayesdtm.html}{https://akisatosuzuki.github.io/bayesdtm.html}.
#'
#' For a theoretical rationale for using this model, please see the following paper:
#'
#' \href{https://arxiv.org/abs/2008.10903}{Suzuki, Akisato. 2020. "Policy Implications of
#' Statistical Estimates: A General Bayesian Decision-Theoretic Model for Binary Outcomes."
#' arXiv:2008.10903 \[stat.ME\]. https://arxiv.org/abs/2008.10903.}
#'
#' @section Author(s):
#' Author & Maintainer: Akisato Suzuki (\email{akisato.suzuki@@gmail.com})
#'
#' @section bayesdtm functions:
#' loss_logit: compute expected losses based on a Bayesian decision-theoretic model for outputs from Bayesian logistic regression
#'
#' @docType package
#' @name bayesdtm
NULL
