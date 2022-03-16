#' Causal binary loss function model for Bayesian linear probability models
#'
#' This function implements the causal binary loss function model (Suzuki 2022)
#' for Bayesian linear probability models.
#'
#' This function implements the causal binary loss function model (Suzuki 2022) for Bayesian
#' linear probability models. It computes expected losses under an intervention (in causal inference
#' senses) and those under no intervention (i.e., the status quo), across different ratios of the
#' cost of the intervention to the cost of an undesirable outcome.
#'
#' For simplicity, the function computes these ratios by taking a fixed value for the cost of an
#' intervention, while taking a vector of various values for the cost of an undesirable outcome.
#'
#' It is possible to specify the minimum desirable and undesirable effect sizes on the probability
#' scale (\code{md} and \code{mu} respectively). If \code{md}=0, the strict inequality md<0 is assumed;
#' otherwise the inequality <= and >= is assumed.
#'
#' One caveat in the setup of linear probability models is that the binary dependent variable
#' must be coded such that the value of 1 means an undesirable outcome and the value of 0 means
#' a desirable outcome
#'
#' If a greater value of the intervention variable captures an intervention meant to increase
#' the likelihood of an undesirable outcome, you must specify a negative value for \code{unit},
#' the size of the unit change of interest, in the function. In this way, a decrease in the
#' intervetion variable captures an intervention meant to reduce the likelihood of an undesirable
#' outcome
#'
#' If the variable of an intervention is a continuous / ordered variable, you can specify
#' \code{unit}, the size of the unit change of interest, other than 1 (which is the default value).
#'
#' If the average treatment effect is the quantity of interest and there are more than one independent
#' variable used in a linear probability model, the vector should be created that is the sum of all
#' covariates plus the posterior mean of their coefficients, and this should be used as an input
#' for \code{pi}. The loss function will compute the predicted probability of the outcome for each
#' observation and then will use their mean (the missing values in the vector, if any, will be
#' removed).
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
#' @param posterior A numeric vector of posterior samples for the estimated effect of the intervention
#' @param pi A numeric value or vector for the baseline likelihood of the undesirable outcome
#' @param md A numeric value for the minimum desired effect size on the probability scale; default = 0
#' @param mu A numeric value for the minimum undesired effect size on the probability scale; default = 0
#' @param unit A numeric value for the size of the unit change of interest in the intervention variable; default = 1
#' @param cp A numeric value for the cost of the intervention; default = 1
#' @param ce A numeric value or vector for the cost(s) of the undesirable outcome; default = c(rep(101:10000)/100)
#' @return A list having two elements: the first element is a data frame that has the expected losses under the intervention ("losses_i"), those under no intervention ("losses_noi"), and the ratio of the cost of the intervention to the cost of the undesirable outcome ("cost_ratio"); the second element is the ggplot object that plots the first element
#' @section Author(s):
#' Author & Maintainer: Akisato Suzuki (\email{akisato.suzuki@@gmail.com})
#' @examples
#' \dontrun{
#' # Compute expected losses
#' loss_lpm(posterior = posterior, pi = pi)
#'
#' # Compute expected losses with specifying the minimum desired effect size
#' # as a probability difference of 0.1
#' loss_lpm(posterior = posterior, pi = pi, md = 0.1)
#'
#' # If a value of one in the binary variable captures an intervention meant
#' # to increase the likelihood of an undesirable outcome, specify -1 in unit
#' loss_lpm(posterior = posterior, pi = pi, unit = -1)
#'
#' # Change the ratio of the costs
#' loss_lpm(posterior = posterior, pi = pi, cp = 2, ce = rep(5:10))
#' }
#' @export


loss_lpms <- function(posterior, pi, md=0, mu=0, unit=1, cp=1, ce=c(rep(101:10000)/100)){


  # Identify incorrect inputs
  if(is.numeric(posterior)==FALSE | length(posterior[is.na(posterior) == TRUE]) > 0){
    stop("The input for POSTERIOR must be a numeric vector and must not contain missing values.")
  }

  if(is.numeric(pi)==FALSE){
    stop("The input for PI must be a numeric vector.")
  }

  if(is.numeric(md)==FALSE | length(md) > 1){
    stop("The input for MD must be a numeric value.")
    }

  if(is.numeric(mu)==FALSE | length(mu) > 1){
    stop("The input for MU must be a numeric value.")
    }

  if(is.numeric(unit)==FALSE | length(mu) > 1){
    stop("The input for UNIT must be a numeric value.")
  }

  if(is.numeric(cp)==FALSE | length(cp) > 1){
    stop("The input for CP must be a numeric value.")
  }

  if(is.numeric(ce)==FALSE){
    stop("The input for CE must be a numeric vector.")
  }


  # Scale the effect size by the size of the intervention variable's unit change of interest
  posterior <- posterior * unit


  # Return warning messages
  if(min(posterior)>=0){
    warning("The estimate of the causal effect has no probability of reducing the likelihood of the undeisrable outcome and, therefore, by definition the policy intervention is suboptimal regardless of its cost.")
  }

  if(md<min(posterior)){
    warning("The estimate of the causal effect has no probability of reducing the likelihood of the undeisrable outcome by more than the specified minimum desired effect size and, therefore, by definition the policy intervention is suboptimal regardless of its cost.")
  }

  if(length(ce[cp >= ce]) > 0){
    warning("Where the cost of an intervention is not smaller than the cost of an undesirable outcome, the expected loss will never be smaller when an intervention is done.")
  }


  # Define the loss function
  loss <- function(c_p, c_e, i, pi, theta_int, p, theta_unint, q){
    c_p*i + c_e*mean(
      pi+theta_int*p*i+theta_unint*q*i, na.rm = TRUE
    )
  }


  # Preparation for estimation
  cases <- cbind(cp, ce)
  n <- c(rep(1:length(cases[,1])))
  cases <- cbind(cases, n)
  if(md == 0){
    theta_int <- ifelse(length(posterior[posterior<md])>0,
                        mean(posterior[posterior<md]), 0)
  }
  else{
    theta_int <- ifelse(length(posterior[posterior<=md])>0,
                        mean(posterior[posterior<=md]), 0)
  }
  theta_unint <- ifelse(length(posterior[posterior>=mu])>0,
                        mean(posterior[posterior>=mu]), 0)
  p <- length(posterior[posterior<md])/length(posterior)
  q <- length(posterior[posterior>=mu])/length(posterior)


  # Expected loss per c_p and c_e given I(i)=1
  losses_i <- c()
  for(i in 1:length(n)){
    loss_i <- loss(cases[i,1], cases[i,2], 1, pi, theta_int, p, theta_unint, q)
    losses_i <- c(losses_i, loss_i)
  }


  # Estimate expected loss per c_p and c_e given I(i)=0
  losses_noi <- c()
  for(i in 1:length(n)){
    loss_noi <- loss(cases[i,1], cases[i,2], 0, pi, 0, 0, 0, 0)
    losses_noi <- c(losses_noi, loss_noi)
  }


  # Calculate the ratio of c_p to c_e
  cost_ratios <- c()
  for(i in 1:length(n)){
    cost_ratio <- cases[i,1]/cases[i,2]
    cost_ratios <- c(cost_ratios, cost_ratio)
  }


  losses <- as.data.frame(cbind(losses_i,
                                losses_noi,
                                cost_ratios))


  # Plot the results
  ggplot2::ggplot(data=losses, mapping=ggplot2::aes(x=cost_ratios)) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste("Black dots: expected losses under the intervention",
                           "\nGrey dots: expected losses under no intervention", sep="")) +
    ggplot2::geom_point(ggplot2::aes(y=losses_i), color="black") +
    ggplot2::geom_point(ggplot2::aes(y=losses_noi), color="grey") +
    ggplot2::labs(x="Cost of the intervention / cost of the outcome",
                  y="Expected loss") -> fig

  list <- list()
  list[[1]] <- losses
  list[[2]] <- fig

  return(list)

}
