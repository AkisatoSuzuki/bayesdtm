#' Compute expected losses based on Bayesian logistic regression
#'
#' This function implements a Bayesian decision-theoretic model for outputs from Bayesian
#' logistic regression.
#'
#' This function implements a Bayesian decision-theoretic model for outputs from Bayesian
#' logistic regression. It computes expected losses given an intervention (in causal inference
#' senses) and those without it, across different ratios of the cost of the intervention to
#' the cost of an undesirable event. More specifically, in the loss function the cost of an
#' intervention is fixed at 1 while the cost of an undesirable event is changed from 1 to 100.
#' So the ratio of the costs is from 0.01 to 1, and there are more data points the smaller
#' the ratio.
#'
#' There are two caveats in the setup of logistic regression. The binary dependent variable
#' must be coded so that the value of 1 means an undesirable event and the value of 0 means
#' a desirable event. Similarly, the variable of an intervention must be coded so that a
#' larger value means an effect to reduce the likelihood of an undesirable event.
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
#' @param posterior A vector of posterior samples for the estimated effect of an intervention
#' @param pi A baseline log odds value of an undesirable event
#' @param md A minimum desired effect size in a log odds scale; default = 0
#' @param mu A minimum undesired effect size in a log odds scale; default = 0
#' @return A list having two elements: the first element is a data frame that has the expected losses given an intervention ("losses_i"), those given no intervention ("losses_noi"), and the ratio of the cost of the intervention to the cost of the undesirable event ("cost_ratio"); the second element is a ggplot object that plots the first element
#' @section Author(s):
#' Author & Maintainer: Akisato Suzuki (\email{akisato.suzuki@@gmail.com})
#' @examples
#' \dontrun{
#' # Compute expected losses
#' loss_logit(posterior = posterior, pi = pi)
#'
#' # Compute expected losses with specifying the minimum desired effect size
#' # as an odds ratio of 0.8
#' loss_logit(posterior = posterior, pi = pi, md = log(0.8))
#' }
#' @export


loss_logit <- function(posterior, pi, md=0, mu=0){


  # Identify incorrect inputs
  if(is.numeric(posterior)==FALSE){
    stop("The input for POSTERIOR must be a numeric vector.")
  }

  if(is.numeric(pi)==FALSE | length(pi) > 1){
    stop("The input for PI must be a numeric value.")
  }

  if(is.numeric(md)==FALSE | length(md) > 1){
    stop("The input for MD must be a numeric value.")
    }

  if(is.numeric(mu)==FALSE | length(mu) > 1){
    stop("The input for MU must be a numeric value.")
    }


  # Return warning messages
  if(min(posterior)>=0){
    warning("The estimate of the causal effect has no probability of reducing the likelihood of the undeisrable event and, therefore, by definition the policy intervention is suboptimal regardless of its cost.")
  }

  if(md<min(posterior)){
    warning("The estimate of the causal effect has no probability of reducing the likelihood of the undeisrable event by more than the specified minimum desired effect size and, therefore, by definition the policy intervention is suboptimal regardless of its cost.")
  }


  # Define the loss function
  loss <- function(c_p, c_e, i, pi, theta_int, p, theta_unint, q){
    c_p*i+c_e*(1/(1+exp(-(pi+theta_int*p*i+theta_unint*q*i))))
  }


  # Preparation for estimation
  cases <- expand.grid(1, c(rep(10:1000)/10))
  n <- c(rep(1:length(cases[,1])))
  cases <- cbind(cases, n)
  theta_int <- ifelse(length(posterior[posterior<md])>0,
                      mean(posterior[posterior<md]), 0)
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
    ggplot2::ggtitle(paste("Effect size < odds ratio ", round(exp(md), digits=2),
                  "\nBlack dots: expected losses given the intervention",
                  "\nGrey dots: expected losses given no intervention", sep="")) +
    ggplot2::geom_point(ggplot2::aes(y=losses_i), color="black") +
    ggplot2::geom_point(ggplot2::aes(y=losses_noi), color="grey") +
    ggplot2::labs(x="Cost of the intervention / cost of the outcome",
                  y="Expected loss") -> fig

  list <- list()
  list[[1]] <- losses
  list[[2]] <- fig

  return(list)

}
