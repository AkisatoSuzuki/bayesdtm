# bayesdtm: A Bayesian Decision-Theoretic Model to Compute Expected Losses with and without an Intervention

This is an R package. This package implements a Bayesian decision-theoretic model to compute expected losses given an intervention (in causal inference senses) and those without it across different ratios of the cost of the intervention to the cost of the status quo, and returns the plot of these expected losses.

For a full example from estimating posterior samples to using the package, please see <a href="https://akisatosuzuki.github.io/bayesdtm.html" target="_blank">https://akisatosuzuki.github.io/bayesdtm.html</a>.

For a theoretical rationale for using this model, please see <a href="https://arxiv.org/abs/2008.10903" target="_blank">Suzuki (2020)</a>.

# Installation

To install this package, you need to have the <a href="https://CRAN.R-project.org/package=devtools" target="_blank">devtools package</a> (Wickham, Hester, and Chang 2020) installed. Once you have the devtools package installed, run the following code to install the bayesdtm package:

<code>
devtools::install_github("AkisatoSuzuki/bayesdtm")
</code>

# Citation

If you use this package, please cite the following items:

Suzuki, Akisato. 2020. "Policy Implications of Statistical Estimates: A General Bayesian Decision-Theoretic Model for Binary Outcomes." arXiv:2008.10903 [stat.ME]. https://arxiv.org/abs/2008.07478.

Suzuki, Akisato. 2021. "bayesdtm: A Bayesian Decision-Theoretic Model to Compute Expected Losses with and without an Intervention." R package version 0.0.0.9002.

# References

Suzuki, Akisato. 2020. "Policy Implications of Statistical Estimates: A General Bayesian Decision-Theoretic Model for Binary Outcomes." arXiv:2008.10903 [stat.ME]. https://arxiv.org/abs/2008.10903.

Wickham, Hadley, Jim Hester, and Winston Chang. 2020. "devtools: Tools to Make Developing R Packages Easier." R package version 2.3.0. https://CRAN.R-project.org/package=devtools.
