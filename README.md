# bayesdtm: Bayesian Decision-Theoretic Models to Compute Expected Losses under an Intervention and the Status Quo

This R package implements Bayesian decision-theoretic models to compute expected losses under an intervention (in causal inference senses) and those under no intervention (i.e., the status quo), across different ratios of the cost of the intervention to the cost of an undesirable outcome.

For a full example from estimating posterior samples to using the package, please see <a href="https://akisatosuzuki.github.io/bayesdtm.html" target="_blank">https://akisatosuzuki.github.io/bayesdtm.html</a>.

For a theoretical rationale, please see <a href="https://doi.org/10.1080/2330443X.2022.2050328" target="_blank">Suzuki (2022)</a>.

## Installation

To install this package, you need to have the <a href="https://CRAN.R-project.org/package=devtools" target="_blank">devtools package</a> (Wickham, Hester, and Chang 2020) installed. Once you have the devtools package installed, run the following code to install the bayesdtm package:

<code>
devtools::install_github("AkisatoSuzuki/bayesdtm")
</code>

## Citation

If you use this package, please cite the following items:

Suzuki, Akisato. 2022. "Policy Implications of Statistical Estimates: A General Bayesian Decision-Theoretic Model for Binary Outcomes." <em>Statistics and Public Policy</em>, https://doi.org/10.1080/2330443X.2022.2050328.

Suzuki, Akisato. 2022. "bayesdtm: Bayesian Decision-Theoretic Models to Compute Expected Losses under an Intervention and the Status Quo." R package version 1.0.0.

## Author

Author & Maintainer: Akisato Suzuki (akisato.suzuki@gmail.com)

## References

Suzuki, Akisato. 2022. "Policy Implications of Statistical Estimates: A General Bayesian Decision-Theoretic Model for Binary Outcomes." <em>Statistics and Public Policy</em>, https://doi.org/10.1080/2330443X.2022.2050328.

Wickham, Hadley, Jim Hester, and Winston Chang. 2020. "devtools: Tools to Make Developing R Packages Easier." R package version 2.3.0. https://CRAN.R-project.org/package=devtools.
