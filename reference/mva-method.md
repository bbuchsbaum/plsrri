# Multivariate Analysis Method Protocol

R6 base class defining the protocol that all multivariate analysis
methods must implement. Methods declare their capabilities, implement
`fit()`, and optionally provide resampling hooks and UI metadata.

## Details

This is the core abstraction for method-neutral multivariate
neuroimaging analysis. PLS, CPCA, CCA, ICA, and other methods all
implement this protocol. The framework uses capabilities to adapt the
builder API, Shiny UI, and result accessors to each method
automatically.
