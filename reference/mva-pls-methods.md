# PLS Method Implementations for MvaMethod Protocol

Wraps the existing PLS analysis engine as MvaMethod implementations.
These call
[`pls_analysis()`](https://bbuchsbaum.github.io/plsrri/reference/pls_analysis.md)
unchanged, preserving MATLAB parity, and map the output to the universal
`mva_decomposition` shape.
