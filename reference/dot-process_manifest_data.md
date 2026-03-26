# Process Manifest Data

Converts raw manifest-based observation specs into numeric data
matrices. Each observation is defined by a list of NIfTI sources (lags)
which are masked and folded into a single feature axis in
MATLAB-compatible order.

## Usage

``` r
.process_manifest_data(spec)
```
