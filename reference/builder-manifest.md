# Builder API: Add Subjects from Manifest / GDS

Helpers for constructing PLS-ready data matrices from higher-level
inputs. In particular, this supports event-related fMRI style inputs
where the 4th dimension of a NIfTI encodes **lags** (time since onset),
and the PLS datamat columns are constructed by folding voxel × lag into
a single feature axis (MATLAB-compatible ordering).
