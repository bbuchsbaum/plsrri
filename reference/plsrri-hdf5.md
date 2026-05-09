# HDF5 I/O for plsrri results

Native HDF5 reader/writer for `pls_result` objects. Provides lossless
round-trip for all six PLS methods (including multiblock and non-rotated
variants) along with optional `perm_result`, `boot_result`,
`splithalf_result`, NeuroVol masks, and site/diagnostics metadata.

The schema (root attribute `format_version = "plsrri/1.0"`) groups
fields by category — `/decomposition`, `/scores`, `/correlations`,
`/multiblock`, `/perm_result`, `/boot_result`, `/splithalf_result`,
`/inputs`, `/metadata` — and uses gzip+shuffle chunking for large
arrays. Datasets carry self-describing `dims` and `description`
attributes so the file is inspectable in any HDF5 tool (HDFView, h5dump,
h5py).

Requires the `hdf5r` package (Suggests).
