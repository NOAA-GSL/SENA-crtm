[![CI for crtm](https://github.com/NOAA-GSL/SENA-crtm/actions/workflows/ci.yml/badge.svg)](https://github.com/NOAA-GSL/SENA-crtm/actions/workflows/ci.yml)

```
This repository is a scientific product and is not official communication
of the National Oceanic and Atmospheric Administration, or the United States
Department of Commerce. All NOAA GitHub project code is provided on an ‘as
is’ basis and the user assumes responsibility for its use. Any claims against
the Department of Commerce or Department of Commerce bureaus stemming from
the use of this GitHub project will be governed by all applicable Federal
law. Any reference to specific commercial products, processes, or service
by service mark, trademark, manufacturer, or otherwise, does not constitute
or imply their endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and logo of
a DOC bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.
```

# Overview

This repository contains a stand-alone kernel for the `CRTM_Doubling_layer_AD`
subroutine, extracted from the Community Radiative Transfer Model (CRTM). The 
goal of this repository is to facilitate direct comparisons of different
implementations of `CRTM_Doubling_layer_AD`. Alternative implementations may
use different programming languages, parallel programming models, or numerical
modeling libraries. They may also target different hardware such as GPUs or
ARM processors. In numerical modeling, metrics for comparison traditionally
include performance and portability. However, this work encourages comparison
of developer productivity and design metrics such as ease of use, extensibility,
and maintainability.

A Fortran 90 implementation of `CRTM_Doubling_layer_AD` is provided as a baseline
reference to which other implementations should be compared. Additionally, two
GPU versions are provided using OpenACC directives.  These GPU versions do not
alter the original code structure.  Additional implementations will be added as
they are developed.

# Contents

This repository is organized as follows.

### `test/`

Contains the reference test baselines all implementations must use for testing
and validation.

### `ref/`

Contains the source tree for the reference implementation. See the README in that
directory for details about how to run and test it.

### Contributing

Please see the [Contributing Guide](CONTRIBUTING.md) for information about
contributing to this repository.
