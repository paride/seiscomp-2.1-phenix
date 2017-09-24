# SeisComP 2.1-phenix

## Introduction

SeisComP 2.1 the the latest version of SeisComP coming with the
`comserv` plugin needed to acquire data from the old Quanterra
Q4120 digitizers.

This version has been modified to allow its compilation on modern
GNU/Linux systems. For a list of the changes check the git log.

The upstream source code is publicly distributed by GFZ
and available at:

[ftp://ftp.gfz-potsdam.de/home/st/GEOFON/software/SeisComP/2.5/](ftp://ftp.gfz-potsdam.de/home/st/GEOFON/software/SeisComP/)

## Compilation

In September 2017
the build process has been tested on a 64-bit Debian GNU/Linux
system following the *sid* branch. At the time of the test the
system came with gcc 7.2.

Beside the `build-essential` tools a few packages are needed:

- `libxml2-dev`
- `libplot-dev`
- `libgd-dev`
- `libf2c2-dev`
- `flex`

In order to build the pdf manual (disabled by default) `texlive` is also
needed. To build this software simply run `make` in the `src` directory.

### Cleaning up

Run `make veryclean` in `src/`.

## Notable changes

Two plugins have been removed:

- The Guralp plugin, as it comes with an x86 binary library. Source code is
available on request, but its redistribution is explicitly forbidden.
- The `fs` plugin, used to feed data from files in Titan, Seisan or Mini-SEED format
(broken, especially the Titan part). This is disabled in SeisComP 2.5.

## Freedom status

SeisComP 2.1 is not licensed in a clear way, so it has
to be considered **non-free**.
An ambiguous and clearly non-free
[SeisComP Public License](http://geofon.gfz-potsdam.de/software/seiscomp/license.pdf)
does exist, but seems to apply only to the newer SeisComP3. 
