[![build-status-badge]][build-status]

# PyD

PyD provides seamless interoperability between Python and the D programming language.

This repo is a fork with the aim of massively cleaning up PyD and bringing it up to
speed with modern D.

So far it's mostly been a job of deleting old things and revamping the build system.

# Requirements

## Python

CPython 2.7+

## D Compilers

Currently tested and working on DMD 2.071.0 and LDC 0.17.1, others may work too, don't know.
Using LDC requires dub 0.9.25 (currently available: 0.9.25-rc.1)

[build-status-badge]: https://travis-ci.org/John-Colvin/pyd.svg?branch=master
[build-status]: https://travis-ci.org/John-Colvin/pyd
