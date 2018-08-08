# Changelog

## 0.1.7.0 - 2018-08-08

 * Add support for ghc-8.4 and Cabal-2.2.

## 0.1.6.1 - 2017-12-17

 * Fixed `moduleinfo` command to load targets correctly.
 * Print version on `-v` command line option.
 * Fixed build with ghc-8.2.0.

## 0.1.6.0 - 2017-08-21

 * Added handling of source errors: GHC `SourceError` and other exceptions are
   now correctly sent to the frontend process. This enables `hdevtools` to
   correctly report haskell syntax errors and improves visibility of exceptions
   leading the backend process to die.
 * `.hdevtoolsrc` file for project-specific configuration.
 * Use of `stack` can be turned off with `--nostack`.

## 0.1.5.0 - 2016-12-23

 * (Re-)added template haskell support when required. Can be turned off using `--noth`.
 * Fixed system installed GHC libdir paths using stack.

## 0.1.4.1 - 2016-09-04

 * Do not try to execute `stack` commands if not available.

## 0.1.4.0 - 2016-08-08

 * Determine GHC libdir and binaries using stack.

## 0.1.3.2 - 2016-06-09

 * Added GHC 8.0.x support

## 0.1.3.1 - 2016-05-13

 * Added support for new Cabal versions
 * Do not generate code, fixing inline-c modules typechecking

## 0.1.3.0 - 2016-02-29

 * Improved performance in stack projects: The stack configuration is
   updated only when the passed path implies a different `stack.yaml`.
   Performance improved substantially from roughly ~1s to ~0.3s.

## 0.1.2.2 - 2016-01-11

 * Added type checking support for tests and benchmarks in stack projects.
