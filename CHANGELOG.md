# Changelog

## 2017

 * `.hdevtoolsrc` file for project-specific configuration.
 * Use of `stack` can be turned of with `--nostack`

## 0.1.5.0 - 2016-12-23

 * (Re-)added template haskell support when required. Can be turned off using `--noTH`.
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
