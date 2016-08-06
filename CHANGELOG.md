# Changelog

## 0.1.3.3 - UNRELEASED

 * Determine GHC libdir and binaries correctly using stack.

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
