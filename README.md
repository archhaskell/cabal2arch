*This tool is no longer maintained*

Nowadays we use [cblrepo](https://github.com/magthe/cblrepo) to maintain the haskell packages in HABS.

Please let us know if you would have use of `cabal2arch` and want to take over maintainership.

cabal2arch
==========

cabal2arch is a tool used to convert CABAL ([Common Architecture for
Building Applications and Libraries][1]) files into [ArchLinux][2]
source packages.

Usage
-----

As its only argument, cabal2arch expects a file path, directory path, or
URL to the Cabal description of the package that should be converted.
For example:

    % cabal2arch puremd5.cabal
    % cabal2arch http://hackage.haskell.org/packages/archive/pureMD5/2.1.0.1/pureMD5.cabal

Build and install
-----------------

Run the well-known triple:

    % runhaskell Setup.lhs configure
    % runhaskell Setup.lhs build
    % runhaskell Setup.lhs install

[1]: http://www.haskell.org/ghc/docs/latest/html/Cabal/
[2]: http://www.archlinux.org/
