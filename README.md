cabal2arch
==========

cabal2arch is a tool used to convert CABAL ([Common Architecture for Building
Applications and Libraries][1]) files into [ArchLinux][2] source packages.

Example usage
-------------

It takes a single argument, it can be either a file, a directory (containing a
single CABAL file), or the URL of a CABAL file.

    % cabal2arch puremd5.cabal
    % cabal2arch http://hackage.haskell.org/packages/archive/pureMD5/2.1.0.1/pureMD5.cabal

Build and install
-----------------

Run the well-known triple:

    % runhaskell Setup.lhs configure
    % runhaskell Setup.lhs build
    % runhaskell Setup.lhs install

Adding CABAL options as needed.

[1]: http://www.haskell.org/ghc/docs/latest/html/Cabal/
[2]: http://www.archlinux.org/
