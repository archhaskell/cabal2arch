cabal2arch
==========

cabal2arch is a tool used to convert CABAL ([Common Architecture for Building
Applications and Libraries][1]) files into [ArchLinux][2] source packages.

Commands
--------

cabal2arch has two subcommands:

1. `conv`

    Convert a single package description (CABAL file) into a ArchLinux source
    package.

1. `convtar`

    Given a package list and a tarball of package descriptions create tree of
    ArchLinux source packages for the listed packages.  The package list must
    contain lines of the format "<pkgname> <version>".

Example usage
-------------

Examples for `conv`:

    % cabal2arch conv puremd5.cabal
    % cabal2arch conv http://hackage.haskell.org/packages/archive/pureMD5/2.1.0.1/pureMD5.cabal

Example for `convtar`:

    % cabal2arch convtar PKGLIST 00-index.tar my-abs

Build and install
-----------------

Run the well-known triple:

    % runhaskell Setup.lhs configure
    % runhaskell Setup.lhs build
    % runhaskell Setup.lhs install

Adding CABAL options as needed.

[1]: http://www.haskell.org/ghc/docs/latest/html/Cabal/
[2]: http://www.archlinux.org/
