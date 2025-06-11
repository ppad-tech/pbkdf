# pbkdf

[![](https://img.shields.io/hackage/v/ppad-pbkdf?color=blue)](https://hackage.haskell.org/package/ppad-pbkdf)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-pbkdf-lightblue)](https://docs.ppad.tech/pbkdf)

A password-based key derivation function (PBKDF2) per
[RFC2898](https://datatracker.ietf.org/doc/html/rfc2898).

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  > -- import qualified
  > import qualified Crypto.KDF.PBKDF as KDF
  >
  > -- supply your own HMAC function
  > import qualified Crypto.Hash.SHA256 as SHA256
  >
  > -- derive a 32-byte key from a secret
  > KDF.derive SHA256.hmac "my password" "my salt" 100 32
  Just "\"\NAKqxp\165S\t\212i\139\SUB(\132\176\204\224<\164\177\144\&1D\209\175\145\139[K\159h\205"
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/pbkdf][hadoc].

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code.

Current benchmark figures on my mid-2020 MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-pbkdf/PBKDF-SHA256/derive (outlen 32)
  time                 533.2 μs   (490.9 μs .. 579.9 μs)
                       0.967 R²   (0.941 R² .. 0.993 R²)
  mean                 494.0 μs   (480.3 μs .. 518.3 μs)
  std dev              57.73 μs   (38.72 μs .. 98.81 μs)
  variance introduced by outliers: 81% (severely inflated)

  benchmarking ppad-pbkdf/PBKDF-SHA512/derive (outlen 32)
  time                 241.2 μs   (233.4 μs .. 249.6 μs)
                       0.991 R²   (0.987 R² .. 0.995 R²)
  mean                 233.6 μs   (227.8 μs .. 240.1 μs)
  std dev              20.22 μs   (16.95 μs .. 24.39 μs)
  variance introduced by outliers: 74% (severely inflated)
```

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be [challenging to achieve][const].

The PBKDF implementation within has been tested against the [Project
Wycheproof vectors][wyche] available for SHA-256 and SHA-512, using
the HMAC functions from [ppad-sha256][sh256] and [ppad-sha512][sh512]
respectively.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-pbkdf
```

to get a REPL for the main library.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[hadoc]: https://docs.ppad.tech/pbkdf
[sh256]: https://git.ppad.tech/sha256
[sh512]: https://git.ppad.tech/sha512
[const]: https://www.chosenplaintext.ca/articles/beginners-guide-constant-time-cryptography.html
[wyche]: https://github.com/C2SP/wycheproof
