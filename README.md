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

Current benchmark figures on an M4 Silicon MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-pbkdf/PBKDF-SHA256/derive (outlen 32)
  time                 58.19 μs   (58.14 μs .. 58.25 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 58.39 μs   (58.35 μs .. 58.44 μs)
  std dev              155.6 ns   (130.8 ns .. 193.7 ns)

  benchmarking ppad-pbkdf/PBKDF-SHA512/derive (outlen 32)
  time                 44.01 μs   (43.92 μs .. 44.14 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 44.09 μs   (44.02 μs .. 44.19 μs)
  std dev              270.4 ns   (209.1 ns .. 345.1 ns)
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
