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
  time                 216.9 μs   (216.3 μs .. 217.3 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 215.7 μs   (215.3 μs .. 216.1 μs)
  std dev              1.446 μs   (1.275 μs .. 1.819 μs)

  benchmarking ppad-pbkdf/PBKDF-SHA512/derive (outlen 32)
  time                 113.5 μs   (113.3 μs .. 113.6 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 113.4 μs   (113.3 μs .. 113.5 μs)
  std dev              303.1 ns   (256.5 ns .. 398.4 ns)
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
