{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Crypto.KDF.PBKDF as KDF
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512

main :: IO ()
main = defaultMain [
    suite
  ]

suite :: Benchmark
suite =
  bgroup "ppad-pbkdf" [
    bgroup "PBKDF-SHA256" [
      bench "derive (outlen 32)" $
        nf (KDF.derive SHA256.hmac "muh password" "muh salt" 32) 64
    ]
  , bgroup "PBKDF-SHA512" [
      bench "derive (outlen 32)" $
        nf (KDF.derive SHA512.hmac "muh password" "muh salt" 32) 64
    ]
  ]

