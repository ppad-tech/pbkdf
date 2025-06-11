{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.KDF.PBKDF as KDF
import qualified Data.Aeson as A
import qualified Data.Text.IO as TIO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Wycheproof as W

main :: IO ()
main = do
  wycheproof_hmacsha256 <- TIO.readFile "etc/pbkdf2_hmacsha256_test.json"
  wycheproof_hmacsha512 <- TIO.readFile "etc/pbkdf2_hmacsha512_test.json"
  let wycheproofs = do
        a <- A.decodeStrictText wycheproof_hmacsha256 :: Maybe W.Wycheproof
        b <- A.decodeStrictText wycheproof_hmacsha512 :: Maybe W.Wycheproof
        pure (a, b)
  case wycheproofs of
    Nothing -> error "couldn't parse wycheproof vectors"
    Just (w256, w512) -> defaultMain $ testGroup "ppad-pbkdf" [
        wycheproof_tests SHA256 w256
      , wycheproof_tests SHA512 w512
      ]

data Hash = SHA256 | SHA512
  deriving Show

wycheproof_tests :: Hash -> W.Wycheproof -> TestTree
wycheproof_tests h W.Wycheproof {..} =
  testGroup ("wycheproof vectors (pbkdf, " <> show h <> ")") $
    fmap (execute_group h) wp_testGroups

execute_group :: Hash -> W.PbkdfTestGroup -> TestTree
execute_group h W.PbkdfTestGroup {..} =
  testGroup mempty (fmap (execute h) ptg_tests)

execute :: Hash -> W.PbkdfTest -> TestTree
execute h W.PbkdfTest {..} = testCase t_msg $ do
    let pas = pt_password
        sal = pt_salt
        cow = pt_iterationCount
        siz = pt_dkLen
        pec = pt_dk
    case KDF.derive hmac pas sal cow siz of
      Nothing
        | pt_result == "invalid" -> assertBool "invalid" True
        | otherwise -> assertFailure mempty
      Just out
        | pt_result == "invalid" -> assertBool "invalid" (pec /= out)
        | otherwise -> assertEqual mempty pec out
  where
    hmac = case h of
      SHA256 -> SHA256.hmac
      SHA512 -> SHA512.hmac
    t_msg = "test " <> show pt_tcId -- XX embellish

