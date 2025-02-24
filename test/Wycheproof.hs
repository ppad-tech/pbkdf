{-# LANGUAGE OverloadedStrings #-}

module Wycheproof (
    Wycheproof(..)
  , PbkdfTestGroup(..)
  , PbkdfTest(..)
  ) where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)

data Wycheproof = Wycheproof {
    wp_numberOfTests :: !Int
  , wp_testGroups    :: ![PbkdfTestGroup]
  } deriving Show

instance A.FromJSON Wycheproof where
  parseJSON = A.withObject "Wycheproof" $ \m -> Wycheproof
    <$> m .: "numberOfTests"
    <*> m .: "testGroups"

data PbkdfTestGroup = PbkdfTestGroup {
    ptg_type    :: !T.Text
  , ptg_tests   :: ![PbkdfTest]
  } deriving Show

instance A.FromJSON PbkdfTestGroup where
  parseJSON = A.withObject "PbkdfTestGroup" $ \m -> PbkdfTestGroup
    <$> m .: "type"
    <*> m .: "tests"

data PbkdfTest = PbkdfTest {
    pt_tcId           :: !Int
  , pt_comment        :: !T.Text
  , pt_password       :: !BS.ByteString
  , pt_salt           :: !BS.ByteString
  , pt_iterationCount :: !Word64
  , pt_dkLen          :: !Word32
  , pt_dk             :: !BS.ByteString
  , pt_result         :: !T.Text
  } deriving Show

decodehex :: T.Text -> BS.ByteString
decodehex t = case B16.decode (TE.encodeUtf8 t) of
  Nothing -> error "bang"
  Just bs -> bs

instance A.FromJSON PbkdfTest where
  parseJSON = A.withObject "PbkdfTest" $ \m -> PbkdfTest
    <$> m .: "tcId"
    <*> m .: "comment"
    <*> fmap decodehex (m .: "password")
    <*> fmap decodehex (m .: "salt")
    <*> m .: "iterationCount"
    <*> m .: "dkLen"
    <*> fmap decodehex (m .: "dk")
    <*> m .: "result"


