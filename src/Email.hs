{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Email where

import Data.Aeson
import GHC.Generics

data WorkVal = WorkVal
  { workEmail :: String
  } deriving (Eq, Show, Generic)

instance ToJSON WorkVal
instance FromJSON WorkVal

data PleasureVal = PleasureVal
  { pleasureEmail :: String
  } deriving (Eq, Show, Generic)

instance ToJSON PleasureVal
instance FromJSON PleasureVal

data EmailType = Work | Pleasure deriving (Eq, Show, Generic)

instance ToJSON EmailType
instance FromJSON EmailType

data family ReduceE (e :: EmailType)

data instance (ReduceE 'Work) =
  WorkEmail WorkVal deriving (Eq, Show, Generic)
data instance (ReduceE 'Pleasure) =
  PleasureEmail PleasureVal deriving (Eq, Show, Generic)

instance FromJSON (ReduceE 'Work)
instance FromJSON (ReduceE 'Pleasure)

data Response = Response Email

data Email where
  MkEmail :: ReduceE (et :: EmailType) -> Email

importEmail
  :: forall (et :: EmailType) . (FromJSON (ReduceE et))
  => Value
  -> Either String Email
importEmail blob =
  case fromJSON blob of
    Error err -> Left err
    Success (e :: ReduceE et) -> Right $ (MkEmail e)

