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

import Control.Monad.IO.Class
import Control.Exception.Safe
import Control.Monad.Except
import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp as Warp
import Servant

type Req  = ReqBody '[JSON] Value
type Resp = Post    '[JSON] Email

type EmailAPI =
  "api" :> "email" :>
    ("work" :> Req :> Resp :<|>
     "pleasure" :> Req :> Resp)

api :: Proxy EmailAPI
api = Proxy

server :: Server EmailAPI
server = undefined
  -- work :<|> pleasure
  -- where
  --   work     = importEmail @'Work
  --   pleasure = importEmail @'Pleasure

importEmail
  :: forall (et :: EmailType) . (FromJSON (ReduceE et))
  => Value
  -> ExceptT ServantErr IO Email
importEmail blob =
  case fromJSON blob of
    Error err -> throwString err
    Success (e :: ReduceE et) -> return $ (MkEmail e)

app :: IO Application
app = return (serve api $ server)

run :: IO ()
run = Warp.run 4000 =<< app

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


