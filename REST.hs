{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module REST where

import Servant.Server
import Servant.API hiding (Patch)
import Servant.API.ContentTypes
import Servant.API.Generic
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (M1, Meta(MetaData), Rep, Generic)
import GHC.TypeLits (Symbol, KnownSymbol)

-- | RESTful services in Haskell might look something like the following

type Name a = GName (Rep a)

type family GName (f :: * -> *) :: Symbol where
  GName (M1 info ('MetaData name mod pack nt) f) = name

type ResourceAPI key patch contentTypes resource =
  Capture (Name key) key :> (
         ReqBody contentTypes resource :> PutNoContent
    :<|>                                  Get contentTypes resource
    :<|>                                  DeleteNoContent
    :<|> ReqBody contentTypes resource :> PostNoContent
    :<|> ReqBody contentTypes patch    :> PatchNoContent
  )

class Resource resource m where
  type Patch resource
  type Key resource
  put :: Key resource -> resource -> m NoContent
  get :: Key resource -> m resource
  delete :: Key resource -> m NoContent
  post :: Key resource -> resource -> m NoContent
  patch :: Key resource -> Patch resource -> m NoContent

resourceServer :: forall (resource :: *) (contentTypes :: [*]) (m :: * -> *). Monad m => Resource resource m => ServerT (ResourceAPI (Key resource) (Patch resource) contentTypes resource) m
resourceServer key = put @resource key :<|> get @resource key :<|> delete @resource key :<|> post @resource key :<|> patch @resource key

resourceApplication :: forall (resource :: *) (contentTypes :: [*]) (m :: * -> *).
  ( KnownSymbol (Name resource)
  , KnownSymbol (Name (Key resource))
  , Resource resource m
  , Monad m
  , FromHttpApiData (Key resource)
  , AllCTRender contentTypes resource
  , HasServer (Get contentTypes resource) '[]
  , AllMimeUnrender contentTypes resource
  , AllMimeUnrender contentTypes (Patch resource)
  ) => (forall x. m x -> Handler x) -> Application
resourceApplication handle = serve (Proxy @(ResourceAPI (Key resource) (Patch resource) contentTypes resource)) (hoistServer (Proxy @(ResourceAPI (Key resource) (Patch resource) contentTypes resource)) handle resourceServer)
