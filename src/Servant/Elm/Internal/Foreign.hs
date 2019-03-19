{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Servant.Elm.Internal.Foreign where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Typeable
import           Elm.TyRep
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor)

data LangElm

instance Typeable a => HasForeignType LangElm EType a where
  typeFor _ _ _ =
    toElmType (Proxy :: Proxy a)

getEndpoints
  :: ( HasForeign LangElm EType api
     , GenerateList EType (Foreign EType api))
  => Proxy api
  -> [Req EType]
getEndpoints =
  listFromAPI (Proxy :: Proxy LangElm) (Proxy :: Proxy EType)
