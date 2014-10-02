{-# LANGUAGE OverloadedStrings #-}
module Sigym4.Geometry.Persistent () where

import Data.String (fromString)
import Data.ByteString.Lazy (fromChunks, toStrict)
import Database.Persist
import Database.Persist.Postgresql
import qualified Data.ByteString.Base16 as B16
import Sigym4.Geometry
import Sigym4.Geometry.Binary
import Data.Binary (encode)

instance (VectorSpace v, KnownNat srid) => PersistField (Geometry v srid) where
    toPersistValue = PersistDbSpecific . B16.encode . toStrict
                   . encode
    fromPersistValue (PersistDbSpecific bs)
      = case B16.decode bs of
            (b, "") -> case wkbDecode (fromChunks [b]) of
                          Left e  -> Left (fromString e)
                          Right g -> Right g
            _       -> Left "fromPersistValue(Geometry v): invalid hex encoded geometry"
    fromPersistValue _ = Left "fromPersistValue(Geometry v): wrong SQL type" 

instance (VectorSpace v, KnownNat srid) => PersistFieldSql (Geometry v srid) where
    sqlType _ = SqlOther "geometry"

