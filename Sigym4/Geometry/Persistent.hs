{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , InstanceSigs
           #-}
module Sigym4.Geometry.Persistent (GeoEsqueleto(..)) where

import Data.String (fromString)
import Data.Proxy (Proxy(..))
import Data.ByteString.Lazy (fromChunks, toStrict)
import Database.Persist
import Database.Persist.Postgresql
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql
import qualified Data.ByteString.Base16 as B16
import Sigym4.Geometry
import Sigym4.Geometry.Binary
import Data.Binary (encode)

instance (VectorSpace v, KnownNat srid) => PersistField (Geometry v srid) where
    toPersistValue = PersistDbSpecific . B16.encode . toStrict
                   . encode
    fromPersistValue (PersistDbSpecific bs) = fromHexEWKB bs
    fromPersistValue (PersistByteString bs) = fromHexEWKB bs
    fromPersistValue _ = Left "fromPersistValue(Geometry v): wrong SQL type:"

fromHexEWKB bs
  = case B16.decode bs of
        (b, "") -> case wkbDecode (fromChunks [b]) of
                      Left e  -> Left (fromString e)
                      Right g -> Right g
        _       -> Left "fromHexEWKB: invalid hex encoded geometry"

instance (VectorSpace v, KnownNat srid) => PersistFieldSql (Geometry v srid) where
    sqlType _ = SqlOther "geometry"

class Esqueleto query expr backend => GeoEsqueleto query expr backend where
  -- predicates
  fullyWithin :: (VectorSpace v, KnownNat srid)
              => expr (Value (Geometry v srid))
              -> expr (Value (Geometry v srid))
              -> expr (Value Bool)

  within :: (VectorSpace v, KnownNat srid)
           => expr (Value (Geometry v srid))
           -> expr (Value (Geometry v srid))
           -> expr (Value Bool)

  touches :: (VectorSpace v, KnownNat srid)
             => expr (Value (Geometry v srid))
             -> expr (Value (Geometry v srid))
             -> expr (Value Bool)

  intersects :: (VectorSpace v, KnownNat srid)
             => expr (Value (Geometry v srid))
             -> expr (Value (Geometry v srid))
             -> expr (Value Bool)

  disjoint :: (VectorSpace v, KnownNat srid)
           => expr (Value (Geometry v srid))
           -> expr (Value (Geometry v srid))
           -> expr (Value Bool)

  equals :: (VectorSpace v, KnownNat srid)
         => expr (Value (Geometry v srid))
         -> expr (Value (Geometry v srid))
         -> expr (Value Bool)

  contains :: (VectorSpace v, KnownNat srid)
           => expr (Value (Geometry v srid))
           -> expr (Value (Geometry v srid))
           -> expr (Value Bool)

  overlaps :: (VectorSpace v, KnownNat srid)
           => expr (Value (Geometry v srid))
           -> expr (Value (Geometry v srid))
           -> expr (Value Bool)

  covers :: (VectorSpace v, KnownNat srid)
         => expr (Value (Geometry v srid))
         -> expr (Value (Geometry v srid))
         -> expr (Value Bool)

  containsProperly :: (VectorSpace v, KnownNat srid)
             => expr (Value (Geometry v srid))
             -> expr (Value (Geometry v srid))
             -> expr (Value Bool)


  closestPoint :: (VectorSpace v, KnownNat srid)
             => expr (Value (Geometry v srid))
             -> expr (Value (Geometry v srid))
             -> expr (Value (Geometry v srid))


  centroid :: (VectorSpace v, KnownNat srid)
           => expr (Value (Geometry v srid))
           -> expr (Value (Geometry v srid))

  transform :: (VectorSpace v, KnownNat srid, KnownNat srid2)
           => expr (Value (Geometry v srid))
           -> expr (Value (Geometry v srid2))

  force2D :: (VectorSpace v, KnownNat srid)
          => expr (Value (Geometry v srid))
          -> expr (Value (Geometry V2 srid))

  force3D :: (VectorSpace v, KnownNat srid)
          => expr (Value (Geometry v srid))
          -> expr (Value (Geometry V3 srid))

  distance :: (VectorSpace v, KnownNat srid)
           => expr (Value (Geometry v srid))
           -> expr (Value (Geometry v srid))
           -> expr (Value Double)

  area :: (VectorSpace v, KnownNat srid)
       => expr (Value (Geometry v srid))
       -> expr (Value Double)

    
instance GeoEsqueleto SqlQuery SqlExpr SqlBackend where
    fullyWithin :: forall v srid. (VectorSpace v, KnownNat srid)
               => SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value Bool)
    fullyWithin a b = unsafeSqlFunction func (a, b)
      where func = func2d3d (Proxy :: Proxy v) "ST_FullyWithin" "ST_3DFullyWithin"

    within :: forall v srid. (VectorSpace v, KnownNat srid)
               => SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value Bool)
    within a b = unsafeSqlFunction func (a, b)
      where func = func2d3d (Proxy :: Proxy v) "ST_Within" "ST_3DWithin"

    equals a b = unsafeSqlFunction "ST_Equals" (a, b)
    disjoint a b = unsafeSqlFunction "ST_Disjoint" (a, b)
    touches a b = unsafeSqlFunction "ST_Touches" (a, b)
    overlaps a b = unsafeSqlFunction "ST_Overlaps" (a, b)
    covers a b = unsafeSqlFunction "ST_Covers" (a, b)
    contains a b = unsafeSqlFunction "ST_Contains" (a, b)
    containsProperly a b = unsafeSqlFunction "ST_ContainsProperly" (a, b)

    intersects :: forall v srid. (VectorSpace v, KnownNat srid)
               => SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value Bool)
    intersects a b = unsafeSqlFunction func (a, b)
      where func = func2d3d (Proxy :: Proxy v) "ST_Intersects" "ST_3DIntersects"
              
    distance :: forall v srid. (VectorSpace v, KnownNat srid)
               => SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value Double)
    distance a b = unsafeSqlFunction func (a, b)
      where func = func2d3d (Proxy :: Proxy v) "ST_Distance" "ST_3DDistance"

    area = unsafeSqlFunction "ST_Area"

    centroid = unsafeSqlFunction "ST_Centroid"

    closestPoint :: forall v srid. (VectorSpace v, KnownNat srid)
               => SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value (Geometry v srid))
               -> SqlExpr (Value (Geometry v srid))
    closestPoint a b = unsafeSqlFunction func (a, b)
      where func = func2d3d (Proxy :: Proxy v) "ST_ClosestPoint" "ST_3DClosestPoint"

    -- FIXME: Use CPP to vary on postgis version
    force2D = unsafeSqlFunction "ST_Force_2D"
    force3D = unsafeSqlFunction "ST_Force_3D"

    transform :: forall v srid srid2.
                 (VectorSpace v, KnownNat srid, KnownNat srid2)
              => SqlExpr (Value (Geometry v srid))
              -> SqlExpr (Value (Geometry v srid2))
    transform g = unsafeSqlFunction "ST_Transform"
                  (ensureGeom g, val srid :: SqlExpr (Value Int))
        where srid = fromIntegral $ gSrid $ (undefined :: Geometry v srid2)

ensureGeom :: SqlExpr (Value (Geometry v srid))
           -> SqlExpr (Value (Geometry v srid))
ensureGeom = unsafeSqlFunction "geometry"

func2d3d p a b
  = case dim p of
      2 -> a
      3 -> b
      _ -> error "Sigym4/Geometry/Persistent.2d3d: only V2 and V3 are supported" 
