{-# LANGUAGE ConstraintKinds
           , EmptyDataDecls
           , FlexibleContexts
           , GADTs
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , OverloadedStrings
           , QuasiQuotes
           , Rank2Types
           , TemplateHaskell
           , TypeFamilies
           , ScopedTypeVariables
           , CPP
           , TypeSynonymInstances
           , DataKinds
           #-}
module Sigym4.Geometry.PersistentSpec (main, spec) where

import Prelude as P
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger(..), runStderrLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Database.Esqueleto
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.TH
import Data.Text (Text)
import Data.Maybe
import Data.Monoid
import Test.Hspec
import qualified Data.List as L

import qualified Control.Monad.Trans.Resource as R

import Sigym4.Geometry
import Sigym4.Geometry.Persistent

connString :: ConnectionString
connString = "host=localhost port=5432 user=test dbname=test password=test"

type LonLatGeometry = Geometry V2 4326
type ZLonLatGeometry = Geometry V3 4326
type UTM30Geometry = Geometry V2 23030

sameElementsAs :: Eq a => [a] -> [a] -> Bool
sameElementsAs l1 l2 = null (l1 L.\\ l2)

-- Test schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
  City
    name Text
    geometry UTM30Geometry
    deriving Eq Show
  Person
    name Text
    location ZLonLatGeometry
    deriving Eq Show
  Country
    name Text
    geometry LonLatGeometry
    deriving Eq Show
|]


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let p1  = Person "John" $ mkLocation (-3.69497018223769) 40.4667095805601 0
      p2  = Person "Rachel" $ mkLocation 2.15365369561441 41.401725851023 0
      p3  = Person "Susan" $ mkLocation (-0.30321743085458) 39.5594541204846 0
      mad = City "Madrid" $ mkPolyBox 424853 4462788 456141 4499621
      vlc = City "Valencia" $ mkPolyBox 720677 4351473 735386 4383251
      bar = City "Barcelona" $ mkPolyBox 922374 4587294 937033 4603879
      es  = Country "Spain" $ mkPolyBox (-18.15) 27.64 4.31 43.79
      fr  = Country "France" $ mkPolyBox (-63.088291499) 43.79 55.84 51.09

      insertAll :: (forall m. RunDbMonad m => SqlPersistT (R.ResourceT m) ())
      insertAll = insert_ p1 >> insert_ p2 >> insert_ p3
               >> insert_ mad >> insert_ vlc >> insert_ bar
               >> insert_ es >> insert_ fr


  describe "de/serialization from/to database" $ do
    it "works for a point" $ do
      let g = personLocation p1
      ret <- run $ select $ return $ val g
      ret `shouldBe` [ Value g ]

    it "works for a polygon" $ do
      let g = cityGeometry mad
      ret <- run $ select $ return $ val g
      ret `shouldBe` [ Value g ]

  describe "contains/force2D" $ do
    it "works for a 3d point" $ run $ do
        insertAll
        let fromCountry name =
              fmap (map entityVal) $ select $ from $ \(p,c) -> do
                where_ $ (c^.CountryGeometry) `contains`
                         force2D (p^.PersonLocation)
                where_ $ c^.CountryName ==. name
                return p
        inFrance <- fromCountry $ val "France"
        inSpain <- fromCountry $ val "Spain"
        liftIO $ do
          inFrance `shouldBe` []
          sameElementsAs inSpain [p1,p2,p3] `shouldBe` True
      
  describe "contains/transform/force2D" $ do
    it "works for a 3d point" $ run $ do
        insertAll
        let fromCity name = fmap (map entityVal) $ select $ from $ \(p,c) -> do
              where_ $ transform (c^.CityGeometry) `contains`
                       force2D (p^.PersonLocation)
              where_ $ c^.CityName ==. name
              return p
        inMadrid <- fromCity $ val "Madrid"
        inBarcelona <- fromCity $ val "Barcelona"
        inValencia <- fromCity $ val "Valencia"
        liftIO $ do
          inMadrid `shouldBe` [p1]
          inBarcelona `shouldBe` [p2]
          inValencia `shouldBe` [p3]

  describe "contains/transform/force2D with Value" $ do
    it "works for a 3d point" $ run $ do
        insertAll
        let fromCity city = fmap (map entityVal) $ select $ from $ \p -> do
              where_ $ transform (val (cityGeometry city)) `contains`
                       force2D (p^.PersonLocation)
              return p
        inMadrid <- fromCity mad
        inBarcelona <- fromCity bar
        inValencia <- fromCity vlc
        liftIO $ do
          inMadrid `shouldBe` [p1]
          inBarcelona `shouldBe` [p2]
          inValencia `shouldBe` [p3]


mkLocation :: Double -> Double -> Double -> Geometry V3 srid
mkLocation x y z = GeoPoint $ Point $ V3 x y z

mkPolyBox :: Double -> Double -> Double -> Double -> Geometry V2 srid
mkPolyBox xmin ymin xmax ymax
  = fromJust $ mkPolygon' [(xmin,ymin),(xmin,ymax),(xmax,ymax),(xmax,ymin)
                          ,(xmin,ymin)]

mkPolygon' :: [(Double,Double)] -> Maybe (Geometry V2 srid)
mkPolygon' = fmap (GeoPolygon . flip Polygon mempty)
           . mkLinearRing
           . P.map (Point . uncurry V2)

type RunDbMonad m = ( MonadBaseControl IO m, MonadIO m, MonadLogger m
                    , R.MonadThrow m )

run :: (forall m. RunDbMonad m => SqlPersistT (R.ResourceT m) a) -> IO a
run act
  = runStderrLoggingT
  . R.runResourceT
  . withPostgresqlConn connString
  . runSqlConn
  $ (initializeDB >> act >>= \ret -> transactionUndo >> return ret)


initializeDB
  :: (forall m. RunDbMonad m
  => SqlPersistT (R.ResourceT m) ())
initializeDB  = do
  rawExecute "CREATE TABLE city(id SERIAL PRIMARY KEY)" []
  rawExecute "CREATE TABLE country(id SERIAL PRIMARY KEY)" []
  rawExecute "CREATE TABLE person(id SERIAL PRIMARY KEY)" []
  (_ :: [Single PersistValue]) <- rawSql
        "SELECT AddGeometryColumn('city','geometry',23030,'POLYGON',2)" []
  (_ :: [Single PersistValue]) <- rawSql
        "SELECT AddGeometryColumn('country','geometry',4326,'POLYGON',2)" []
  (_ :: [Single PersistValue]) <- rawSql
        "SELECT AddGeometryColumn('person','geometry',4326,'POINT',3)" []
  runMigration migrateAll
