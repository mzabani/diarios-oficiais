{-# LANGUAGE RankNTypes, FlexibleContexts, PartialTypeSignatures, ScopedTypeVariables #-}
module BeamUtils (beamInsertOrThrow, beamInsertReturningOrThrow, beamInsertOnNoConflict, beamInsertOrGet, beamInsertOrGet_, beamDelete, withDbConnection, withDbTransaction, withDbTransaction_, RowOperation(..), MonadBeamPostgres) where

import Data.Conduit
import Database.Beam
import Control.Monad.Fail (MonadFail)
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres.Conduit as Pg
import Database.Beam.Backend.SQL.BeamExtensions
import Control.Monad.Trans.Control
import qualified Database.PostgreSQL.Simple as PGS
import Data.Pool
import Database.PostgreSQL.Simple

withDbConnection :: MonadBaseControl IO m => Pool Connection -> (Connection -> m a) -> m a
withDbConnection = withResource

-- | Runs a function inside a transaction that has begun. If an exception is thrown the transaction is rolled back;
-- it is committed otherwise.
withDbTransaction :: MonadBaseControl IO m => Connection -> m a -> m a
withDbTransaction conn f = control $ \runInBase -> withTransaction conn (runInBase f)

-- | Runs a function inside a transaction that has begun. If an exception is thrown the transaction is rolled back;
-- it is committed otherwise.
withDbTransaction_ :: MonadBaseControl IO m => Connection -> m a -> m ()
withDbTransaction_ conn f = withDbTransaction conn f >> return ()

conduitRight :: Monad m => ConduitM () b m () -> m (Maybe b)
conduitRight c = runConduit (c .| await)

type PgBeam table m db = (FromBackendRow Pg.Postgres (table Identity), Database Pg.Postgres db, Beamable table, MonadIO m)
type MonadBeamPostgres m = MonadBeam Pg.Postgres m

beamInsertOrThrow :: PgBeam table m db =>
    PGS.Connection
    -> DatabaseEntity Pg.Postgres db (TableEntity table)
    -> (forall s'. table (QExpr Pg.Postgres s'))
    -> m ()
beamInsertOrThrow conn tbl val = beamInsertReturningOrThrow conn tbl val >> return ()

beamInsertReturningOrThrow :: PgBeam table m db =>
    PGS.Connection
    -> DatabaseEntity Pg.Postgres db (TableEntity table)
    -> (forall s'. table (QExpr Pg.Postgres s'))
    -> m (table Identity)
beamInsertReturningOrThrow conn tbl val =
    liftIO $ Pg.runBeamPostgres conn $ do
        retList <- runInsertReturningList (insert tbl (insertExpressions [ val ]))
        case retList of
            [ret] -> return ret
            _     -> error "insertReturning returned a list that is not an one-element list"
    -- retMaybe <- Pg.runInsertReturning conn (Pg.insertReturning tbl (insertExpressions [val]) Pg.onConflictDefault (Just id)) conduitRight
    -- case retMaybe of
    --     Nothing  -> error "insertReturning returned Nothing.. impossible (it should throw on conflict or return a newly inserted row)!"
    --     Just ret -> return ret

beamInsertOnNoConflict :: (FromBackendRow Pg.Postgres (table Identity), Beamable table, Monad m, MonadIO m, MonadBaseControl IO m, MonadFail m) =>
    PGS.Connection
    -> DatabaseEntity Pg.Postgres db (TableEntity table)
    -> (forall s'. table (QExpr Pg.Postgres s'))
    -> m ()
beamInsertOnNoConflict conn tbl val = do
    _ <- Pg.runInsertReturning conn (Pg.insertReturning tbl (insertExpressions [val]) (Pg.onConflict Pg.anyConflict Pg.onConflictDoNothing) (Just id)) conduitRight
    return ()

-- beamInsertOrGet2 :: (HasSqlEqualityCheck Pg.PgExpressionSyntax proj, FromBackendRow Pg.Postgres (table Identity), Database Pg.Postgres be, Table table, Beamable table, Monad m, MonadIO m, MonadBaseControl IO m) =>
--     PGS.Connection
--     -> DatabaseEntity Pg.Postgres be (TableEntity table)
--     -> (forall s'. table (QExpr Pg.PgExpressionSyntax s'))
--     -> (_ -> QExpr Pg.PgExpressionSyntax s proj)
--     -> m (table Identity)
-- beamInsertOrGet2 conn tbl val confFields = beamInsertOrGet conn tbl val (\t -> confFields t ==. confFields val)

data RowOperation = RowExisted | RowInserted

-- | Returns the first row that passes the supplied condition if it exists or inserts the new object and returns it while also returning if the row returned already existed or if it's recently inserted.
beamInsertOrGet :: PgBeam table m db =>
    PGS.Connection
    -> DatabaseEntity Pg.Postgres db (TableEntity table)
    -> (forall s'. table (QExpr Pg.Postgres s'))
    -> (table (QExpr Pg.Postgres QBaseScope) -> QExpr Pg.Postgres QBaseScope Bool)
    -> m (table Identity, RowOperation)
beamInsertOrGet conn tbl val confFields = do
    retMaybe <- liftIO $ Pg.runBeamPostgres conn $ runSelectReturningOne $ select $ filter_ confFields $ all_ tbl
    case retMaybe of
        Just ret -> return (ret, RowExisted)
        Nothing  -> do
            row <- beamInsertReturningOrThrow conn tbl val
            return (row, RowInserted)

beamInsertOrGet_ :: PgBeam table m db =>
    PGS.Connection
    -> DatabaseEntity Pg.Postgres db (TableEntity table)
    -> (forall s'. table (QExpr Pg.Postgres s'))
    -> (table (QExpr Pg.Postgres QBaseScope) -> QExpr Pg.Postgres QBaseScope Bool)
    -> m (table Identity)
beamInsertOrGet_ conn tbl val confFields = fst <$> beamInsertOrGet conn tbl val confFields

-- | Apenas uma simplificação para deletar mais facilmente
beamDelete :: PgBeam table m db =>
    PGS.Connection
    -> DatabaseEntity Pg.Postgres db (TableEntity table)
    -> (forall s. (forall s'. table (QExpr Pg.Postgres s')) -> QExpr Pg.Postgres s Bool)
    -> m ()
beamDelete conn tbl whereCond = liftIO . Pg.runBeamPostgres conn . runDelete $ delete tbl whereCond