{-# LANGUAGE PartialTypeSignatures #-}
module Model.Diarios where

import RIO
import Data.Time
import Database.Beam
import qualified Database.Beam.Postgres as Pg
import qualified Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.FromField (FromField(..))

data DiariosDb f = DiariosDb { origensDiarios :: f (TableEntity OrigemDiarioT), diarios :: f (TableEntity DiarioT), diariosABaixar :: f (TableEntity DiarioABaixarT), conteudosDiarios :: f (TableEntity ConteudoDiarioT), diariosABaixarToConteudosDiarios :: f (TableEntity DiarioABaixarToConteudoDiarioT), paragrafosDiarios :: f (TableEntity ParagrafoDiarioT), statusDownloads :: f (TableEntity StatusDownloadDiarioT), downloadsTerminados :: f (TableEntity DownloadTerminadoT), nomesEncontrados :: f (TableEntity NomeEncontradoT), tokensTextoTbl :: f (TableEntity TokenTextoT) } deriving Generic
instance Database be DiariosDb
diariosDb :: DatabaseSettings be DiariosDb
diariosDb = defaultDbSettings `withDbModification`
            dbModification {
                origensDiarios  = modifyEntityName (const "origemdiario") <> modifyTableFields tableModification,
                diarios         = modifyEntityName (const "diario") <> modifyTableFields tableModification { diarioOrigemDiarioId = OrigemDiarioId "origemdiarioid" },
                statusDownloads = modifyEntityName (const "statusdownloaddiario") <> modifyTableFields tableModification { statusdownloaddiarioDiarioABaixarId = DiarioABaixarId "diarioabaixarid", statusdownloaddiarioInicioDownload = "iniciodownload" },
                diariosABaixar = modifyEntityName (const "diarioabaixar") <> modifyTableFields tableModification { diarioabaixarDiarioId = DiarioId "diarioid", diarioabaixarInicioDownload = "iniciodownload" },
                conteudosDiarios = modifyEntityName (const "conteudodiario") <> modifyTableFields tableModification { conteudodiarioMd5Sum = "md5sum" },
                downloadsTerminados = modifyEntityName (const "downloadterminado") <> modifyTableFields tableModification { downloadterminadoStatusDownloadDiarioId = StatusDownloadDiarioId "statusdownloaddiarioid", downloadterminadoMomentoTermino = "momentotermino", downloadterminadoMd5Sum = "md5sum", downloadterminadoFilePath = "filepath" },
                diariosABaixarToConteudosDiarios = modifyEntityName (const "diarioabaixartoconteudodiario") <> modifyTableFields tableModification { diarioabaixartoconteudodiarioDiarioABaixarId = DiarioABaixarId "diarioabaixarid", diarioabaixartoconteudodiarioConteudoDiarioId = ConteudoDiarioId "conteudodiarioid" },
                paragrafosDiarios = modifyEntityName (const "paragrafodiario") <> modifyTableFields tableModification { paragrafodiarioConteudoDiarioId = ConteudoDiarioId "conteudodiarioid", paragrafodiarioConteudo = "conteudo" },
                nomesEncontrados = modifyEntityName (const "nomeencontrado") <> modifyTableFields tableModification { nomeencontradoConteudoDiarioId = ConteudoDiarioId "conteudodiarioid", nomeencontradoLoweredNome = "lowerednome", nomeencontradoPosicao = "posicao" },
                tokensTextoTbl = modifyEntityName (const "tokens_texto") <> modifyTableFields tableModification { tokentextoConteudoDiarioId = ConteudoDiarioId "conteudo_diario_id" }
            }

data OrigemDiarioT f = OrigemDiario {
    origemdiarioId :: C f Int,
    origemdiarioNomeCompleto :: C f (Maybe Text),
    origemdiarioCidade :: C f (Maybe Text),
    origemdiarioEstado :: C f (Maybe Text)
} deriving Generic
type OrigemDiario = OrigemDiarioT Identity
type OrigemDiarioId = PrimaryKey OrigemDiarioT Identity
deriving instance Eq OrigemDiarioId
instance FromField (PrimaryKey OrigemDiarioT Identity) where
    fromField field bs = OrigemDiarioId <$> fromField @Int field bs
instance Beamable OrigemDiarioT
instance Table OrigemDiarioT where
    data PrimaryKey OrigemDiarioT f = OrigemDiarioId (Columnar f Int) deriving Generic
    primaryKey = OrigemDiarioId . origemdiarioId
instance Beamable (PrimaryKey OrigemDiarioT)

data DiarioT f = Diario {
    diarioId :: C f Int,
    diarioOrigemDiarioId :: PrimaryKey OrigemDiarioT f,
    -- ^ Um nome humano para o diário, de forma que facilite a identificação deste se todos os diários de uma mesma data fossem listados.
    diarioNome :: C f Text,
    -- ^ A data deste diário, que junto com o nome e a origem o identifica unicamente
    diarioData :: C f Day
} deriving Generic
type Diario = DiarioT Identity
type DiarioId = PrimaryKey DiarioT Identity
instance Beamable DiarioT
instance Table DiarioT where
    data PrimaryKey DiarioT f = DiarioId (Columnar f Int) deriving Generic
    primaryKey = DiarioId . diarioId
instance Beamable (PrimaryKey DiarioT)

getDiarioNaData :: MonadIO m => OrigemDiarioId -> Day -> PGS.Connection -> m (Maybe Diario)
getDiarioNaData oid dataDiario conn = liftIO $ Pg.runBeamPostgres conn $ runSelectReturningOne $ select $ do
    diario <- all_ (diarios diariosDb)
    guard_ $ diarioOrigemDiarioId diario ==. val_ oid &&. diarioData diario ==. val_ dataDiario
    return diario

diarioPossuiParagrafos :: MonadIO m => ConteudoDiarioId -> PGS.Connection -> m Bool
diarioPossuiParagrafos cdid conn = liftIO $ do
    oneIfExists <- Pg.runBeamPostgres conn $ runSelectReturningOne $ select $ do
        parag <- all_ (paragrafosDiarios diariosDb)
        guard_ $ paragrafodiarioConteudoDiarioId parag ==. val_ cdid
        return $ pk parag
    return $ isJust oneIfExists

-- getDatasSemDiarios :: MonadIO m => Day -> Day -> PGS.Connection -> m [(Day, OrigemDiarioId)]
-- getDatasSemDiarios dt1 dt2 conn = liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select $ do
--     diario <- all_ (diarios diariosDb)
--     dbaixar <- oneToMany_ (diariosABaixar diariosDb) diarioabaixarDiarioId diario
--     statdwn <- oneToMany_ (statusDownloads diariosDb) statusdownloaddiarioDiarioABaixarId dbaixar
--     _ <- oneToMany_ (downloadsTerminados diariosDb) downloadterminadoStatusDownloadDiarioId statdwn
--     dt <- values_ [min dt1 dt2 .. max dt1 dt2] -- TODO: generate_series.. with todasDatas (data) as (select #{min} + (n || ' days')::interval from generate_series(0, #{diffDays ate de}) n)
--     -- guard_' $  just_ dt ==?. diarioData diario
--     pure (diarioData diario, diarioOrigemDiarioId diario)

data DiarioABaixarT f = DiarioABaixar {
    diarioabaixarId :: C f Int,
    diarioabaixarDiarioId :: PrimaryKey DiarioT f,
    diarioabaixarInicioDownload :: C f UTCTime
} deriving Generic
type DiarioABaixar = DiarioABaixarT Identity
type DiarioABaixarId = PrimaryKey DiarioABaixarT Identity
instance Beamable DiarioABaixarT
instance Table DiarioABaixarT where
    data PrimaryKey DiarioABaixarT f = DiarioABaixarId (C f Int) deriving Generic
    primaryKey = DiarioABaixarId . diarioabaixarId
instance Beamable (PrimaryKey DiarioABaixarT)

data ConteudoDiarioT f = ConteudoDiario {
    conteudodiarioId :: C f Int,
    conteudodiarioMd5Sum :: C f Text,
    conteudodiarioDiarioExiste :: C f Bool
    -- ^ Indica se o Diário existe de fato ou se este é um "Diário vazio",
    --   utilizado para representar a ausência de Diário Oficial.
} deriving Generic
type ConteudoDiario = ConteudoDiarioT Identity
type ConteudoDiarioId = PrimaryKey ConteudoDiarioT Identity
instance Beamable ConteudoDiarioT
instance Table ConteudoDiarioT where
    data PrimaryKey ConteudoDiarioT f = ConteudoDiarioId (C f Int) deriving Generic
    primaryKey = ConteudoDiarioId . conteudodiarioId
instance Beamable (PrimaryKey ConteudoDiarioT)

data ParagrafoDiarioT f = ParagrafoDiario {
    paragrafodiarioId :: C f Int,
    paragrafodiarioConteudoDiarioId :: PrimaryKey ConteudoDiarioT f,
    paragrafodiarioOrdem :: C f Int,
    paragrafodiarioConteudo :: C f Text
} deriving Generic
type ParagrafoDiario = ParagrafoDiarioT Identity
type ParagrafoDiarioId = PrimaryKey ParagrafoDiarioT Identity
instance Beamable ParagrafoDiarioT
instance Table ParagrafoDiarioT where
    data PrimaryKey ParagrafoDiarioT f = ParagrafoDiarioId (C f Int) deriving Generic
    primaryKey = ParagrafoDiarioId . paragrafodiarioId
instance Beamable (PrimaryKey ParagrafoDiarioT)

data DiarioABaixarToConteudoDiarioT f = DiarioABaixarToConteudoDiario {
    diarioabaixartoconteudodiarioId :: C f Int,
    diarioabaixartoconteudodiarioDiarioABaixarId :: PrimaryKey DiarioABaixarT f,
    diarioabaixartoconteudodiarioConteudoDiarioId :: PrimaryKey ConteudoDiarioT f
} deriving Generic
type DiarioABaixarToConteudoDiario = DiarioABaixarToConteudoDiarioT Identity
type DiarioABaixarToConteudoDiarioId = PrimaryKey DiarioABaixarToConteudoDiarioT Identity
instance Beamable DiarioABaixarToConteudoDiarioT
instance Table DiarioABaixarToConteudoDiarioT where
    data PrimaryKey DiarioABaixarToConteudoDiarioT f = DiarioABaixarToConteudoDiarioId (C f Int) deriving Generic
    primaryKey = DiarioABaixarToConteudoDiarioId . diarioabaixartoconteudodiarioId
instance Beamable (PrimaryKey DiarioABaixarToConteudoDiarioT)

data StatusDownloadDiarioT f = StatusDownloadDiario {
    statusdownloaddiarioId :: C f Int,
    statusdownloaddiarioDiarioABaixarId :: PrimaryKey DiarioABaixarT f,
    statusdownloaddiarioUrl :: C f Text,
    statusdownloaddiarioOrdem :: C f Int,
    statusdownloaddiarioInicioDownload :: C f UTCTime
} deriving Generic
type StatusDownloadDiario = StatusDownloadDiarioT Identity
type StatusDownloadDiarioId = PrimaryKey StatusDownloadDiarioT Identity
instance Beamable StatusDownloadDiarioT
instance Table StatusDownloadDiarioT where
    data PrimaryKey StatusDownloadDiarioT f = StatusDownloadDiarioId (C f Int) deriving Generic
    primaryKey = StatusDownloadDiarioId . statusdownloaddiarioId
instance Beamable (PrimaryKey StatusDownloadDiarioT)

data DownloadTerminadoT f = DownloadTerminado {
    downloadterminadoId :: C f Int,
    downloadterminadoStatusDownloadDiarioId :: PrimaryKey StatusDownloadDiarioT f,
    downloadterminadoMomentoTermino :: C f UTCTime,
    downloadterminadoMd5Sum :: C f Text,
    downloadterminadoFilePath :: C f (Maybe Text)
} deriving Generic
type DownloadTerminado = DownloadTerminadoT Identity
type DownloadTerminadoId = PrimaryKey DownloadTerminadoT Identity
instance Beamable DownloadTerminadoT
instance Table DownloadTerminadoT where
    data PrimaryKey DownloadTerminadoT f = DownloadTerminadoId (C f Int) deriving Generic
    primaryKey = DownloadTerminadoId . downloadterminadoId
instance Beamable (PrimaryKey DownloadTerminadoT)

data NomeEncontradoT f = NomeEncontrado {
    nomeencontradoId :: C f Int,
    nomeencontradoLoweredNome :: C f Text,
    nomeencontradoConteudoDiarioId :: PrimaryKey ConteudoDiarioT f,
    nomeencontradoPosicao :: C f Int
} deriving Generic
type NomeEncontrado = NomeEncontradoT Identity
type NomeEncontradoId = PrimaryKey NomeEncontradoT Identity
instance Beamable NomeEncontradoT
instance Table NomeEncontradoT where
    data PrimaryKey NomeEncontradoT f = NomeEncontradoId (C f Int) deriving Generic
    primaryKey = NomeEncontradoId . nomeencontradoId
instance Beamable (PrimaryKey NomeEncontradoT)

data TokenTextoT f = TokenTexto {
    tokentextoId :: C f Int,
    tokentextoValorTexto :: C f Text,
    tokentextoConteudoDiarioId :: PrimaryKey ConteudoDiarioT f,
    tokentextoTipo :: C f Text,
    tokentextoInicio :: C f Int,
    tokentextoComprimento :: C f Int
} deriving Generic
type TokenTexto = TokenTextoT Identity
type TokenTextoId = PrimaryKey TokenTextoT Identity
instance Beamable TokenTextoT
instance Table TokenTextoT where
    data PrimaryKey TokenTextoT f = TokenTextoId (C f Int) deriving Generic
    primaryKey = TokenTextoId . tokentextoId
instance Beamable (PrimaryKey TokenTextoT)