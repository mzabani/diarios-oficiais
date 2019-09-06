{-# LANGUAGE PartialTypeSignatures #-}
module Model.Diarios where

import Data.Time
import Database.Beam
import qualified Database.Beam.Postgres as Pg
import qualified Database.PostgreSQL.Simple as PGS
import Data.Text

data DiariosDb f = DiariosDb { origensDiarios :: f (TableEntity OrigemDiarioT), diarios :: f (TableEntity DiarioT), diariosABaixar :: f (TableEntity DiarioABaixarT), conteudosDiarios :: f (TableEntity ConteudoDiarioT), diariosABaixarToConteudosDiarios :: f (TableEntity DiarioABaixarToConteudoDiarioT), secoesDiarios :: f (TableEntity SecaoDiarioT), paragrafosDiarios :: f (TableEntity ParagrafoDiarioT), statusDownloads :: f (TableEntity StatusDownloadDiarioT), downloadsTerminados :: f (TableEntity DownloadTerminadoT), nomesEncontrados :: f (TableEntity NomeEncontradoT), tokensTextoTbl :: f (TableEntity TokenTextoT) } deriving Generic
instance Database be DiariosDb
diariosDb :: DatabaseSettings be DiariosDb
diariosDb = defaultDbSettings `withDbModification`
            dbModification {
                origensDiarios  = modifyTable (const "origemdiario") tableModification,
                diarios         = modifyTable (const "diario") (tableModification { diarioOrigemDiarioId = OrigemDiarioId "origemdiarioid" }),
                statusDownloads = modifyTable (const "statusdownloaddiario") (tableModification { statusdownloaddiarioDiarioABaixarId = DiarioABaixarId "diarioabaixarid", statusdownloaddiarioInicioDownload = "iniciodownload" }),
                diariosABaixar = modifyTable (const "diarioabaixar") tableModification { diarioabaixarDiarioId = DiarioId "diarioid", diarioabaixarInicioDownload = "iniciodownload" },
                conteudosDiarios = modifyTable (const "conteudodiario") tableModification { conteudodiarioConteudo = "conteudo", conteudodiarioMd5Sum = "md5sum" },
                downloadsTerminados = modifyTable (const "downloadterminado") tableModification { downloadterminadoStatusDownloadDiarioId = StatusDownloadDiarioId "statusdownloaddiarioid", downloadterminadoMomentoTermino = "momentotermino", downloadterminadoMd5Sum = "md5sum", downloadterminadoFilePath = "filepath" },
                diariosABaixarToConteudosDiarios = modifyTable (const "diarioabaixartoconteudodiario") tableModification { diarioabaixartoconteudodiarioDiarioABaixarId = DiarioABaixarId "diarioabaixarid", diarioabaixartoconteudodiarioConteudoDiarioId = ConteudoDiarioId "conteudodiarioid" },
                secoesDiarios = modifyTable (const "secaodiario") tableModification { secaodiarioConteudoDiarioId = ConteudoDiarioId "conteudodiarioid", secaodiarioConteudo = "conteudo" },
                paragrafosDiarios = modifyTable (const "paragrafodiario") tableModification { paragrafodiarioConteudoDiarioId = ConteudoDiarioId "conteudodiarioid", paragrafodiarioConteudo = "conteudo" },
                nomesEncontrados = modifyTable (const "nomeencontrado") tableModification { nomeencontradoConteudoDiarioId = ConteudoDiarioId "conteudodiarioid", nomeencontradoLoweredNome = "lowerednome", nomeencontradoPosicao = "posicao" },
                tokensTextoTbl = modifyTable (const "tokens_texto") tableModification { tokentextoConteudoDiarioId = ConteudoDiarioId "conteudo_diario_id" }
            }

data OrigemDiarioT f = OrigemDiario {
    origemdiarioId :: C f Int,
    origemdiarioNomeCompleto :: C f (Maybe Text),
    origemdiarioCidade :: C f (Maybe Text),
    origemdiarioEstado :: C f (Maybe Text)
} deriving Generic
type OrigemDiario = OrigemDiarioT Identity
type OrigemDiarioId = PrimaryKey OrigemDiarioT Identity
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
getDiarioNaData oid dataDiario conn = liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runSelectReturningOne $ select $ do
    diario <- all_ (diarios diariosDb)
    guard_ $ diarioOrigemDiarioId diario ==. val_ oid &&. diarioData diario ==. val_ dataDiario
    return diario

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
    conteudodiarioConteudo :: C f Text,
    conteudodiarioMd5Sum :: C f Text
} deriving Generic
type ConteudoDiario = ConteudoDiarioT Identity
type ConteudoDiarioId = PrimaryKey ConteudoDiarioT Identity
instance Beamable ConteudoDiarioT
instance Table ConteudoDiarioT where
    data PrimaryKey ConteudoDiarioT f = ConteudoDiarioId (C f Int) deriving Generic
    primaryKey = ConteudoDiarioId . conteudodiarioId
instance Beamable (PrimaryKey ConteudoDiarioT)

data SecaoDiarioT f = SecaoDiario {
    secaodiarioId :: C f Int,
    secaodiarioConteudoDiarioId :: PrimaryKey ConteudoDiarioT f,
    secaodiarioOrdem :: C f Int,
    secaodiarioConteudo :: C f Text
} deriving Generic
type SecaoDiario = SecaoDiarioT Identity
type SecaoDiarioId = PrimaryKey SecaoDiarioT Identity
instance Beamable SecaoDiarioT
instance Table SecaoDiarioT where
    data PrimaryKey SecaoDiarioT f = SecaoDiarioId (C f Int) deriving Generic
    primaryKey = SecaoDiarioId . secaodiarioId
instance Beamable (PrimaryKey SecaoDiarioT)

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
    downloadterminadoFilePath :: C f Text
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