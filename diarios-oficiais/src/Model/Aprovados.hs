{-# LANGUAGE PartialTypeSignatures #-}
module Model.Aprovados where

import Database.Beam
import Data.Time
import Zlude
import qualified Database.Beam.Postgres as Pg
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Text as T
import Model.Diarios
import Data.Char

data AprovadosDb f = AprovadosDb { _usuarios :: f (TableEntity UsuarioT), _assinaturas :: f (TableEntity AssinaturaT), _usuariosencontrados :: f (TableEntity UsuarioEncontradoT), _usuariosbuscados :: f (TableEntity UsuarioBuscadoT) } deriving Generic
instance Database be AprovadosDb
aprovadosDb :: DatabaseSettings be AprovadosDb
aprovadosDb = defaultDbSettings `withDbModification` dbModification {
  _assinaturas = modifyTable (const "assinaturas") (tableModification { _assinaturaUsuarioId = UsuarioId "usuario_id" } )
  , _usuariosencontrados = modifyTable (const "usuariosencontrados") (tableModification { _usuarioencontradoConteudoDiarioId = ConteudoDiarioId "conteudo_diario_id", _usuarioencontradoUsuarioId = UsuarioId "usuario_id" } )
  , _usuariosbuscados = modifyTable (const "usuariosbuscados") (tableModification { _usuariobuscadoConteudoDiarioId = ConteudoDiarioId "conteudo_diario_id", _usuariobuscadoUsuarioId = UsuarioId "usuario_id" } )
}

data UsuarioT f = UsuarioT {
  _usuarioId :: C f Int
  , _usuarioEmail :: C f T.Text
  , _usuarioNomeCompleto :: C f T.Text
  , _usuarioFacebookId :: C f (Maybe T.Text)
} deriving (Generic, Beamable)

type Usuario = UsuarioT Identity
type UsuarioId = PrimaryKey UsuarioT Identity
deriving instance Eq UsuarioId

instance Table UsuarioT where
    data PrimaryKey UsuarioT f = UsuarioId (Columnar f Int) deriving Generic
    primaryKey = UsuarioId . _usuarioId
instance Beamable (PrimaryKey UsuarioT)

_usuarioPrimeiroNome :: Usuario -> T.Text
_usuarioPrimeiroNome usr = T.takeWhile (' ' /=) $ _usuarioNomeCompleto usr

data AssinaturaT f = AssinaturaT {
  _assinaturaId :: Columnar f Int
  , _assinaturaUsuarioId :: PrimaryKey UsuarioT f
  , _assinaturaDesde :: C f Day
  , _assinaturaAte :: C f Day
} deriving (Generic, Beamable)

type Assinatura = AssinaturaT Identity
type AssinaturaId = PrimaryKey AssinaturaT Identity

instance Table AssinaturaT where
    data PrimaryKey AssinaturaT f = AssinaturaId (Columnar f Int) deriving Generic
    primaryKey = AssinaturaId . _assinaturaId
instance Beamable (PrimaryKey AssinaturaT)

data UsuarioEncontradoT f = UsuarioEncontrado {
  _usuarioencontradoId :: Columnar f Int
  , _usuarioencontradoConteudoDiarioId :: PrimaryKey ConteudoDiarioT f
  , _usuarioencontradoUsuarioId :: PrimaryKey UsuarioT f
  , _usuarioencontradoIdx :: C f Int
  , _usuarioencontradoNomeEncontrado :: C f T.Text
} deriving (Generic, Beamable)

type UsuarioEncontrado = UsuarioEncontradoT Identity
type UsuarioEncontradoId = PrimaryKey UsuarioEncontradoT Identity

instance Table UsuarioEncontradoT where
    data PrimaryKey UsuarioEncontradoT f = UsuarioEncontradoId (Columnar f Int) deriving Generic
    primaryKey = UsuarioEncontradoId . _usuarioencontradoId
instance Beamable (PrimaryKey UsuarioEncontradoT)

-- | Um registro de UsuarioBuscado indica que um certo Usuario já foi buscado num ConteudoDiario (e portanto não precisa ser buscado novamente a menos que o algoritmo de busca tenha mudado)
data UsuarioBuscadoT f = UsuarioBuscado {
  _usuariobuscadoId :: Columnar f Int
  , _usuariobuscadoConteudoDiarioId :: PrimaryKey ConteudoDiarioT f
  , _usuariobuscadoUsuarioId :: PrimaryKey UsuarioT f
  , _usuariobuscadoBuscaTerminadaEm :: C f UTCTime
} deriving (Generic, Beamable)

type UsuarioBuscado = UsuarioBuscadoT Identity
type UsuarioBuscadoId = PrimaryKey UsuarioBuscadoT Identity

instance Table UsuarioBuscadoT where
    data PrimaryKey UsuarioBuscadoT f = UsuarioBuscadoId (Columnar f Int) deriving Generic
    primaryKey = UsuarioBuscadoId . _usuariobuscadoId
instance Beamable (PrimaryKey UsuarioBuscadoT)

-- | Esta Query expressa todos os Usuarios com uma assinatura ativa na data fornecida
usuariosAtivosQuery :: Day -> (forall s. Q Pg.Postgres AprovadosDb s (UsuarioT (QExpr Pg.Postgres s)))
usuariosAtivosQuery dia = do
  usr <- all_ (_usuarios aprovadosDb)
  assinatura <- oneToMany_ (_assinaturas aprovadosDb) _assinaturaUsuarioId usr
  guard_ $ _assinaturaDesde assinatura <=. val_ dia &&. val_ dia <=. _assinaturaAte assinatura
  pure usr

-- | Retorna todos os usuários ativos na data fornecida que não foram buscados no ConteudoDiario com Id fornecido
getUsuariosAtivosNaoBuscados :: MonadIO m => PGS.Connection -> Day -> ConteudoDiarioId -> m [Usuario]
getUsuariosAtivosNaoBuscados conn dia conteudoDiarioId = liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select $ do
  usrAtivo <- usuariosAtivosQuery dia
  buscaUsr <- leftJoin_ (all_ (_usuariosbuscados aprovadosDb)) (\usrBuscado -> _usuariobuscadoConteudoDiarioId usrBuscado ==. val_ conteudoDiarioId &&. _usuariobuscadoUsuarioId usrBuscado ==. pk usrAtivo)
  guard_ $ pk buscaUsr ==. nothing_
  pure usrAtivo

agruparUsuariosPorPrimeiroNome :: [Usuario] -> [(T.Text, NonEmpty Usuario)]
agruparUsuariosPorPrimeiroNome = safeGroupBy (primeiroNomeLowered . _usuarioNomeCompleto)
    where primeiroNomeLowered = T.toLower . T.takeWhile (not . isSpace)

getTodosUsuariosAgrupadosPorPrimeiroNomeLowered :: MonadIO m => PGS.Connection -> m [(T.Text, NonEmpty Usuario)]
getTodosUsuariosAgrupadosPorPrimeiroNomeLowered conn = do
    todosUsrs <- liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select $ all_ (_usuarios aprovadosDb)
    return $ safeGroupBy (primeiroNomeLowered . _usuarioNomeCompleto) todosUsrs
    where primeiroNomeLowered = T.toLower . T.takeWhile (not . isSpace)
        -- TODO: Forbid empty spaces in beginning and end in the database and remove them before inserting names into the database!
        -- TODO: Só por via das dúvidas, verificar presença de hífen no nome de cadastrado.. (hífen pode aparecer no fim de uma linha para quebrar uma palavra no documento)