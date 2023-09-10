
module Core.Types
  ( -- | project
    emptyProject,
    devopsProject,
    appProject,
    mkProject,
    -- | features
    devopsOnly,
    appOnly,
    fullFeature,
  )
where

import Tophat

type ProjectName = Text

type ProjectAuthor = Text

type ProjectInitVersion = Text

type ProjectDescription = Text

data ProjectFeature
  = DevOpsOnly (NonEmpty DevOpsFeature)
  | AppOnly AppArchitecture (NonEmpty AppFeature)
  | FullFeature AppArchitecture (NonEmpty AppFeature) (NonEmpty DevOpsFeature)
  deriving stock (Eq, Show, Typeable, Generic)

data Project
  = EmptyProject
  | NonEmptyProject
      ProjectName
      ProjectAuthor
      ProjectInitVersion
      ProjectDescription
      ProjectFeature
  | ProjectGroup (NonEmpty ProjectFeature)
  deriving stock (Eq, Show, Typeable, Generic)

data AppFeature
  = Common CommonFeature
  | Domain DomainFeature
  deriving stock (Eq, Show, Typeable, Generic)

data CommonFeature
  = Logging
  | CmdLineParsing
  | ConfigParsing
  | Persistent
  | Monitoring
  deriving stock (Eq, Show, Typeable, Generic)

data DomainFeature
  = SimpleCommandLineTool
  | BackendService
  | DevOpsTool
  deriving stock (Eq, Show, Typeable, Generic)

data Phase = Development | Test | Production
  deriving stock (Eq, Show, Typeable, Generic)

data Environment = Environment Phase
  deriving stock (Eq, Show, Typeable, Generic)


data Config = Config Environment
  deriving stock (Eq, Show, Typeable, Generic)

data GHCVersion = GHC8107 | GHC928 | GHC946 | GHC962
  deriving stock (Eq, Show, Typeable, Generic)

data NixRelease = Nix2205 | Nix2211 | Nix2305
  deriving stock (Eq, Show, Typeable, Generic)

data BuildFeature = Build GHCVersion NixRelease
  deriving stock (Eq, Show, Typeable, Generic)

data DeployFeature = Deploy Config BuildFeature
  deriving stock (Eq, Show, Typeable, Generic)

data DevelopFeature = Develop BuildFeature
  deriving stock (Eq, Show, Typeable, Generic)

data DevOpsFeature
  = Building BuildFeature
  | Deploying DeployFeature
  | Developing DevelopFeature
  deriving stock (Eq, Show, Typeable, Generic)

data AppArchitecture
  = AllInIO
  | TaglessFinalMTL
  | ReaderTCapability
  | ServiceHandleRegistry
  | EffectSystemEffectful
  | FreeMonadPolyney
  deriving stock (Eq, Show, Typeable, Generic)

cabalFileMeta :: Template Int Text
cabalFileMeta =
  makeTemplate
    ( embedConst ("version: " <> "0.0.1.0")
      >>> embedConst ("name: " <> "myname")
    )

cabalFileCommon :: Template Int Text
cabalFileCommon =
  makeTemplate
    ( embedConst ("version: " <> "0.0.1.0")
        >>> embedConst ("name: " <> "myname")
    )

cabalFile :: Template Int Text
cabalFile = cabalFileMeta <> cabalFileCommon

emptyProject :: Project
emptyProject = EmptyProject

devopsProject ::
  ProjectName ->
  ProjectAuthor ->
  ProjectInitVersion ->
  ProjectDescription ->
  NonEmpty DevOpsFeature ->
  Project
devopsProject name author initVersion description feats =
  NonEmptyProject
    name
    author
    initVersion
    description
    $ DevOpsOnly feats

appProject ::
  ProjectName ->
  ProjectAuthor ->
  ProjectInitVersion ->
  ProjectDescription ->
  AppArchitecture ->
  NonEmpty AppFeature ->
  Project
appProject name author initVersion description appArch feats =
  NonEmptyProject
    name
    author
    initVersion
    description
    $ AppOnly appArch feats

mkProject ::
  ProjectName ->
  ProjectAuthor ->
  ProjectInitVersion ->
  ProjectDescription ->
  AppArchitecture ->
  NonEmpty AppFeature ->
  NonEmpty DevOpsFeature ->
  Project
mkProject name author initVersion description appArch appFeats devopsFeats =
  NonEmptyProject
    name
    author
    initVersion
    description
    $ FullFeature appArch appFeats devopsFeats

devopsOnly :: NonEmpty DevOpsFeature -> ProjectFeature
devopsOnly = DevOpsOnly

appOnly :: AppArchitecture -> NonEmpty AppFeature -> ProjectFeature
appOnly = AppOnly

fullFeature ::
  AppArchitecture ->
  NonEmpty AppFeature ->
  NonEmpty DevOpsFeature ->
  ProjectFeature
fullFeature = FullFeature
