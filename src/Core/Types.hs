{-# LANGUAGE GADTs #-}
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

type MyMyMyMyMyMyMyMyName = Text

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
  | ProjectGroup (NonEmpty Project)
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
  | Security
  deriving stock (Eq, Show, Typeable, Generic)

data DomainFeature
  = SimpleCommandLineTool
  | BackendService
  | DevOpsTool
  deriving stock (Eq, Show, Typeable, Generic)

data Phase = Development | Test | Production
  deriving stock (Eq, Show, Typeable, Generic)

data Environment where
  Environment :: Phase -> Environment
  deriving stock (Eq, Show, Typeable, Generic)

data Config where
  Config :: Environment -> Config
  deriving stock (Eq, Show, Typeable, Generic)

data GHCVersion = GHC8107 | GHC928 | GHC946 | GHC962
  deriving stock (Eq, Show, Typeable, Generic)

data NixRelease = Nix2205 | Nix2211 | Nix2305
  deriving stock (Eq, Show, Typeable, Generic)

data BuildFeature = Build GHCVersion NixRelease
  deriving stock (Eq, Show, Typeable, Generic)

data DeployFeature = Deploy Config BuildFeature
  deriving stock (Eq, Show, Typeable, Generic)

data DevelopFeature where
  Develop :: BuildFeature -> DevelopFeature
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
      >>> embedConst ("description: " <> "my project")
      >>> embedConst ("category: " <> "Command Line")
      >>> embedConst ("here: " <> "there")
      >>> embedConst ("whof" <> "whom")
      >>> embedConst ("who" <> "am I")
      >>> embedConst ("That" <> "This")
      >>> embedConst "I am your boss"
      >>> embedConst "Are you OK?"
      >>> embedConst "fake news."
      >>> embedConst "also fake news"
      >>> embedConst "and can the test be run automacially?"
      >>> embedConst "oh, this is fine."
      >>> embedConst "fake news is also news."
      >>> embedConst "fake, it is fake"
      >>> embedConst "Hey, you, give me your hands."
      >>> embedConst ("Vt World" <> "Vi World")
    )

cabalFileCommon :: Template Int Text
cabalFileCommon =
  makeTemplate
    ( embedConst ("version: " <> "0.0.1.0")
      >>> embedConst ("name: " <> "myname")
      >>> embedConst ("author: " <> "A Great Guy")
      >>> embedConst ("category: " <> "Network")
      >>> embedShow (const @Text "Yes")
      >>> embedConst ("Who can I talk to?" <> "I'm the one, the only one.")
      >>> embedConst ("description: " <> "A great library")
      >>> embedConst "a great story"
      >>> embedConst ("descript" <> "hehehehhe")
      >>> embedConst "Here"
      >>> embedConst "There"
      >>> embedConst ("You" <> "Me")
      >>> embedConst "How are you?"
      >>> embedConst "I am the one."
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
