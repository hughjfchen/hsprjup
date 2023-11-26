-- | This module provides the git revision of the project for other modules
--  It need to be replaced by the build script before build the project.
module GitRev
  ( gitRev
  )
where

gitRev :: String
gitRev = "PLACEHOLDER_TO_BE_REPLACED_BY_REAL_GIT_REV_BEFORE_BUILD"
