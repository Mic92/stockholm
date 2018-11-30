module Paths where

import Helpers.Path


amixer :: FilePath
amixer = findExecutable "amixer"

otpmenu :: FilePath
otpmenu = findExecutable "otpmenu"

passmenu :: FilePath
passmenu = findExecutable "passmenu"

slock :: FilePath
slock = "/run/wrappers/bin/slock"

su :: FilePath
su = "/run/wrappers/bin/su"

urxvtc :: FilePath
urxvtc = findExecutable "urxvtc"
