module Paths where

import Helpers.Path


otpmenu :: FilePath
otpmenu = findExecutable "otpmenu"

pactl :: FilePath
pactl = findExecutable "pactl"

passmenu :: FilePath
passmenu = findExecutable "passmenu"

slock :: FilePath
slock = "/run/wrappers/bin/slock"

su :: FilePath
su = "/run/wrappers/bin/su"

urxvtc :: FilePath
urxvtc = findExecutable "urxvtc"
