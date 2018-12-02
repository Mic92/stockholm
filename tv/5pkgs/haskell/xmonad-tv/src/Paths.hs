module Paths where

import Helpers.Path


otpmenu :: FilePath
otpmenu = findExecutable "otpmenu"

pactl :: FilePath
pactl = findExecutable "pactl"

passmenu :: FilePath
passmenu = findExecutable "passmenu"

slock :: FilePath
slock = findExecutable "slock"

su :: FilePath
su = findExecutable "su"

urxvtc :: FilePath
urxvtc = findExecutable "urxvtc"
