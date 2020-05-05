module Paths where

import Helpers.Path


flameshot :: FilePath
flameshot = findExecutable "flameshot-once"

otpmenu :: FilePath
otpmenu = findExecutable "otpmenu"

pactl :: FilePath
pactl = findExecutable "pactl"

passmenu :: FilePath
passmenu = findExecutable "passmenu"

pavucontrol :: FilePath
pavucontrol = findExecutable "pavucontrol"

slock :: FilePath
slock = findExecutable "slock"

su :: FilePath
su = findExecutable "su"

urxvtc :: FilePath
urxvtc = findExecutable "urxvtc"

xcalib :: FilePath
xcalib = findExecutable "xcalib"
