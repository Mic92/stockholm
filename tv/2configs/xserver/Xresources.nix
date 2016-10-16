{ config, lib, pkgs, ... }:

with config.krebs.lib;

pkgs.writeText "Xresources" /* xdefaults */ ''
  !URxvt*background:	#050505

  !  2013-02-25 \e was reas escape before
  ! *VT100.Translations: #override\
  ! 	:<Btn4Down>: string("\e[5~")\n\
  ! 	:<Btn5Down>: string("\e[6~")

  ! XTerm*VT100*Translations: #override \
  ! Shift<Key>Return: string(" &") string(0x0A) \n\
  ! Meta<Key>Return: string(" | less") string(0x0A) \n\
  ! ~Shift<Key>Prior: scroll-back(1,page) \n\
  ! ~Shift<Key>Next: scroll-forw(1,page) \n\
  ! Shift<Key>Prior: scroll-back(1) \n\
  ! Shift<Key>Next: scroll-forw(1) \n\
  ! <Key>Delete: string(0x1b) string("[2~")
  ! \n\
  ! <Key>BackSpace: string(0x7f)

  !  2013-02-2013-02-25
  ! ! <M-c>: load bash-completion (if not already)
  ! URxvt*VT100*Translations: #override\
  !  	Meta<KeyPress>c:\
  ! 		string("\eOH# \eOF\n+compl\n\eOA\eOA\eOH\e[3~\e[3~\eOF")\
  ! 		string(0x7)\n

  !  do not scroll automatically on output:
  ! XTerm*scrollTtyOutput:	false
  URxvt*cutchars:  "\\`\"'&()*,;<=>?@[]^{|}‘’"
  ! URxvt*secondaryScreen: false

  ! URxvt*loginShell:	true

  URxvt*eightBitInput:		false
  ! *eightBitOutput:	1
  ! URxvt*decTerminalID:	220
  ! URxvt*utf8:		1
  ! URxvt*locale:		UTF-8
  ! XTerm*customization:	-color
  URxvt*SaveLines:	4096
  URxvt*font:		-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1
  URxvt*boldFont:		-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1

  !  2013-05-23 if this does not work try
  !    xset +fp /usr/share/fonts/local/
  !    xset fp rehash
  ! URxvt*font:       -*-termsynu-edium-*-*-*-12-*-*-*-*-*-iso10646-1
  ! URxvt*boldFont:		-*-termsynu-bold-*-*-*-12-*-*-*-*-*-iso10646-1
  ! 
  !-misc-termsynu-medium-r-normal--12-87-100-100-c-70-iso10646-1

  ! XTerm*font:	-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso10646-1
  URxvt*scrollBar:	false

  ! XTerm*font:-nil-profont-medium-r-normal--11-110-72-72-c-60-iso8859-1
  ! URxvt*boldFont:-nil-profont-medium-r-normal--11-110-72-72-c-60-iso8859-1

  URxvt*background:	#050505
  ! URxvt*background:	#041204

  !URxvt.depth: 32
  !URxvt*background: rgba:0500/0500/0500/cccc

  ! URxvt*background:	#080810
  URxvt*foreground:	#d0d7d0
  ! URxvt*background:	black
  ! URxvt*foreground:	white
  ! URxvt*background:	rgb:00/00/40
  ! URxvt*foreground:	rgb:a0/a0/d0
  ! XTerm*cursorColor:	rgb:00/00/60
  URxvt*cursorColor:	#f042b0
  URxvt*cursorColor2:	#f0b000
  URxvt*cursorBlink:	off
  ! URxvt*cursorUnderline: true
  ! URxvt*highlightColor: #232323
  ! URxvt*highlightTextColor: #b0ffb0

  URxvt*.pointerBlank: true
  URxvt*.pointerBlankDelay: 987654321
  URxvt*.pointerColor: #f042b0
  URxvt*.pointerColor2: #050505

  ! URxvt*fading: 50
  ! URxvt*fadeColor: #0f0f0f

  ! XTerm*colorMode:	on
  ! URxvt*dynamicColors:	on
  ! URxvt*boldColors:	off

  URxvt*jumpScroll:	true

  !  allow synthetic events for fvwm, so pass window specific keys
  ! XTerm*allowSendEvents:  true
  URxvt*allowSendEvents:	false

  !  better double/tripple clicking in xterms
  !   Format: csv, [low-]high:value
  ! 
  !  extend character class 48 due they are used in urls
  !  (see: man xterm; /CHARACTER CLASSES)
  !                        !     %     -./      @     &     =     ?
  URxvt*charClass:        33:48,37:48,45-47:48,64:48,38:48,61:48,63:48
  URxvt*cutNewline:       False
  URxvt*cutToBeginningOfLine:     False

  !  BLACK for indigo background
  URxvt*color0:		#232342

  !  TODO: man xterm; /ACTIONS

  ! *VT100*colorULMode: on
  ! XTerm*underLine: on
  ! 
  ! URxvt*color0:		black
  ! URxvt*color1:		red3
  ! URxvt*color2:		green3
  ! URxvt*color3:		yellow3
  ! URxvt*color4:		blue2
  ! URxvt*color5:		magenta3
  ! URxvt*color6:		cyan3
  ! URxvt*color7:		gray90
  ! URxvt*color8:		burlywood1
  ! URxvt*color9:		sienna1
  ! URxvt*color10:		PaleVioletRed1
  ! URxvt*color11:		LightSkyBlue
  ! URxvt*color12:		white
  ! URxvt*color13:		white
  ! URxvt*color14:		white
  ! URxvt*color33:		#f0b0f0


  ! URxvt*color0:	#000000
  ! URxvt*color1:	#c00000
  ! URxvt*color2:	#80c070 
  URxvt*color3:	#c07000
  ! URxvt*color4:	#0000c0
  URxvt*color4:	#4040c0
  ! URxvt*color5:	#c000c0 
  ! URxvt*color6:	#008080
  URxvt*color7:	#c0c0c0

  URxvt*color8:	#707070
  URxvt*color9:	#ff6060
  URxvt*color10:	#70ff70
  URxvt*color11:	#ffff70
  URxvt*color12:	#7070ff
  URxvt*color13:	#ff50ff
  URxvt*color14:	#70ffff
  URxvt*color15:	#ffffff

  ! XTerm*color91: #000070
  ! XTerm*color92: #000080
  ! XTerm*color93: #000090
  ! XTerm*color94: #0000a0
  ! XTerm*color95: #0000b0
  ! XTerm*color96: #0000c0
  ! XTerm*color97: #0000d0
  ! XTerm*color98: #0000e0
  ! XTerm*color99: #0000f0

  ! !! vim-create-colorscheme {{{
  ! !! Question	cterm=none
  ! XTerm*color20: #f0b000
  ! !! }}}
  ! 
  ! 
  ! #include ".xrdb/look-zenburn.xrdb"
  ! #include ".xrdb/xterm.xrdb"



  ! URxvt.perl-ext: matcher
  ! URxvt.urlLauncher: cr
  ! URxvt.underlineColor: blue

  ! URxvt.matcher.button:   1
  ! URxvt.perl-ext:         default,matcher
  ! URxvt.urlLauncher: cr
  ! URxvt.matcher.pattern.1: \\bwww\\.[\\w-]+\\.[\\w./?&@#-]*[\\w/-]
  ! URxvt.underlineColor: blue

  ! 2014-05-12 von lass
  !URxvt.perl-ext-common:      default,clipboard,url-select,keyboard-select 
  !URxvt.url-select.launcher:  /home/tv/bin/ff -new-tab 
  !URxvt.url-select.underline: true 
  !URxvt.keysym.M-u:           perl:url-select:select_next 
  !URxvt.keysym.M-Escape:      perl:keyboard-select:activate 
  !URxvt.keysym.M-s:           perl:keyboard-select:search




  !  2013-02-25 I neve use this
  URxvt*iso14755:	False

  URxvt*urgentOnBell: True
  URxvt*visualBell: True

  ! ref https://github.com/muennich/urxvt-perls
  URxvt*perl-ext: default,url-select
  URxvt*keysym.M-u: perl:url-select:select_next
  URxvt*url-select.launcher: /etc/per-user/${config.krebs.build.user.name}/bin/ff -new-tab
  URxvt*url-select.underline: true
  URxvt*colorUL: #4682B4
  URxvt.perl-lib: ${pkgs.urxvt_perls}/lib/urxvt/perl
  URxvt.saveLines: 4096

  root-urxvt*background: #230000
  root-urxvt*foreground: #e0c0c0
  root-urxvt*BorderColor: #400000
  root-urxvt*color0: #800000
''
