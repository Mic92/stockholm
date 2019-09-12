{pkgs, environment, config, lib, ... }:

with pkgs;

let

  bar_update_interval = "1"; # In seconds
  terminal_pkg = "${pkgs.gnome3.gnome-terminal}/bin/gnome-terminal";
  terminal_class = "Gnome\\\\-terminal";
  terminal_instance = "gnome\\\\-terminal\\\\-server";

  reload_layout = pkgs.writeScript "reload_layout.sh" ''
    #!/bin/sh
    pkill $(basename  "${terminal_pkg}")

    i3-msg append_layout ${shell_layout}

    for i in {1..16}
    do
      ${terminal_pkg} &
    done
  '';

  single_shell_spawn = pkgs.writeScript "single_shell_spawn.sh" ''
    #!/bin/sh

    i3-msg focus parent
    i3-msg append_layout ${single_shell}
    for i in {1..2}
    do
      ${terminal_pkg}
    done
    sleep 0.3
    i3-msg focus parent
    '';

  single_shell = pkgs.writeText "single-shell.json" ''
{
    "border": "pixel",
    "floating": "auto_off",
    "layout": "tabbed",
    "percent": 1,
    "type": "con",
    "nodes": [
        {
            "border": "pixel",
            "floating": "auto_off",
            "layout": "splith",
            "percent": 1,
            "type": "con",
            "nodes": [
                {
                    "border": "pixel",
                    "floating": "auto_off",
                    "layout": "splitv",
                    "percent": 0.5,
                    "type": "con",
                    "nodes": [
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        },
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        }
                    ]
                }
            ]
        }
    ]
}
  '';

  shell_layout = pkgs.writeText "shell-layout.json" ''
    {
    "border": "pixel",
    "floating": "auto_off",
    "layout": "tabbed",
    "percent": 1,
    "type": "con",
    "nodes": [
       {
            "border": "pixel",
            "floating": "auto_off",
            "layout": "splith",
            "percent": 1,
            "type": "con",
            "nodes": [
                {
                    "border": "pixel",
                    "floating": "auto_off",
                    "layout": "splitv",
                    "percent": 0.5,
                    "type": "con",
                    "nodes": [
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        },
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        }
                    ]
                },
                {
                    "border": "pixel",
                    "floating": "auto_off",
                    "layout": "splitv",
                    "percent": 0.5,
                    "type": "con",
                    "nodes": [
                        {
                            "border": "pixel",
                            "floating": "auto_off",
                            "layout": "splitv",
                            "percent": 1,
                            "type": "con",
                            "nodes": [
                                {
                                    "border": "pixel",

                                    "floating": "auto_off",
                                    "geometry": {
                                        "height": 434,
                                        "width": 722,
                                        "x": 0,
                                        "y": 0
                                    },
                                    "percent": 0.5,
                                    "swallows": [
                                        {
                                            "class": "^${terminal_class}$",
                                            "instance": "^${terminal_instance}$"
                                        }
                                    ],
                                    "type": "con"
                                },
                                {
                                    "border": "pixel",

                                    "floating": "auto_off",
                                    "geometry": {
                                        "height": 434,
                                        "width": 722,
                                        "x": 0,
                                        "y": 0
                                    },
                                    "percent": 0.5,
                                    "swallows": [
                                        {
                                            "class": "^${terminal_class}$",
                                            "instance": "^${terminal_instance}$"
                                        }
                                    ],
                                    "type": "con"
                                }
                            ]
                        }
                    ]
                }
            ]
        },

       {
            "border": "pixel",
            "floating": "auto_off",
            "layout": "splith",
            "percent": 1,
            "type": "con",
            "nodes": [
                {
                    "border": "pixel",
                    "floating": "auto_off",
                    "layout": "splitv",
                    "percent": 0.5,
                    "type": "con",
                    "nodes": [
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        },
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        }
                    ]
                },
                {
                    "border": "pixel",
                    "floating": "auto_off",
                    "layout": "splitv",
                    "percent": 0.5,
                    "type": "con",
                    "nodes": [
                        {
                            "border": "pixel",
                            "floating": "auto_off",
                            "layout": "splitv",
                            "percent": 1,
                            "type": "con",
                            "nodes": [
                                {
                                    "border": "pixel",

                                    "floating": "auto_off",
                                    "geometry": {
                                        "height": 434,
                                        "width": 722,
                                        "x": 0,
                                        "y": 0
                                    },
                                    "percent": 0.5,
                                    "swallows": [
                                        {
                                            "class": "^${terminal_class}$",
                                            "instance": "^${terminal_instance}$"
                                        }
                                    ],
                                    "type": "con"
                                },
                                {
                                    "border": "pixel",

                                    "floating": "auto_off",
                                    "geometry": {
                                        "height": 434,
                                        "width": 722,
                                        "x": 0,
                                        "y": 0
                                    },
                                    "percent": 0.5,
                                    "swallows": [
                                        {
                                            "class": "^${terminal_class}$",
                                            "instance": "^${terminal_instance}$"
                                        }
                                    ],
                                    "type": "con"
                                }
                            ]
                        }
                    ]
                }
            ]
        },

        {
            "border": "pixel",
            "floating": "auto_off",
            "layout": "splith",
            "percent": 1,
            "type": "con",
            "nodes": [
                {
                    "border": "pixel",
                    "floating": "auto_off",
                    "layout": "splitv",
                    "percent": 0.5,
                    "type": "con",
                    "nodes": [
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        },
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        }
                    ]
                },
                {
                    "border": "pixel",
                    "floating": "auto_off",
                    "layout": "splitv",
                    "percent": 0.5,
                    "type": "con",
                    "nodes": [
                        {
                            "border": "pixel",
                            "floating": "auto_off",
                            "layout": "splitv",
                            "percent": 1,
                            "type": "con",
                            "nodes": [
                                {
                                    "border": "pixel",

                                    "floating": "auto_off",
                                    "geometry": {
                                        "height": 434,
                                        "width": 722,
                                        "x": 0,
                                        "y": 0
                                    },
                                    "percent": 0.5,
                                    "swallows": [
                                        {
                                            "class": "^${terminal_class}$",
                                            "instance": "^${terminal_instance}$"
                                        }
                                    ],
                                    "type": "con"
                                },
                                {
                                    "border": "pixel",

                                    "floating": "auto_off",
                                    "geometry": {
                                        "height": 434,
                                        "width": 722,
                                        "x": 0,
                                        "y": 0
                                    },
                                    "percent": 0.5,
                                    "swallows": [
                                        {
                                            "class": "^${terminal_class}$",
                                            "instance": "^${terminal_instance}$"
                                        }
                                    ],
                                    "type": "con"
                                }
                            ]
                        }
                    ]
                }
            ]
        },


        {
            "border": "pixel",
            "floating": "auto_off",
            "layout": "splith",
            "percent": 1,
            "type": "con",
            "nodes": [
                {
                    "border": "pixel",
                    "floating": "auto_off",
                    "layout": "splitv",
                    "percent": 0.5,
                    "type": "con",
                    "nodes": [
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        },
                        {
                            "border": "pixel",

                            "floating": "auto_off",
                            "geometry": {
                                "height": 434,
                                "width": 722,
                                "x": 0,
                                "y": 0
                            },
                            "percent": 0.5,
                            "swallows": [
                                {
                                    "class": "^${terminal_class}$",
                                    "instance": "^${terminal_instance}$"
                                }
                            ],
                            "type": "con"
                        }
                    ]
                },
                {
                    "border": "pixel",
                    "floating": "auto_off",
                    "layout": "splitv",
                    "percent": 0.5,
                    "type": "con",
                    "nodes": [
                        {
                            "border": "pixel",
                            "floating": "auto_off",
                            "layout": "splitv",
                            "percent": 1,
                            "type": "con",
                            "nodes": [
                                {
                                    "border": "pixel",

                                    "floating": "auto_off",
                                    "geometry": {
                                        "height": 434,
                                        "width": 722,
                                        "x": 0,
                                        "y": 0
                                    },
                                    "percent": 0.5,
                                    "swallows": [
                                        {
                                            "class": "^${terminal_class}$",
                                            "instance": "^${terminal_instance}$"
                                        }
                                    ],
                                    "type": "con"
                                },
                                {
                                    "border": "pixel",

                                    "floating": "auto_off",
                                    "geometry": {
                                        "height": 434,
                                        "width": 722,
                                        "x": 0,
                                        "y": 0
                                    },
                                    "percent": 0.5,
                                    "swallows": [
                                        {
                                            "class": "^${terminal_class}$",
                                            "instance": "^${terminal_instance}$"
                                        }
                                    ],
                                    "type": "con"
                                }
                            ]
                        }
                    ]
                }
            ]
        }
    ]
    }
  '';


  i3_conf_file =  pkgs.writeText "config" ''
    # This file has been auto-generated by i3-config-wizard(1).
    # It will not be overwritten, so edit it as you like.
    #
    # Should you change your keyboard layout some time, delete
    # this file and re-run i3-config-wizard(1).
    #

    # i3 config file (v4)
    #
    # Please see https://i3wm.org/docs/userguide.html for a complete reference!

    set $mod Mod4

    #######################
    #                     #
    #       LOOKS         #
    #                     #
    #######################
    # Font for window titles. Will also be used by the bar unless a different font
    # is used in the bar {} block below.
    font pango:Bitstream Vera Sans Mono 14
    font pango:Monospace 14, Icons 10
    hide_edge_borders smart
    new_window pixel 1
    new_float normal

    # Lockscreen shortcut
    bindsym $mod+l exec xscreensaver-command -l

    # start a terminal
    bindsym $mod+Return exec ${terminal_pkg}


    # class                 border  backgr. text    indicator child_border
    client.focused          #4fceea #285577 #ffffff #2e9ef4   #285577
    client.focused_inactive #333333 #5f676a #ffffff #484e50   #5f676a
    client.unfocused        #333333 #222222 #888888 #292d2e   #222222

    # Size of border
    default_border pixel 3

    #######################
    #                     #
    #   DEFAULT CONFIG    #
    #                     #
    #######################
    floating_modifier $mod
    workspace_layout stacked
    default_orientation vertical

    # Kill focused window
    bindsym $mod+Shift+q kill

    # start dmenu (a program launcher)
    bindsym $mod+d exec ${pkgs.rofi}/bin/rofi -modi drun#run -combi-modi drun#run -show combi -show-icons -display-combi run
    # Switch windows
    bindsym $mod+x exec ${pkgs.rofi}/bin/rofi -modi window -show window -auto-select

    # Arrow keys for focus navigation
    bindsym $mod+Left focus left
    bindsym $mod+Down focus down
    bindsym $mod+Up focus up
    bindsym $mod+Right focus right

    # Move focused window
    bindsym $mod+Shift+j move left
    bindsym $mod+Shift+k move down
    bindsym $mod+Shift+l move up
    bindsym $mod+Shift+odiaeresis move right

    # Arrow keys for focused window movement
    bindsym $mod+Shift+Left move left
    bindsym $mod+Shift+Down move down
    bindsym $mod+Shift+Up move up
    bindsym $mod+Shift+Right move right

    # Split in horizontal orientation
    bindsym $mod+h split h

    # Split in vertical orientation
    bindsym $mod+v split v

    # Enter fullscreen mode for the focused container
    bindsym $mod+f fullscreen toggle

    # Change container layout (stacked, tabbed, toggle split)
    bindsym $mod+s layout stacking
    bindsym $mod+w layout tabbed
    bindsym $mod+e layout toggle split

    # Toggle tiling / floating
    bindsym $mod+Shift+space floating toggle

    # Change focus between tiling / floating windows
    bindsym $mod+space focus mode_toggle

    # Focus the parent container
    bindsym $mod+a focus parent

    # Reload the configuration file
    bindsym $mod+Shift+c reload

    # Restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
    bindsym $mod+Shift+r restart

    # Exit i3 (logs you out of your X session)
    bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"

    # Resize window. You can also use the mouse for that
    mode "resize" {
      # Pressing right will grow the window’s width.
      # Pressing up will shrink the window’s height.
      # Pressing down will grow the window’s height.
      bindsym a resize shrink width 10 px or 10 ppt
      bindsym s resize grow height 10 px or 10 ppt
      bindsym w resize shrink height 10 px or 10 ppt
      bindsym d resize grow width 10 px or 10 ppt

      # same bindings, but for the arrow keys
      bindsym Left resize shrink width 10 px or 10 ppt
      bindsym Down resize grow height 10 px or 10 ppt
      bindsym Up resize shrink height 10 px or 10 ppt
      bindsym Right resize grow width 10 px or 10 ppt

      # back to normal: Enter or Escape
      bindsym Return mode "default"
      bindsym Escape mode "default"
    }

    # Enable floating
    for_window [class="usbguard-applet-qt"] floating enable
    for_window [class="Nm-connection-editor"] floating enable
    for_window [class="Gnome-disks"] floating enable
    for_window [class="QtPass" title="QtPass"] move scratchpad

    # Make the currently focused window a scratchpad
    bindsym $mod+Shift+minus move scratchpad

    # Show the first scratchpad window
    bindsym $mod+minus scratchpad show

    # Show the sup-mail scratchpad window, if any.
    bindsym $mod+Shift+s [class="QtPass" title="QtPass"] scratchpad show

    #######################
    #                     #
    #     WORKSPACES      #
    #                     #
    #######################
    # Variables
    set $workspace1 "1: Terminals"
    set $workspace2 "2: Firefox"
    set $workspace3 "3: Emacs"
    set $workspace4 "4"
    set $workspace5 "5"
    set $workspace6 "6"
    set $workspace7 "7"
    set $workspace8 "8"
    set $workspace9 "9"
    set $workspace10 "10"

    assign [class="emacs"] $workspace3
    assign [class="Firefox"] $workspace2
    assign [class="Daily"] $workspace5

    assign [class="VirtualBox Manager"] $workspace4
    assign [class="Virt-manager" title="Virtual Machine Manager"] $workspace4

    assign [class="libreoffice"] $workspace6
    assign [class="Eclipse"] $workspace6

    # Workspace lateral movement
    bindsym $mod+Next workspace next
    bindsym $mod+Prior workspace prev

    # Switch to workspace
    bindsym $mod+1 workspace $workspace1
    bindsym $mod+2 workspace $workspace2
    bindsym $mod+3 workspace $workspace3
    bindsym $mod+4 workspace $workspace4
    bindsym $mod+5 workspace $workspace5
    bindsym $mod+6 workspace $workspace6
    bindsym $mod+7 workspace $workspace7
    bindsym $mod+8 workspace $workspace8
    bindsym $mod+9 workspace $workspace9
    bindsym $mod+0 workspace $workspace10

    # Move workspace to other monitor
    bindsym $mod+Shift+Next move workspace to output right
    bindsym $mod+Shift+Prior move workspace to output left

    # move focused container to workspace
    bindsym $mod+Shift+1 move container to workspace $workspace1
    bindsym $mod+Shift+2 move container to workspace $workspace2
    bindsym $mod+Shift+3 move container to workspace $workspace3
    bindsym $mod+Shift+4 move container to workspace $workspace4
    bindsym $mod+Shift+5 move container to workspace $workspace5
    bindsym $mod+Shift+6 move container to workspace $workspace6
    bindsym $mod+Shift+7 move container to workspace $workspace7
    bindsym $mod+Shift+8 move container to workspace $workspace8
    bindsym $mod+Shift+9 move container to workspace $workspace9
    bindsym $mod+Shift+0 move container to workspace $workspace10


    #######################
    #                     #
    #  FUNCTION KEYS      #
    #                     #
    #######################
    # Backlight controls
    bindsym XF86MonBrightnessUp exec --no-startup-id xbacklight +10
    bindsym XF86MonBrightnessDown exec --no-startup-id xbacklight -10

    # Pulse Audio controls
    bindsym XF86AudioRaiseVolume exec --no-startup-id pactl set-sink-volume 0 +5% #increase sound volume
    bindsym XF86AudioLowerVolume exec --no-startup-id pactl set-sink-volume 0 -5% #decrease sound volume
    bindsym XF86AudioMute exec --no-startup-id pactl set-sink-mute 0 toggle # mute sound
    bindsym XF86AudioMicMute exec --no-startup-id  amixer set Capture toggle

    #######################
    #                     #
    #      TTY KEYS       #
    #                     #
    #######################

    bindsym Mod1+F1 exec --no-startup-id chvt 0
    bindsym Mod1+F2 exec --no-startup-id chvt 1
    bindsym Mod1+F3 exec --no-startup-id chvt 2
    bindsym Mod1+F4 exec --no-startup-id chvt 3
    bindsym Mod1+F5 exec --no-startup-id chvt 4
    bindsym Mod1+F6 exec --no-startup-id chvt 5
    bindsym Mod1+F7 exec --no-startup-id chvt 6
    bindsym Mod1+F8 exec --no-startup-id chvt 7
    bindsym Mod1+F9 exec --no-startup-id chvt 8
    bindsym Mod1+F10 exec --no-startup-id chvt 9


    #######################
    #                     #
    #  CONVENIENCE KEYS   #
    #                     #
    #######################
    bindsym Mod1+Shift+3 exec screenshot

    #######################
    #                     #
    #    WINDOW LAYOUT    #
    #                     #
    #######################
    bindsym Shift+F11 exec --no-startup-id ${reload_layout}
    bindsym Shift+F12 exec --no-startup-id ${single_shell_spawn}


    bar {
        status_command i3status
        position top
    }

    #######################
    #                     #
    #       AUTORUNS      #
    #                     #
    #######################
    ## Start 16 gnome shells
    exec i3-msg 'workspace $workspace1;' && ${reload_layout}

    # Start firefox
    exec --no-startup-id ${pkgs.firefox}/bin/firefox --ProfileManager --new-instance --setDefaultBrowser
    
    # Start firefox
    exec --no-startup-id my-emacs-daemon 
  '';

in {

  #######################
  #                     #
  #     AUTORANDR       #
  #                     #
  #######################

  # Start autorandr on display change
  services.autorandr = {
    enable = true;
    defaultTarget = "mobile";
  };

  # What to execute after resolution has been changed
  environment.etc."xdg/autorandr/postswitch" = {
    text = '' sleep 4 && i3-msg "restart" '';

  };

  # Start autorandr once on startup
  systemd.user.services.boot-autorandr = {
    description = "Autorandr service";
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.autorandr}/bin/autorandr -c";
      Type = "oneshot";
    };
  };



  #######################
  #                     #
  #       XSERVER       #
  #                     #
  #######################
  services.xserver.enable = true;

  # Enable i3 Window Manager
  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3;
    configFile = i3_conf_file;
   };


  # ${pkgs.xorg.xhost}/bin/xhost +SI:localuser:${cfg.user.name}
  # ${pkgs.xorg.xhost}/bin/xhost -LOCAL:
  services.xserver.windowManager.default = "i3";
  services.xserver.desktopManager.xterm.enable = false;


  # Enable the X11 windowing system.
  services.xserver.displayManager.slim = {
    enable = true;
  };

  # Allow users in video group to change brightness
  hardware.brightnessctl.enable = true;

  environment.systemPackages = with pkgs; [
    rofi     # Dmenu replacement
    acpilight # Replacement for xbacklight
    arandr # Xrandr gui
    feh
    wirelesstools # To get wireless statistics
    acpi
    xorg.xhost
    xorg.xauth
    gnome3.gnome_terminal
  ];

}
