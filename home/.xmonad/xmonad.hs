import XMonad
import XMonad.Hooks.DynamicLog -- xmobar integration
import XMonad.Util.EZConfig -- helper functions for configuration

-- Run xmonad and xmobar with the specified settings.
main = xmonad =<< xmobar defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
defaults = defaultConfig {
    -- preferred terminal program
    terminal           = "urxvtc"
  } `additionalKeys` [
    -- additional keybinds
    ((mod1Mask .|. shiftMask, xK_l), spawn "slock"),
    ((mod1Mask, xK_s), spawn "scrot -e 'mv $f ~/Screenshots/ 2>/dev/null'")
  ]
