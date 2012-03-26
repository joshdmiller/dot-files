#!/bin/bash
# The **`xmonad/bin/volume.sh`** script reports the current volume for the
# specified mixer control in a way friendly enough to include in a dzen bar.

# This is the mixer control on which the script will report volume. Note that
# this should be the same value used in the keybindings of your
# [xmonad.hs](#/home/xmonad/xmonad.hs) file to control the volume.
mixer_control="Master"

# First, we ask amixer to report on the mixer control specified above. We pipe
# that output to grep, searching for the line containing "Front Left:". Amixer
# reports the volume for each channel independently rather than together, so we
# grab only the left one with the implicit assumption that the left and right
# channels are equal. Finally, we use awk to pull out the fifth argument in
# that line, which is the volume percentage plus the percent symbol, surrounded
# by square brackets (e.g. "[100%]").
vol=`amixer get "${mixer_control}" | grep "Front Left:" | awk '{print $5}'`

# If the current volume is 0%, we might as well report it as "[Mute]".
if [ $vol == "0%" ]
then 
    echo "[Mute]"

# Otherwise, we eliminate the square brackets provided from the output above
# and echo out the result. The conky bar that utilizes this script simply
# executes this file and and reports the outcome.
else
    vol=${vol/\[/}
    vol=${vol/\]/}
    echo $vol
fi
