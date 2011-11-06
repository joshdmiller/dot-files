#~/bin/bash

vol=`amixer get "Master Front" | grep "Front Left:" | awk '{print $5}'`
if [ "${vol}" == "[0\%]" ]
then 
    echo "[Mute]"
else
    vol=${vol/\[/}
    vol=${vol/\]/}
    echo $vol
fi
