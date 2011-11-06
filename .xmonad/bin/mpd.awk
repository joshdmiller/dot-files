#!/bin/awk -f

# GATHERS INFORMATION ABOUT THE GOINGS-ON OF MPD

BEGIN {

    MPD_CMD = "mpc"; # mpd
    MPD_CMD | getline;
    MPD_CMD | getline;
    mpd_state = $1;
    close(MPD_CMD);

    if ( mpd_state == "[playing]" )
        print "$mpd_smart ($mpd_elapsed/$mpd_length)";
    else {
        if ( mpd_state == "[paused]" )
            #print "| $mpd_smart ($mpd_elapsed/$mpd_length)";
            print "Paused"
        else {
            print "Not Playing"
        }
    }

}
