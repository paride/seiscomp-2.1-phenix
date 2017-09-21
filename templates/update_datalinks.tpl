#!/bin/bash

DATADIR=#home_sysop#/data
ARCHIVE=#home_sysop#/archive

find "$ARCHIVE" -type f -follow | sed -e 's/^\([^/]*\/\)*\([^.]*\)\.\([^.]*\)\.\(\.\|\([^.]\+\.\)\)\([^.]*\)\.\([^.]*\)\.\(.*\)/& \3\/\5\6.\7\/\3.\2.\6.\7.\8.0000/g' | while read f l; do
    [ -e "$DATADIR"/"$l" ] && continue
    
    mkdir -p "$DATADIR"/"$(dirname "$l")"
    ( cd "$DATADIR"/"$(dirname "$l")"; ln -s "$f" "$(basename "$l")" )
done

find "$DATADIR" -type d -follow -mindepth 2 -maxdepth 2 | while read d; do
    (
        cd "$d"
        last="$(find -type l -not -name active -and -not -name \*.ini | sort | while read f; do
            if [ ! -e "$f" ]; then
                rm -f "$f"
                continue
            fi

            echo "$f"
        done | tail --lines=1)"

        [ ! -z "$last" ] && mv "$last" active
    )
done

