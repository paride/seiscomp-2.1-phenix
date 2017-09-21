LoadConfig() {
    KEY_TRANSFORM_SCRIPT="$(cat "$@" | while read line; do
        p=$`echo ${line#\*} | tr -s ' ' | cut -d ' ' -f 1`
        r=`echo $line | tr -s ' ' | cut -s -d ' ' -f 2`
        
        if [ -z "$r" ]; then
            continue
        fi
        
        echo "s%$r%$p%g;"
    done)"
}

Ask() {
    local a
    
    if [ -z "$3" ]; then
        echo -n "$2: "
    else
        echo -n "$2 [$3]: "
    fi

    read a
    
    if [ -z "$a" ]; then
        eval $1=\""$3"\"
    else
        eval $1=\""$a"\"
    fi
}

AskYN() {
    local a
    
    if [ -z "$3" ]; then
        echo -n "$2: "
    else
        echo -n "$2 [$3]: "
    fi

    while :; do
        read a
        [ "$a" = yes -o "$a" = no -o -z "$a" ] && break
        
        echo "Please answer yes or no"
    done
    
    if [ -z "$a" ]; then
        eval $1=\""$3"\"
    else
        eval $1=\""$a"\"
    fi
}

OutputKeys() {
    local p
    
    for p in `cat "$@" | sed -e '/^\*/d' | tr -s ' ' | cut -d ' ' -f 1`; do
        eval echo \"$p=\'\$$p\'\"
    done
}

OutputFile() {
    sed -e "$(eval echo \"$KEY_TRANSFORM_SCRIPT\")" "$1"
}

RemoveLines() {
    sed -e "$(while [ $# -gt 1 ]; do
        echo "/^#$2#/d;"
        shift
    done)" "$1" > "$1.xxx"
    mv "$1.xxx" $1
}

FixLines() {
    sed -e "$(while [ $# -gt 1 ]; do
        echo "s%^#$2#%%;"
        shift
    done)" "$1" > "$1.xxx"
    mv "$1.xxx" "$1"
}

