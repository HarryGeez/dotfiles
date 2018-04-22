#!/bin/bash
VOLUME_NAME="${@:$#}"
VOLUME_NAME=${VOLUME_NAME#/Volumes/}
USER_ID=501
GROUP_ID=20
TIMEOUT=20
if [ `/usr/bin/stat -f "%u" /dev/console` -eq 0 ]; then
    USERNAME=`/usr/bin/defaults read /Library/Preferences/com.apple.loginwindow | /usr/bin/grep autoLoginUser | /usr/bin/awk '{ print $3 }' | /usr/bin/sed 's/;//'`
    if [ "$USERNAME" = "" ]; then
        until [ `stat -f "%u" /dev/console` -ne 0 ] || [ $TIMEOUT -eq 0 ]; do
            sleep 1
            let TIMEOUT--
        done
        if [ $TIMEOUT -ne 0 ]; then
            USER_ID=`/usr/bin/stat -f "%u" /dev/console`
            GROUP_ID=`/usr/bin/stat -f "%g" /dev/console`
        fi
    else
        USER_ID=`/usr/bin/id -u $USERNAME`
        GROUP_ID=`/usr/bin/id -g $USERNAME`
    fi
else
    USER_ID=`/usr/bin/stat -f "%u" /dev/console`
    GROUP_ID=`/usr/bin/stat -f "%g" /dev/console`
fi

/opt/local/bin/ntfs-3g \
    -o volname="${VOLUME_NAME}" \
    -o local \
    -o negative_vncache \
    -o auto_xattr \
    -o auto_cache \
    -o noatime \
    -o windows_names \
    -o user_xattr \
    -o inherit \
    -o uid=$USER_ID \
    -o gid=$GROUP_ID \
    -o allow_other \
    "$@" &> /var/log/ntfsmnt.log

exit $?;
