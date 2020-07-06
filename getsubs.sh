#!/bin/bash

LIST=$*
RESUB='^[\t\ ]*(recursive\ )?subroutine\ ([^\ \(]+)'
REFUN='^[\t\ ]*(recursive\ )?function\ ([^\ \(]+)'

for i in $LIST
do
    echo "==== $i ===="
    SUBS=$(awk "/$RESUB/ {sub(\"^[\t\ ]*recursive[\ ]*\",\"\");sub(\"^[\t ]*subroutine[\ ]*\",\"\");split(\$0,s,\"(\");print s[1]}" $i)
    FUNS=$(awk "/$REFUN/ {sub(\"^[\t\ ]*recursive[\ ]*\",\"\");sub(\"^[\t ]*function[\ ]*\",\"\");split(\$0,s,\"(\");print s[1]}" $i)
    
    for j in $SUBS $FUNS
    do
	echo -n "$j, "
    done

    echo
    echo
done
