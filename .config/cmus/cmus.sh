pcmus="`pgrep cmus`" 
if [ $pcmus -gt "0" ]; then

artist=`cmus-remote -Q | 
	grep --text '^tag artist' | 
	sed '/^tag artistsort/d' | 
	awk '{gsub("tag artist ", "");print}'`
title=`cmus-remote -Q  | 
	grep --text '^tag title' | 
	sed -e 's/tag title //' |
	awk '{gsub("tag title ", "");print}'`
album=`cmus-remote -Q |
        grep --text '^tag album ' | 
	awk '{gsub("tag album ", "");print}'`


        echo "$artist - $title($album)"; else echo ""; 
fi
