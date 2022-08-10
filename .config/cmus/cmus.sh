pcmus="`pgrep cmus`" #grep -C cmus for linux
if [[ $pcmus -gt "0" ]]; then

artist=`cmus-remote -Q | 
	grep --text '^tag artist' | 
	sed '/^tag artistsort/d' | 
	awk '{gsub("tag artist ", "");print}'`
title=`cmus-remote -Q  | 
	grep --text '^tag title' | 
	sed -e 's/tag title //' |
	awk '{gsub("tag title ", "");print}'`

	echo "$artist - $title"; else echo ""; 
fi
