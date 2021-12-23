if [ $(setxkbmap -query | awk '/layout/{print $2}') == 'us' ]
then
	setxkbmap hr
else 
	setxkbmap us
fi

pkill -RTMIN+30 dwmblocks
