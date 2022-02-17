dwmblocks &
transmission-daemon &
#killall -q dwmblocks; setsid dwmblocks &
# pkill -RTMIN+10 dwmblocks &
sxhkd -c ~/.config/sxhkd/sxhkdrc &
#acceptcrontabs.sh &
#updatecrontab.sh &
picom --backend glx --experimental-backends --blur-method dual_kawase --blur-strength 9 &
~/screen.sh &
sleep 2
external=$(xrandr | grep "success" | grep "HDMI-A-0")
if [ external ]; then
    xrandr --newmode "1800x1012_60.00"  150.50  1800 1912 2096 2392  1012 1015 1025 1050 -hsync +vsync
    xrandr --addmode HDMI-A-0 1800x1012_60.00
    xrandr --output HDMI-A-0 --primary --mode "1800x1012_60.00"  --left-of eDP
	feh --bg-center ~/.local/share/wallpapers/Brandenburg\ Gate\ Quadriga\ BW.jpg --bg-center ~/.local/share/wallpapers/Brandenburg\ Gate\ Quadriga\ BW.jpg 
else
	xrandr --output eDP --primary --mode "1920x1080"
	feh --bg-center ~/.local/share/wallpapers/Brandenburg\ Gate\ Quadriga\ BW.jpg
fi
if [ "$(pgrep emacs)" ]; then
	pkill emacs
fi
emacs --daemon &
xflux -l 43.744234 -g 15.888702 &
xrdb -merge ~/.Xresources
# eval $(dbus-launch --sh-syntax --exit-with-session dmw)
exec dwm
# 1800x1012 59.92 Hz (CVT) hsync: 62.92 kHz; pclk: 150.50 MHz
