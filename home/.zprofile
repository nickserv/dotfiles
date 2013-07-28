# Start X automatically if on tty1
#[[ $(fgconsole) = 1 ]] && exec startx &> ~/.xlog -- vt1
