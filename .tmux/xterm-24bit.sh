cat << EOF >> /tmp/xterm-24bit.terminfo
xterm-24bit|xterm with 24-bit direct color mode,
   use=xterm-256color,
   sitm=\E[3m,
   ritm=\E[23m,
   setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
   setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,

EOF

/usr/bin/tic -x -o ~/.terminfo /tmp/xterm-24bit.terminfo

echo -e "\nexport TERM=xterm-24bit\n" >> ~/.bashrc
echo -e "\nexport TERM=xterm-24bit\n" >> ~/.zshrc


