# Install dependencies
echo "======================="
echo "Installing Dependencies"
echo "======================="
yay -S --needed - < packages.txt

# Stow configs
echo "======================="
echo "    Stowing Configs    "
echo "======================="
for dir in */; do
  [ -d "$dir" ] && stow "$dir"
done

# Install haskell and xmonad through ghcup - ghcup preferred to pacman to manage haskell
echo "======================="
echo "   Installing XMonad   "
echo "======================="
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
sudo pacman -S git xorg-server xorg-apps xorg-xinit xorg-xmessage libx11 libxft libxinerama libxrandr libxss pkgconf
cd ~/.xmonad
git clone https://github.com/xmonad/xmonad
git clone https://github.com/xmonad/xmonad-contrib
stack init
stack install

cd ~/dots

# Install haskell and xmonad through ghcup - ghcup preferred to pacman to manage haskell
echo "======================="
echo "   Installing XMobar   "
echo "======================="
cabal install xmobar -fall_extensions

# Set ly as dm, and remove gnome dm
echo "======================="
echo "    Setting Up Ly DM   "
echo "======================="
sudo systemctl disable gdm.service
sudo systemctl enable ly.service

# Install jonaburg/picom
echo "======================="
echo "   Installing Picom    "
echo "======================="
yay -S ninja meson
git clone https://github.com/pijulius/picom.git tmp
cd tmp
meson --buildtype=release . build
ninja -C build
sudo ninja -C build install
cd ..
rm -rf tmp
sudo pacman -R ninja meson

# Change to zsh
echo "======================="
echo " Changing Shell to zsh "
echo "======================="
chsh -s "/bin/zsh" $USER

# Install doom emacs
echo "======================="
echo " Installing Doom Emacs "
echo "======================="
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
systemctl enable --user emacs
~/.config/emacs/bin/doom sync
