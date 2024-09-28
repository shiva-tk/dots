# Install dependencies
echo "======================="
echo "Installing Dependencies"
echo "======================="
yay -S --needed - < packages.txt

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

# Stow configs
echo "======================="
echo "    Stowing Configs    "
echo "======================="
for dir in */; do
  [ -d "$dir" ] && stow "$dir"
done

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
