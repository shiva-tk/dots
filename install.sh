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
  [ -d "$dir" ] && [ "$dir" != "lightdm/" ] && stow "$dir"
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
cabal install --lib xmobar

# Set ly as dm, and remove gnome dm
echo "======================="
echo "  Setting Up lightdm   "
echo "======================="
sudo systemctl disable gdm.service
sudo systemctl enable lightdm.service
sudo cp ~/dots/lightdm/* /etc/lightdm/

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
