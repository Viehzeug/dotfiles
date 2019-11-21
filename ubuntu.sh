sudo apt-get install fail2ban htop emacs vim zsh

# ufw
sudo ufw allow 22
sudo ufw enable

echo "disable password auth and install public key"

echo "if gpu, try to find out the latest magic for nvidia drivers + cuda"

echo "setup conda"


# solarized
sudo apt-get install dconf-cli
cd ~
git clone https://github.com/Anthony25/gnome-terminal-colors-solarized.git
cd gnome-terminal-colors-solarized
./install
