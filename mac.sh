# Based on:
# - https://gist.github.com/bradp/bea76b16d3325f5c47d4
# - https://gist.github.com/millermedeiros/6615994

echo "Creating an SSH key for you..."
ssh-keygen -t ed25519

echo "Please add this public key to Github \n"
echo "https://github.com/account/ssh \n"
read -p "Press [Enter] key after this..."

echo "Installing xcode"
xcode-select --install

if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

echo "Updating homebrew..."
brew update

echo "Installing Git..."
brew install git

echo "Git config"
git config --global user.name "Marc Fischer"
git config --global user.email mail@marcfischer.at

echo "Installing core packages..."
packages=(
    cmake
    coreutils
    fd
    ffmpeg
    findutils
    gawk
    gcc
    gdb
    gnu-sed
    graphviz
    htop
    imagemagick
    llvm
    lua
    tailscale
    neovim
    node
    r
    ripgrep
    rust
    stow
    svn
    tree
    wget
    youtube-dl
    z
)
brew install ${packages[@]}

brew tap homebrew/cask-fonts
brew install font-ubuntu
brew install font-ubuntu-mono
brew tap homebrew/cask-fonts
brew install font-hack-nerd-font
brew install font-ubuntu-mono-nerd-font

echo "Copying dotfiles from Github"
if [ ! -d "$HOME/dotfiles" ]; then
    git clone git@github.com:Viehzeug/dotfiles.git $HOME/dotfiles
fi
cd $HOME/dotfiles
stow -t $HOME -S stow -v
wget https://raw.githubusercontent.com/rupa/z/master/z.sh -O ~/.z.sh
cd -

echo "Installing core apps..."
apps=(
  #ballast -- readjusts airpod volume, check if still needed
  #hammerspoon
  #pokemon-reborn
  #tor-browser
  portfolioperformance
  alfred
  appcleaner
  balenaetcher
  battle-net
  bettertouchtool
  caffeine
  discord
  docker
  dropbox
  enpass
  firefox
  gimp
  google-chrome
  hex-fiend
  inkscape
  iterm2
  julia
  keybase
  latexit
  libreoffice
  mactex
  meld
  microsoft-auto-update
  microsoft-office
  microsoft-teams
  miniconda
  nordvpn
  obisidian
  omnidisksweeper
  owncloud
  plex-media-player
  plexamp
  protonmail-bridge
  signal
  skype
  slack
  steam
  telegram
  textexpander
  the-unarchiver
  timemachineeditor
  virtualbox
  visual-studio-code
  vlc
  vmware-fusion
  whatsapp
  warp
  xquartz
  zoom
  zotero
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo "installing apps with Cask..."
brew install --cask --appdir="/Applications" ${apps[@]}

brew cleanup

echo "Setting up conda"
conda init zsh

echo "Setting some Mac settings..."

#"Allow text selection in Quick Look"
defaults write com.apple.finder QLEnableTextSelection -bool TRUE

#show the Library folder
chflags nohidden ~/Library

#show path bar in finder
defaults write com.apple.finder ShowPathBar -bool True

#show status bar in finder
defaults write com.apple.finder ShowStatusBar -bool True

#show absolute path in finders title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool YES

#"Automatically quit printer app once the print jobs complete"
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

#"Saving to disk (not to iCloud) by default"
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

#"Check for software updates daily, not just once per week"
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

#"Enabling full keyboard access for all controls (e.g. enable Tab in modal dialogs)"
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

#"Setting trackpad & mouse speed to a reasonable number"
defaults write -g com.apple.trackpad.scaling 2
defaults write -g com.apple.mouse.scaling 2.5

#"Enabling subpixel font rendering on non-Apple LCDs"
defaults write NSGlobalDomain AppleFontSmoothing -int 2

#"Showing all filename extensions in Finder by default"
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

#"Disabling the warning when changing a file extension"
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

#"Use column view in all Finder windows by default"
defaults write com.apple.finder FXPreferredViewStyle Clmv

#"Avoiding the creation of .DS_Store files on network volumes"
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

#"Avoiding the creation of .DS_Store files on USB volumes"
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

#"Speeding up Mission Control animations and grouping windows by application"
defaults write com.apple.dock expose-animation-duration -float 0.1
defaults write com.apple.dock "expose-group-by-app" -bool true

#"Setting email addresses to copy as 'foo@example.com' instead of 'Foo Bar <foo@example.com>' in Mail.app"
defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false

# Don’t automatically rearrange Spaces based on most recent use
defaults write com.apple.dock mru-spaces -bool false

# dock location
defaults write com.apple.dock "orientation" -string "right"

# make dock icons small
defaults write com.apple.dock "tilesize" -int "36"

# disable recents
defaults write com.apple.dock "show-recents" -bool "false"

echo "Cleaning up brew"
brew cleanup
killall Finder
killall Dock
echo "Done!"


echo ""
echo ""
echo ""
echo "Next (manual) steps:"
echo "Install Mojo: https://developer.modular.com/download"