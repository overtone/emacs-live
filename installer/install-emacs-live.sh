#!/bin/bash

# Emacs Live Installer
# Written by Sam Aaron samaaron@gmail.com
# May, 2012

# Note:
# Run at your own risk!
# As always, you should read code before you run it on your machine

# Directory to preserve any Emacs configs found
old_config=~/emacs-live-old-config
tmp_dir=~/.emacs-live-installer-tmp
username=$(whoami)

# Check for the presence of git
git --version 2>&1 >/dev/null # improvement by tripleee
GIT_IS_AVAILABLE=$?

# Borrowed from the lein downloader
HTTP_CLIENT=${HTTP_CLIENT:-"wget -O"}
if type -p curl >/dev/null 2>&1; then
    if [ "$https_proxy" != "" ]; then
        CURL_PROXY="-x $https_proxy"
    fi
    HTTP_CLIENT="curl $CURL_PROXY -f -k -L -o"
fi

if [[ -e $old_config ]]; then

  echo $(tput setaf 1)"Emacs Live Installer Warning"$(tput sgr0)

  echo "It looks like I've already stored an Emacs configuration in: "
  echo $(tput setaf 3)$old_config$(tput sgr0)
  echo "Please mv or rm it before running me again."
  echo "I don't want to clobber over valuable files."
  exit 0
fi

# Create temporary directory for working within
rm -rf $tmp_dir
mkdir $tmp_dir

# Download intro and outro text
$HTTP_CLIENT $tmp_dir/intro.txt https://raw.github.com/overtone/emacs-live/master/installer/intro.txt
$HTTP_CLIENT $tmp_dir/outro.txt https://raw.github.com/overtone/emacs-live/master/installer/outro.txt

# Print outro and ask for user confirmation to continue
echo ""
echo ""
echo $(tput setaf 4)
cat $tmp_dir/intro.txt
echo $(tput sgr0)
echo ""

read -p $(tput setaf 3)"Are you sure you would like to continue? (y/N) "$(tput sgr0)

function download_tarball {
     echo ""
     echo $(tput setaf 2)"--> Downloading Emacs Live..."$(tput sgr0)
     echo ""
     $HTTP_CLIENT $tmp_dir/live.zip https://github.com/overtone/emacs-live/zipball/master

     # Unzip zipball
     unzip $tmp_dir/live.zip -d $tmp_dir/
}

function git_clone {
     echo ""
     echo $(tput setaf 2)"--> Cloning Emacs Live..."$(tput sgr0)
     echo ""
    git clone https://github.com/overtone/emacs-live.git $tmp_dir/overtone-emacs-live
}

if [[ $REPLY =~ ^[Yy]$ ]]; then

     # User wishes to install

     # Download Emacs Live with git (or as a tarball if git isn't on the system)

    if [ $GIT_IS_AVAILABLE -eq 0 ]; then
        git_clone
    else
        download_tarball
    fi

    created_old_emacs_config_dir=false

    function create_old_dir {
        if $created_old_emacs_config_dir; then
            # do nothing
            true
        else
            echo ""
            echo $(tput setaf 1)
            echo "======================================"
            echo "     Emacs config files detected. "
            echo "======================================$(tput sgr0)"

            mkdir -p $old_config
            echo "# Your Old Emacs Config Files

This directory contains any Emacs configuration files that had existed prior
to installing Emacs Live.

To see which files have been preserved:

    ls -allh $old_config

To revert back to your old Emacs configs simply:

    rm -rf ~/.emacs.d
    mv $old_config/.emacs* ~/
    rm -rf $old_config" > $old_config/README.md

            created_old_emacs_config_dir=true
        fi
    }

    if [ -e ~/.emacs.d/ ]; then
        create_old_dir
        echo $(tput setaf 1)
        echo "Found ~/.emacs.d config directory"
        echo ""
        mv ~/.emacs.d $old_config/.emacs.d
        echo "Moved to $old_config/.emacs.d"
        echo "------------------------------------------"
        echo ""
        echo $(tput sgr0)
    fi

    if [ -e ~/.emacs.el ]; then
        create_old_dir
        echo $(tput setaf 1)
        echo "Found ~/.emacs.el config file."
        echo ""
        mv ~/.emacs.el $old_config/.emacs.el
        echo "Moved to $old_config/.emacs.el"
        echo "------------------------------------------"
        echo ""
        echo $(tput sgr0)
    fi

    if [ -e ~/.emacs ]; then
        create_old_dir
        echo $(tput setaf 1)
        echo "Found ~/.emacs config file."
        echo ""
        mv ~/.emacs $old_config/.emacs
        echo "Moved to $old_config/.emacs"
        echo "------------------------------------------"
        echo ""
        echo $(tput sgr0)
    fi

    mkdir ~/.emacs.d
    cp -R $tmp_dir/overtone-emacs-live/. ~/.emacs.d
    echo $(tput setaf 4)"Personal Pack"
    echo "-------------"$(tput sgr0)
    echo ""
    echo "If you wish to personalise Emacs Live, it is recommended that you place your
modifications in a personal pack which I can create for you now."
    echo ""
    echo $(tput setaf 2)"What will happen:"
    echo "* Your pack will be created and placed in ~/.live-packs/$username-pack"
    echo "* An Emacs Live config file will be created for you in ~/.emacs-live.el "$(tput sgr0)
    echo ""
    read -p $(tput setaf 3)"Would you like to create a personal pack? (Y/n) "$(tput sgr0)

    if [[ $REPLY =~ ^[^nN]*$ ]]; then
        mkdir -p ~/.live-packs/
        echo "(live-add-packs '(~/.live-packs/$username-pack))" >> ~/.emacs-live.el
        cp -R ~/.emacs.d/packs/template/user-template-pack/ ~/.live-packs/$username-pack
        echo ""
        echo $(tput setaf 2)"--> Personal Pack created"$(tput sgr0)
    fi

    echo $(tput setaf 2)"--> Installation Completed"$(tput sgr0)
    echo $(tput setaf 5)
    cat $tmp_dir/outro.txt
    echo $(tput sgr0)
    echo ""

    rm -rf $tmp_dir

else
  echo "--> Installation aborted."
fi
