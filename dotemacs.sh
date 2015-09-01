#!/bin/bash

#----------------------------------------------------------------------
# From: http://stackoverflow.com/questions/238073/how-to-add-a-progress-bar-to-a-shell-script

# ProgressBar function.
# Input is currentState($1) and totalState($2).
function ProgressBar {
    # Process data.
    let _progress=(${1}*100/${2}*100)/100
    let _done=(${_progress}*4)/10
    let _left=40-$_done
    # Build progressbar string lengths.
    _fill=$(printf "%${_done}s")
    _empty=$(printf "%${_left}s")
    # Build progressbar strings and print the ProgressBar line
    # Output example:
    #   Progress : [########################################] 100%
    printf "\rProgress : [${_fill// /#}${_empty// /-}] ${_progress}%%\n"
}

# Total number of steps.
_end=7
step=0

##----------------------------------------------------------------------
## Welcome message.

cat << EOF
--------------------------------------------------------------------------

  This will guide you to install Emacs 24.5, his friends and additional
  files to have, at least, a small part of the all power that Emacs
  has. Press ENTER to continue.

--------------------------------------------------------------------------
EOF

read -s opcao; echo; echo

##----------------------------------------------------------------------
## Install Emacs-24.5.

if which emacs-24.5 >/dev/null
then
    echo ------------------------------------------------------------
    echo "Emacs-24.5 is installed."
    which emacs-24.5
    ProgressBar ${step} ${_end}; step=$((step+1))
    echo; echo
else
    echo ------------------------------------------------------------
    echo "Emacs-24.5 isn't installed. Do you want to install it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    read opcao
    case $opcao in
        y )
            echo "Add emacs24 repository."
            sudo add-apt-repository ppa:cassou/emacs
            echo
            echo "Update souces list."
            sudo apt-get update
            echo
            echo "Running \"apt-get build-dep emacs24\""
            sudo apt-get build-dep emacs24 -y
            echo
            cd ~/Downloads/
            echo "Downloading Emacs-24.5 ..."
            wget http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz
            echo
            echo "Extracting ..."
            tar -xf emacs-24.5.tar.gz && cd ~/Downloads/emacs-*
            echo
            echo "Type 1 to run \"./configure\""
            read opcao; [ $opcao -eq 1 ] && ./configure
            echo
            echo "Type 1 to run \"make\""
            read opcao; [ $opcao -eq 1 ] && make
            echo
            echo "Type 1 to run \"sudo make install\""
            read opcao; [ $opcao -eq 1 ] && sudo make install
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
fi

##----------------------------------------------------------------------
## Install Emacs' friends.

echo ------------------------------------------------------------
echo "Install emacs-goodies-el? [ y ]"
ProgressBar ${step} ${_end}; step=$((step+1))
read opcao
case $opcao in
    y )
        echo "Install Emacs' friends."
        sudo apt-get install emacs-goodies-el -y
        echo; echo
        ;;
    * )
        echo "Skipped."; echo; echo
        ;;
esac 

##----------------------------------------------------------------------
## Make desktop application for Emacs-24.5.

echo ------------------------------------------------------------
echo "Make desktop application for Emacs-24.5.? [ y ]"
ProgressBar ${step} ${_end}; step=$((step+1))
read opcao
case $opcao in
    y )
        echo "Creating \"/usr/share/applications/emacs-24.5.desktop\"."
        echo; echo
        cat > emacs-24.5.desktop << EOL
[Desktop Entry]
Version=1.0
Name=GNU Emacs 24.5
GenericName=Text Editor
Comment=View and edit files
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=/usr/local/bin/emacs-24.5 %F
TryExec=emacs-24.5
Icon=/home/walmes/GitLab/emacs/Emacs-icon.png
Type=Application
Terminal=false
Categories=Utility;Development;TextEditor;
StartupWMClass=emacs-24.5
EOL
sudo mv emacs-24.5.desktop /usr/share/applications/
        ;;
    * )
        echo "Skipped."; echo; echo
        ;;
esac 

##----------------------------------------------------------------------

emacsddir="$HOME/.emacs.d/lisp/"
if [ ! -d "$emacsddir" ]
then
    echo ------------------------------------------------------------
    echo "$emacsddir doesn't exists. It will be created."
    ProgressBar ${step} ${_end}; step=$((step+1))
    mkdir -v -p $HOME/.emacs.d/lisp/
    echo; echo
else
    echo ------------------------------------------------------------
    echo "$emacsddir exists."
    ProgressBar ${step} ${_end}; step=$((step+1))
    echo; echo
fi

#----------------------------------------------------------------------

dotemacs="$HOME/.emacs"
if [ -f "$dotemacs" ]
then
    echo ------------------------------------------------------------
    echo "~/.emacs file found."
    echo "Do you want replace it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    read opcao
    case $opcao in
        y )
            cd $HOME/GitLab/emacs/
            cp -v dotemacs.el ~/.emacs
            cp -v functions.el ~/.emacs.d/lisp/
            cp -v prelude-packages.el ~/.emacs.d/lisp/
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
else
    echo ------------------------------------------------------------
    echo "~/.emacs file not found."
    echo "It will be created."
    ProgressBar ${step} ${_end}; step=$((step+1))
    cd $HOME/GitLab/emacs/
    cp -v dotemacs.el ~/.emacs
    cp -v functions.el ~/.emacs.d/lisp/
    cp -v prelude-packages.el ~/.emacs.d/lisp/
    echo; echo
fi

##----------------------------------------------------------------------
## Send line or region for shell buffer. 

file="$HOME/.emacs.d/lisp/essh.el"
if [ -f "$file" ]
then
    echo ------------------------------------------------------------
    echo "~/.emacs.d/lisp/essh.el file found."
    echo "Do you want update it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    read opcao
    case $opcao in
        y )
            cd $HOME/.emacs.d/lisp/
            wget -N 'http://www.emacswiki.org/emacs/download/essh.el'
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
else
    echo ------------------------------------------------------------
    echo "~/.emacs.d/lisp/essh.el file not found."
    echo "It will be created."
    ProgressBar ${step} ${_end}; step=$((step+1))
    cd $HOME/.emacs.d/lisp/
    wget -N 'http://www.emacswiki.org/emacs/download/essh.el'
    echo; echo
fi

##----------------------------------------------------------------------

if which tree >/dev/null
then
    echo ------------------------------------------------------------
    echo "This is the last step of the process."
    echo "Do you want show the tree of ~/.emacs.d/ ? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    read opcao
    case $opcao in
        y )
            tree -L 2 ~/.emacs.d/
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
fi

ProgressBar ${step} ${_end}; step=$((step+1))
cat << EOF
--------------------------------------------------------------------------

  You have finished the installation of Emacs. Congratulations!

--------------------------------------------------------------------------
EOF
