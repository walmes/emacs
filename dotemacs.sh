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
_end=13
step=0

## Usage:
# step=0
# ProgressBar ${step} ${_end}; step=$((step+1))
# echo
# ProgressBar ${step} ${_end}; step=$((step+1))
# echo
# ProgressBar ${step} ${_end}; step=$((step+1))
# echo

##----------------------------------------------------------------------
## Welcome message.

cat << EOF
--------------------------------------------------------------------------

  This will guide you to install Emacs, his friends and additional files
  to have, at least, a small part of the all power that Emacs has. Press
  ENTER to continue.

--------------------------------------------------------------------------
EOF

read -s opcao; echo; echo

##----------------------------------------------------------------------
## Install Emacs24.

# if which emacs24; then
if which emacs24 >/dev/null
then
    echo ------------------------------------------------------------
    echo "Emacs24 is installed."
    which emacs24
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    echo; echo
else
    echo ------------------------------------------------------------
    echo "Emacs24 isn't installed. Do you want to install it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Add emacs24 repository."
            sudo add-apt-repository ppa:cassou/emacs
            echo "Update souces list."
            sudo apt-get update
            echo "Install emacs24."
            sudo apt-get install emacs24 -y
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
fi

##----------------------------------------------------------------------
## Install Emacs' friends?

echo ------------------------------------------------------------
echo "Install emacs-goodies-el, auto-complete-el and ess [ y ]"
ProgressBar ${step} ${_end}; step=$((step+1))
# echo ------------------------------------------------------------
read opcao
case $opcao in
    y )
        echo "Install Emacs' friends."
        sudo apt-get install emacs-goodies-el auto-complete-el ess -y
        echo; echo
        ;;
    * )
        echo "Skipped."; echo; echo
        ;;
esac 

##----------------------------------------------------------------------
## Create GitHub directory.

githubdir="$HOME/GitHub"
if [ ! -d "$githubdir" ]
then
    echo ------------------------------------------------------------
    echo "$githubdir doesn't exists. It will be created."
    ProgressBar ${step} ${_end}; step=$((step+1))
    mkdir -v $HOME/GitHub
    echo; echo
fi

##----------------------------------------------------------------------
## Clone/update walmes/emacs repository.

emacsgit="$HOME/GitHub/emacs/.git/"
if [ -d "$emacsgit" ]
then
    echo ------------------------------------------------------------
    echo "~/GitHub/emacs/ repository found."
    ProgressBar ${step} ${_end}; step=$((step+1))
    echo
    echo
    # echo "Do you want update this repository? [ y ]"
    # echo ------------------------------------------------------------
    # read opcao
    # case $opcao in
    #     y )
    #         echo "Updating from github.com/walmes/emacs.git ..."
    #         cd $HOME/GitHub/emacs/
    #         git pull
    #         echo; echo
    #         ;;
    #     * )
    #         echo "Skipped."; echo; echo
    #         ;;
    # esac 
else
    echo ------------------------------------------------------------
    echo "~/GitHub/emacs/ repository not found."
    echo "Do you want to clone it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Cloning from github.com/walmes/emacs.git ..."
            cd $HOME/GitHub/
            git clone git://github.com/walmes/emacs.git
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
fi

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

dotemacs="$HOME/.emacs"
if [ -f "$dotemacs" ]
then
    echo ------------------------------------------------------------
    echo "~/.emacs file found."
    echo "Do you want replace it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            cd $HOME/GitHub/emacs/
            cp -v dotemacs.el ~/.emacs
            cp -v functions.el ~/.emacs.d/
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
    # echo ------------------------------------------------------------
    cd $HOME/GitHub/emacs/
    cp -v dotemacs.el ~/.emacs
    cp -v functions.el ~/.emacs.d/
    echo; echo
fi

##----------------------------------------------------------------------
## Solarized color theme for Emacs.

dir="$HOME/.emacs.d/emacs-color-theme-solarized/.git/"
if [ -d "$dir" ]
then
    echo ------------------------------------------------------------
    echo "~/.emacs.d/emacs-color-theme-solarized repository found."
    echo "Do you want update this repository? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Updating ...";
            cd $HOME/.emacs.d/emacs-color-theme-solarized/
            git pull
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
else
    echo ------------------------------------------------------------
    echo "~/.emacs.d/emacs-color-theme-solarized repository not found."
    echo "Do you want to clone it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Cloning ..."
            cd $HOME/.emacs.d/
            git clone https://github.com/sellout/emacs-color-theme-solarized
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
fi

##----------------------------------------------------------------------
## Markdown mode for Emacs.

dir="$HOME/.emacs.d/markdown-mode/.git/"
if [ -d "$dir" ]
then
    echo ------------------------------------------------------------
    echo "~/.emacs.d/markdown-mode/ repository found."
    echo "Do you want update this repository? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Updating ...";
            cd $HOME/.emacs.d/markdown-mode/
            git pull
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
else
    echo ------------------------------------------------------------
    echo "~/.emacs.d/markdown-mode/ not found."
    echo "Do you want to clone it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Cloning ..."
            cd $HOME/.emacs.d/
            git clone git://jblevins.org/git/markdown-mode.git
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
fi

##----------------------------------------------------------------------
## ESS - Emacs Speaks Statistics.

# dir="$HOME/.emacs.d/ESS/.git/"
# if [ -d "$dir" ]
# then
#     echo ------------------------------------------------------------
#     echo "~/.emacs.d/ESS/ repository found."
#     echo "Do you want update this repository? [ y ]"
#     ProgressBar ${step} ${_end}; step=$((step+1))
#     # echo ------------------------------------------------------------
#     read opcao
#     case $opcao in
#         y )
#             echo "Updating ...";
#             cd $HOME/.emacs.d/ESS/
#             git pull
#             echo; echo
#             ;;
#         * )
#             echo "Skipped."; echo; echo
#             ;;
#     esac 
# else
#     echo ------------------------------------------------------------
#     echo "~/.emacs.d/ESS/ not found."
#     echo "Do you want to clone it? [ y ]"
#     ProgressBar ${step} ${_end}; step=$((step+1))
#     # echo ------------------------------------------------------------
#     read opcao
#     case $opcao in
#         y )
#             echo "Cloning ..."
#             cd $HOME/.emacs.d/
#             git clone https://github.com/emacs-ess/ESS.git
#             echo; echo
#             ;;
#         * )
#             echo "Skipped."; echo; echo
#             ;;
#     esac 
# fi

##----------------------------------------------------------------------
## Polymode to edit R MarkDown and related files.

dir="$HOME/.emacs.d/polymode/.git/"
if [ -d "$dir" ]
then
    echo ------------------------------------------------------------
    echo "~/.emacs.d/polymode/ repository found."
    echo "Do you want update this repository? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Updating ...";
            cd $HOME/.emacs.d/polymode/
            git pull
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
else
    echo ------------------------------------------------------------
    echo "~/.emacs.d/polymode/ not found."
    echo "Do you want to clone it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Cloning ..."
            cd $HOME/.emacs.d/
            git clone https://github.com/vitoshka/polymode.git
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
fi

##----------------------------------------------------------------------
## Auto-complete for Emacs.

## http://cx4a.org/software/auto-complete/
## http://www.emacswiki.org/emacs/ESSAuto-complete

# dir="$HOME/.emacs.d/auto-complete/.git/"
# if [ -d "$dir" ]
# then
#     echo ------------------------------------------------------------
#     echo "~/.emacs.d/auto-complete/ repository found."
#     echo "Do you want update this repository? [ y ]"
#     ProgressBar ${step} ${_end}; step=$((step+1))
#     # echo ------------------------------------------------------------
#     read opcao
#     case $opcao in
#         y )
#             echo "Updating ...";
#             cd $HOME/.emacs.d/auto-complete/
#             git pull
#             cd $HOME/.emacs.d/popup-el/
#             git pull
#             cp -v $HOME/.emacs.d/popup-el/popup.el $HOME/.emacs.d/lisp/
#             echo; echo
#             ;;
#         * )
#             echo "Skipped."; echo; echo
#             ;;
#     esac 
# else
#     echo ------------------------------------------------------------
#     echo "~/.emacs.d/auto-complete/ not found."
#     echo "Do you want to clone it? [ y ]"
#     ProgressBar ${step} ${_end}; step=$((step+1))
#     # echo ------------------------------------------------------------
#     read opcao
#     case $opcao in
#         y )
#             echo "Cloning ..."
#             cd $HOME/.emacs.d/
#             git clone https://github.com/auto-complete/auto-complete.git
#             git clone https://github.com/auto-complete/popup-el.git
#             cp -v $HOME/.emacs.d/popup-el/popup.el $HOME/.emacs.d/
#             echo; echo        
#             ;;
#         * )
#             echo "Skipped."; echo; echo
#             ;;
#     esac 
# fi

##----------------------------------------------------------------------
## Bookmark-plus for Emacs.

dir="$HOME/.emacs.d/bookmark-plus/.git/"
if [ -d "$dir" ]
then
    echo ------------------------------------------------------------
    echo "~/.emacs.d/bookmark-plus/ repository found."
    echo "Do you want update this repository? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Updating ...";
            cd $HOME/.emacs.d/bookmark-plus/
            git pull
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
else
    echo ------------------------------------------------------------
    echo "~/.emacs.d/bookmark-plus/ not found."
    echo "Do you want to clone it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Cloning ..."
            cd $HOME/.emacs.d/
            git clone https://github.com/emacsmirror/bookmark-plus.git
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
fi

##----------------------------------------------------------------------
## Visible bookmarks in Emacs.
## https://github.com/joodland/bm

dir="$HOME/.emacs.d/bm/.git/"
if [ -d "$dir" ]
then
    echo ------------------------------------------------------------
    echo "~/.emacs.d/bm/ repository found."
    echo "Do you want update this repository? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Updating ...";
            cd $HOME/.emacs.d/bm/
            git pull
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
else
    echo ------------------------------------------------------------
    echo "~/.emacs.d/bm/ not found."
    echo "Do you want to clone it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Cloning ..."
            cd $HOME/.emacs.d/
            git clone https://github.com/joodland/bm.git
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac 
fi

##----------------------------------------------------------------------
## Folding code blocks based on indentation.
## https://github.com/zenozeng/yafolding.el.git

dir="$HOME/.emacs.d/yafolding.el/.git/"
if [ -d "$dir" ]
then
    echo ------------------------------------------------------------
    echo "~/.emacs.d/yafolding.el/ repository found."
    echo "Do you want update this repository? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Updating ...";
            cd $HOME/.emacs.d/yafolding.el/
            git pull
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac
else
    echo ------------------------------------------------------------
    echo "~/.emacs.d/yafolding.el/ not found."
    echo "Do you want to clone it? [ y ]"
    ProgressBar ${step} ${_end}; step=$((step+1))
    # echo ------------------------------------------------------------
    read opcao
    case $opcao in
        y )
            echo "Cloning ..."
            cd $HOME/.emacs.d/
            git clone https://github.com/zenozeng/yafolding.el.git
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac
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
    # echo ------------------------------------------------------------
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
    # echo ------------------------------------------------------------
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
    # echo ------------------------------------------------------------
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
