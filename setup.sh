#!/bin/bash

#----------------------------------------------------------------------
# Welcome message.

cat << EOF
------------------------------------------------------------------------

  This will guide you to install Emacs 24.5.1, his friends and
  additional files to have, at least, a small part of the all power that
  Emacs has. Press ENTER to continue.

------------------------------------------------------------------------
EOF

#----------------------------------------------------------------------
# Install Emacs.

# TODO TODO
# git clone https://github.com/emacsmirror/bookmark-plus.git ~/.emacs.d/elpa/bookmark+

function installemacs {

    if which emacs >/dev/null
    then
        MSG="$(emacs --version | head -n 1) is installed. Do you want reinstall it? [y]es/[n]o"
        echo; echo
    else
        MSG="Emacs 24 isn't installed. Do you want install it? [y]es/[n]o"
        echo; echo
    fi

    echo ------------------------------------------------------------
    echo "$MSG"
    read opcao
    case $opcao in
        y )
            echo "Running \"apt-get build-dep emacs\""
            sudo apt-get build-dep emacs -y
            echo
            echo "Running \"apt-get build-dep emacs\""
            sudo apt-get install emacs -y
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac

    echo ------------------------------------------------------------
    echo "Install emacs-goodies-el? [y]es/[n]o"
    read opcao
    case $opcao in
        y )
            echo "Installing emacs-goodies-el (a set of useful packages)."
            sudo apt-get install emacs-goodies-el -y
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac
}

#----------------------------------------------------------------------
# Move emacs init files and extentions.

function moveemacsfiles {

    emacsddir="$HOME/.emacs.d/lisp/"
    if [ ! -d "$emacsddir" ]
    then
        echo ------------------------------------------------------------
        echo "$emacsddir doesn't exists. It will be created."
        mkdir -v -p $HOME/.emacs.d/lisp/
        echo; echo
    else
        echo ------------------------------------------------------------
        echo "$emacsddir exists."
        echo; echo
    fi

    dotemacs="$HOME/.emacs.d/init.el"
    if [ -f "$dotemacs" ]
    then
        echo ------------------------------------------------------------
        echo "~/.emacs.d/init.el file found."
        echo "Do you want replace it? [y]es/[q]uit"
        read opcao
        case $opcao in
            y )
                cd $HOME/Projects/emacs/
                cp -v init.el ~/.emacs.d/init.el
                cp -v funcs.el ~/.emacs.d/lisp/
                echo; echo
                ;;
            * )
                echo "Skipped."; echo; echo
                ;;
        esac
    else
        echo ------------------------------------------------------------
        echo "~/.emacs.d/init.el file not found."
        echo "It will be created."
        cd $HOME/Projects/emacs/
        cp -v init.el ~/.emacs.d/init.el
        cp -v functions.el ~/.emacs.d/lisp/
        echo; echo
    fi
}

#----------------------------------------------------------------------
# Send line or region for shell buffer.

function downloadessh {
    file="$HOME/.emacs.d/lisp/essh.el"
    if [ -f "$file" ]
    then
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/essh.el file found."
        echo "Do you want update it? [y]es/[q]uit"
        read opcao
        case $opcao in
            y )
                wget -N 'http://www.emacswiki.org/emacs/download/essh.el' -P $HOME/.emacs.d/lisp/
                ;;
            * )
                echo "Skipped."; echo; echo
                ;;
        esac
    else
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/essh.el file not found."
        echo "It will be created."
        wget -N 'http://www.emacswiki.org/emacs/download/essh.el' -P $HOME/.emacs.d/lisp/
        echo; echo
    fi
}

#-----------------------------------------------------------------------
# Electric-spacings.

function moveelectricspacing {
    file="$HOME/.emacs.d/lisp/electric-spacing-r.el"
    if [ -f "$file" ]
    then
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/electric-spacings-r.el file found."
        echo "Do you want update it? [y]es/[q]uit"
        read opcao
        case $opcao in
            y )
                if [ ! -f ~/Projects/electric-spacing/electric-spacing-r.el ]
                then
                    echo "File electric-spacing-r.el no found!"
                    echo "Check if directory exists and in on correct branch!"
                else
                    cp -v ~/Projects/electric-spacing/electric-spacing-r.el \
                       ~/.emacs.d/lisp/electric-spacing-r.el
                fi
                ;;
            * )
                echo "Skipped."; echo; echo
                ;;
        esac
    else
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/electric-spacing-r.el file not found."
        echo "It will be created."
        if [ ! -f ~/Projects/electric-spacing/electric-spacing-r.el ]
        then
            echo "File electric-spacing.el no found!"
            echo "Check if directory exists and in on correct branch!"
        else
            cp -v ~/Projects/electric-spacing/electric-spacing-r.el \
               ~/.emacs.d/lisp/electric-spacing-r.el
        fi
        echo; echo
    fi
}

#----------------------------------------------------------------------
# Configure remotes.

function confremotes {
    git remote rm origin
    git remote add origin git@github.com:walmes/emacs.git
    git remote set-url origin --add git@gitlab.c3sl.ufpr.br:walmes/emacs.git
    git remote -v
}

#----------------------------------------------------------------------
# Cicle among options.

while :
do
    printf "\nMenu of options\n\n"
    printf "  1. Install GNU Emacs\n"
    printf "  2. Move init.el and func.el.\n"
    printf "  3. Download and move essh.el.\n"
    printf "  4. Configure remotes.\n"
    printf "  5. Open files with meld.\n"
    printf "  6. Move electric-spacing-r.el.\n"
    printf "  q. Quit.\n\n"

    read -sn1 -p "Select (1,2,3,4,5,6,q): " input
    echo

    case $input in
        1) installemacs ;;
        2) moveemacsfiles ;;
        3) downloadessh ;;
        4) confremotes ;;
        5) meld init.el ~/.emacs.d/init.el && meld funcs.el ~/.emacs.d/lisp/funcs.el;;
        6) moveelectricspacing ;;
        q) break ;;
        *) echo "Invalid seletion" ;;
    esac
done

#----------------------------------------------------------------------
# End of session.

cat << EOF

------------------------------------------------------------------------

  You have finished the installation of Emacs. Congratulations!

------------------------------------------------------------------------
EOF
