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
# Install Emacs-24.5.

function installemacs24.5 {

    if which emacs24 >/dev/null
    then
        MSG="$(emacs24 --version | head -n 1) is installed. Do you want reinstall it? [y]es/[n]o"
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
            echo "Running \"apt-get build-dep emacs24\""
            sudo apt-get build-dep emacs24 -y
            echo
            echo "Running \"apt-get build-dep emacs24 emacs-goodies-el\""
            sudo apt-get install emacs24 emacs-goodies-el -y
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

    dotemacs="$HOME/.emacs"
    if [ -f "$dotemacs" ]
    then
        echo ------------------------------------------------------------
        echo "~/.emacs file found."
        echo "Do you want replace it? [y]es/[q]uit"
        read opcao
        case $opcao in
            y )
                cd $HOME/repos/emacs/
                cp -v dotemacs.el ~/.emacs
                cp -v funcs.el ~/.emacs.d/lisp/
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
        cd $HOME/repos/emacs/
        cp -v dotemacs.el ~/.emacs
        cp -v funcs.el ~/.emacs.d/lisp/
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
        cd $HOME/.emacs.d/lisp/
        wget -N 'http://www.emacswiki.org/emacs/download/essh.el'
        echo; echo
    fi
}

#-----------------------------------------------------------------------
# Electric-spacings.

function moveelectricspacing {
    file="$HOME/.emacs.d/lisp/electric-spacing.el"
    if [ -f "$file" ]
    then
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/electric-spacings.el file found."
        echo "Do you want update it? [y]es/[q]uit"
        read opcao
        case $opcao in
            y )
                if [ ! -f ~/repos/electric-spacing/ess-electric-spacing.el ]
                then
                    echo "File ess-electric-spacing.el no found!"
                    echo "Check if directory exists and in on correct branch!"
                else
                    cp -v ~/repos/electric-spacing/ess-electric-spacing.el \
                       ~/.emacs.d/lisp/electric-spacing.el
                fi
                ;;
            * )
                echo "Skipped."; echo; echo
                ;;
        esac
    else
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/ess-electric-spacing.el file not found."
        echo "It will be created."
        if [ ! -f ~/repos/electric-spacing/ess-electric-spacing.el ]
        then
            echo "File ess-electric-spacing.el no found!"
            echo "Check if directory exists and in on correct branch!"
        else
            cp -v ~/repos/electric-spacing/ess-electric-spacing.el \
               ~/.emacs.d/lisp/electric-spacing.el
        fi
        echo; echo
    fi
}

#----------------------------------------------------------------------
# Cicle among options.

while :
do
    printf "\nMenu of options\n\n"
    printf "  1. Install GNU Emacs 24.5.1\n"
    printf "  2. Move .emacs and .emacs.d/.\n"
    printf "  3. Download and move essh.el.\n"
    printf "  4. Move electric-spacing.el.\n"
    printf "  5. Open files with meld.\n"
    printf "  q. Quit.\n\n"

    read -sn1 -p "Select (1, 2, 3, 4, 5, q): " input
    echo

    case $input in
        1) installemacs24.5 ;;
        2) moveemacsfiles ;;
        3) downloadessh ;;
        4) moveelectricspacing ;;
        5) meld dotemacs.el ~/.emacs ;;
        q) break ;;
        *) echo "Invalid seletion" ;;
    esac
done

#----------------------------------------------------------------------
# End of session.

cat << EOF

------------------------------------------------------------------------

  You have finished the customization of Emacs. Congratulations!

------------------------------------------------------------------------
EOF
