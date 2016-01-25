#!/bin/bash

#----------------------------------------------------------------------
# Welcome message.

cat << EOF
--------------------------------------------------------------------------

  This will guide you to install Emacs 24.5, his friends and additional
  files to have, at least, a small part of the all power that Emacs
  has. Press ENTER to continue.

--------------------------------------------------------------------------
EOF

#----------------------------------------------------------------------
# Install Emacs-24.5.

function installemacs24.5 {

    if which emacs-24.5 >/dev/null
    then
        MSG="Emacs-24.5 is installed. Do you want reinstall it? [y]es/[n]o"
        echo; echo
    else
        MSG="Emacs-24.5 isn't installed. Do you want install it? [y]es/[n]o"
        echo; echo
    fi

    echo ------------------------------------------------------------
    echo "$MSG"
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

    echo ------------------------------------------------------------
    echo "Install emacs-goodies-el? [y]es/[n]o"
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
}

#----------------------------------------------------------------------
# Make desktop application for Emacs-24.5.

function makedesktopapp {
    echo ------------------------------------------------------------
    echo "Make desktop application for Emacs-24.5.? [y]es/[q]uit"
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
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;text/x-org;text/x-emacs-lisp;text/x-markdown;text/x-r-markdown;text/css;application/x-yaml;text/x-r-doc;text/x-r-source;
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
                cd $HOME/GitLab/emacs/
                cp -v dotemacs.el ~/.emacs
                cp -v funcs.el ~/.emacs.d/lisp/
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
        cd $HOME/GitLab/emacs/
        cp -v dotemacs.el ~/.emacs
        cp -v functions.el ~/.emacs.d/lisp/
        cp -v prelude-packages.el ~/.emacs.d/lisp/
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

#----------------------------------------------------------------------
# Configure remotes.

function confremotes {
    git remote rm origin
    git remote add origin git@github.com:walmes/emacs.git
    git remote set-url origin --add git@gitlab.c3sl.ufpr.br:walmes/emacs.git
    git remote set-url origin --add git@git.leg.ufpr.br:walmes/emacs.git
    git remote -v
}

#----------------------------------------------------------------------
# Cicle among options.

while :
do
    printf "\nMenu of options\n\n"
    printf "  1. Install Emacs-24.5.\n"
    printf "  2. Make desktop application for Emacs-24.5.\n"
    printf "  3. Move .emacs and .emacs.d/.\n"
    printf "  4. Download and move essh.el.\n"
    printf "  5. Configure remotes.\n"
    printf "  6. Open files with meld.\n"
    printf "  q. Quit.\n\n"

    read -sn1 -p "Select (1,2,3,4,5,6,q): " input
    echo

    case $input in
        1) installemacs24.5 ;;
        2) makedesktopapp ;;
        3) moveemacsfiles ;;
        4) downloadessh ;;
        5) confremotes ;;
        6) meld dotemacs.el ~/.emacs ;;
        q) break ;;
        *) echo "Invalid seletion" ;;
    esac
done

#----------------------------------------------------------------------
# End of session.

cat << EOF

--------------------------------------------------------------------------

  You have finished the installation of Emacs. Congratulations!

--------------------------------------------------------------------------
EOF
