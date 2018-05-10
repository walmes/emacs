#!/bin/bash

#----------------------------------------------------------------------
# Welcome message.

# ASCII Art. 87487
# http://patorjk.com/software/taag/#p=display&f=Calvin%20S&t=GNU%20Emacs

cat << EOF
------------------------------------------------------------------------

  ╔═╗╔╗╔╦ ╦  ╔═╗┌┬┐┌─┐┌─┐┌─┐
  ║ ╦║║║║ ║  ║╣ │││├─┤│  └─┐
  ╚═╝╝╚╝╚═╝  ╚═╝┴ ┴┴ ┴└─┘└─┘

  This will guide you to install GNU Emacs, his friends and additional
  files to have, at least, a small part of the all power that GNU Emacs
  has. Press ENTER to continue.

------------------------------------------------------------------------
EOF

#----------------------------------------------------------------------
# Install Emacs.

function installEmacs {

    if which emacs >/dev/null
    then
        MSG="$(emacs --version | head -n 1) is installed. Do you want reinstall it? [y]es/[n]o and next"
        echo; echo
    else
        MSG="GNU Emacs isn't installed. Do you want install it? [y]es/[n]o and next"
        echo; echo
    fi

    echo ------------------------------------------------------------
    echo "$MSG"
    read opcao
    case $opcao in
        y )
            echo "Running \"apt-get build-dep emacs\"."
            sudo apt-get build-dep emacs -y
            echo
            echo "Running \"apt-get install emacs\"."
            sudo apt-get install emacs -y
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac

    echo ------------------------------------------------------------
    echo "Install \"emacs-goodies-el\" (a set of useful packages)? [y]es/[n]o and next"
    read opcao
    case $opcao in
        y )
            echo "Installing \"emacs-goodies-el\"."
            sudo apt-get install emacs-goodies-el -y
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac

    echo ------------------------------------------------------------
    echo "Install \"virtualenv\" (Needed for Python auto complete)? [y]es/[n]o and quit"
    read opcao
    case $opcao in
        y )
            echo "Installing \"virtualenv\"."
            sudo apt-get install virtualenv -y
            echo; echo
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac
}

#----------------------------------------------------------------------
# Install packages.

function installPackages {

    echo ------------------------------------------------------------
    echo "Do you want to install GNU Emacs packages listed"
    echo "in \"install-packages.el\" file?" [y]es/[q]uit
    read opcao
    case $opcao in
        y )
            emacs --script install-packages.el
            echo; echo "Packages installed."
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac

    echo "This is the directory tree of the installed packages:"
    tree -L 1 $HOME/.emacs.d/elpa/

}

#----------------------------------------------------------------------
# Move emacs init files and extentions.

function moveEmacsFiles {

    cp -v init.el ~/.emacs.d/init.el
    cp -v funcs.el ~/.emacs.d/lisp/
    emacs --batch --eval '(byte-compile-file "~/.emacs.d/lisp/funcs.el")'

}

function createEmacsFiles {

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
                moveEmacsFiles
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
        moveEmacsFiles
        echo; echo
    fi
}

#----------------------------------------------------------------------
# Send line or region for shell buffer.

function downloadEssh {

    wget -N 'http://www.emacswiki.org/emacs/download/essh.el' -P $HOME/.emacs.d/lisp/
    emacs --batch --eval '(byte-compile-file "~/.emacs.d/lisp/essh.el")'

}

function createEssh {

    file="$HOME/.emacs.d/lisp/essh.el"

    if [ -f "$file" ]
    then
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/essh.el file found."
        echo "Do you want update it? [y]es/[q]uit"
        read opcao
        case $opcao in
            y )
                downloadEssh
                ;;
            * )
                echo "Skipped."; echo; echo
                ;;
        esac
    else
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/essh.el file not found."
        echo "It will be created."
        downloadEssh
        echo; echo
    fi

}

#----------------------------------------------------------------------
# Bookmark+.

function downloadBookmark {

        read opcao
        case $opcao in
            y )
                # https://github.com/emacsmirror/bookmark-plus
                wget -N https://github.com/emacsmirror/bookmark-plus/archive/master.zip -P $HOME/.emacs.d/elpa/
                unzip $HOME/.emacs.d/elpa/master.zip -d $HOME/.emacs.d/elpa/
                mv -v $HOME/.emacs.d/elpa/bookmark-plus-master $HOME/.emacs.d/elpa/bookmark+
                rm -v $HOME/.emacs.d/elpa/master.zip
                # emacs --batch --eval '(byte-recompile-directory "~/.emacs.d/elpa/bookmark+" 0 t)'
                ;;
            * )
                echo "Skipped."; echo; echo
                ;;
        esac

}

function createBookmark {

    hasBookmark=$(ls $HOME/.emacs.d/elpa/ | grep 'bookmark+')

    if [ "$hasBookmark" == "" ];
    then
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/bookmark+ folder not found."
        echo "Do you want download and update it? [y]es/[q]uit"
        downloadBookmark
    else
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/bookmark+ folder found."
        echo "Do you want download and update it? [y]es/[q]uit"
        downloadBookmark
    fi

}

#-----------------------------------------------------------------------
# Electric-spacings.

function downloadElectricSpacing {

    echo "Do you want download it? [y]es/[q]uit"
    read opcao
    case $opcao in
        y )
            wget -N 'https://raw.githubusercontent.com/walmes/electric-spacing/master/electric-spacing-r.el' -P $HOME/.emacs.d/lisp/
            ;;
        * )
            echo "Download skipped."; echo; echo
            break
            ;;
    esac

}

function createElectricSpacing {

    case $opcao in
        y )
            if [ ! -f ~/Projects/electric-spacing/electric-spacing-r.el ]
            then
                echo "Git project file electric-spacing-r.el not found!"
                downloadElectricSpacing
            else
                cp -v ~/Projects/electric-spacing/electric-spacing-r.el \
                   ~/.emacs.d/lisp/electric-spacing-r.el
            fi
            emacs --batch --eval '(byte-compile-file "~/.emacs.d/lisp/electric-spacing-r.el")'
            ;;
        * )
            echo "Skipped."; echo; echo
            ;;
    esac

}

function moveElectricSpacing {

    file="$HOME/.emacs.d/lisp/electric-spacing-r.el"

    if [ -f "$file" ]
    then
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/electric-spacings-r.el file found."
        echo "Do you want update it? [y]es/[q]uit"
        read opcao
        createElectricSpacing
    else
        echo ------------------------------------------------------------
        echo "~/.emacs.d/lisp/electric-spacing-r.el file not found."
        echo "It will be created."
        opcao="y"
        createElectricSpacing
    fi

}

#-----------------------------------------------------------------------
# Remember after installation.

read -r -d '' REMEMBER <<EOM

------------------------------------------------------------------------

    This to do the first time you open GNU Emacs:

    1. Byte compile \`bookmark+\` directory. Run in a LISP buffer:
       (byte-recompile-directory "~/.emacs.d/elpa/bookmark+" 0 t)
    2. Conclude \`jedi\` installation:
       M-x jedi:install-server RET

------------------------------------------------------------------------
EOM

#----------------------------------------------------------------------
# Cicle among options.

while true
do

    printf "\nMenu of options:\n\n"
    printf "  1. Install GNU Emacs, \`emacs-goodies-el\` and \`virtualenv\`\n"
    printf "  2. Install GNU Emacs packages listed in \`install-packages.el\`.\n"
    printf "  3. Move and byte compile \`init.el\` and \`funcs.el\`.\n"
    printf "  4. Download and byte compile \`essh.el\`.\n"
    printf "  5. Download and byte compile \`electric-spacing-r.el\`.\n"
    printf "  6. Download \`bookmark+\`.\n"
    printf "  7. Open \`init.el\` and \`funcs.el\` with Meld.\n"
    printf "  8. List of things to do when open GNU Emacs the first time.\n"
    printf "  q. Quit.\n\n"

    read -sn1 -p "Select (1, 2, 3, 4, 5, 6, 7, 8, q): " input
    echo

    case $input in
        1) installEmacs ;;
        2) installPackages ;;
        3) createEmacsFiles ;;
        4) createEssh ;;
        5) moveElectricSpacing ;;
        6) createBookmark ;;
        7) meld init.el ~/.emacs.d/init.el && meld funcs.el ~/.emacs.d/lisp/funcs.el ;;
        8) echo "$REMEMBER" ;;
        q) break ;;
        *) echo "Invalid option!" ;;
    esac

done

#----------------------------------------------------------------------
# End of session.

cat << EOF

------------------------------------------------------------------------

  You have finished the installation of GNU Emacs. Congratulations!

------------------------------------------------------------------------
EOF
