# Emacs configuration

> Saying that Emacs is just a text editor is like calling iPhone just a
> phone. [(Luke - Terminally Incoherent)](http://www.terminally-incoherent.com/blog/2007/12/13/emacs-with-auctex-as-a-latex-ide/)

> While any text editor can save your files, only Emacs can save your
> soul. (Per Abrahamsen)

> Show me your ~/.emacs and I will tell you who you are. (Bogdan
> Maryniuk)

> The reasonable man adapts himself to Emacs; the unreasonable one
> persists in trying to adapt Emacs to himself. Therefore all progress
> depends on the unreasonable man. (G.B. Shaw)

[Emacs](http://www.gnu.org/software/emacs/tour/) is my favorite text
editor for R, LaTeX, Markdown, Shell, and related files due its high
flexibility and customizability.  Customize means to change the way
something looks or works so that it is exactly what you want or need.
With this in mind, Emacs allows you to customize him to get all of this
power and became more efficient and productive.  Good editors allow the
user choose color schemes, font attributes, has code highlight, proper
code indentation and offer sophisticated ways to do find and replace
strings, for example.  In addition to this key features, with Emacs it
is also possible set key bindings for pre defined functions or even
create new functions.  You can use the Linux terminal inside Emacs and
even play [tetris](http://www.youtube.com/watch?v=5A8knEALaIY), without
mention [the psychoanalyst](http://www.eeggs.com/items/49593.html)
embedded.

Emacs customizations are defined in the `.emacs` file. On Linux systems,
this file is found, by default, in the user home folder.  To me, the
path is `/home/walmes/.emacs`.

To have proper code highlight in R sessions you need to have installed
[ESS](http://ess.r-project.org/).  To work with LaTeX it is recommended
install [AUCTeX](http://www.gnu.org/software/auctex/).  Emacs also have
resources to edit MarkDown (`.md`) and even R+MarkDown (`.Rmd`).  Look
the packages `markdown-mode` and `polymode`.

Most of the content in my `.emacs` file was inspired or copied from the
web.  My first reference was
[Fernando Mayer](https://github.com/fernandomayer/emacs-files)'s Emacs
init file, also available in GitHub.  Queries in
[stackoverflow](http://stackoverflow.com/questions/tagged/emacs) and
mailing list also gave lots of contribution.  The most complete and
sophisticated [`.emacs`](http://pages.sachachua.com/.emacs.d/Sacha.html)
I ever found belongs to [Sasha Chua](http://sachachua.com/blog/).

The purpose of this repository is to keep safe and share my Emacs
configurations files. The `dotemacs.el` file must be renamed to
`.emacs`. The `.el` extension is for GitHub proper highlight.  The
`funcs.el` contains function and key bindings definitions.  It is loaded
by the `.emacs` file.

The `dotemacs.sh` is a bash file that do almost all the setup I need:
install Emacs and his friends, clone repositories, move files to the
right directories and so on.

```bash
# Download of dotemacs.sh.
cd ~/Downloads/
wget https://raw.githubusercontent.com/walmes/emacs2/master/dotemacs.sh

# Give executable permission.
chmod +x dotemacs.sh

# Execute it.
./dotemacs.sh
```

The file `install-packages.el` should be executed in the emacs batch
mode to install some packages.

```bash
# Executes Emacs in th batch mode.
emacs -batch -l install-packages.el
```

All the above instructions are for Ubuntu GNU/Linux. They were made
tested in Ubuntu 14.04 and Linux Mint 17 Quiana. For Windows users, I
recommend the Emacs available from
[Vicent Goulet](https://vigou3.github.io/emacs-modified-windows/) that
already has ESS and AUCTeX. More information for Windows users available
in [John Cook](http://www.johndcook.com/emacs_windows.html)'s blog.
