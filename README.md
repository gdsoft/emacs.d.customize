# Centaur Emacs Customize #

## Quick Start ##

-------------------------------------------------------------------------------

### Install ###
```
git clone https://github.com/guodongsoft/emacs.d.git ~/.emacs.d
git clone https://github.com/guodongsoft/emacs.d.customize.git ~/.emacs.d.customize
```

### Create custom link file ###
```
rm  ~/.emacs.d/custom.el
ln -s  ~/.emacs.d.customize/custom.el ~/.emacs.d/custom.el
ln -s  ~/.emacs.d.customize/custom-post.el ~/.emacs.d/custom-post.el
```

### emacs-libvterm ###

``` shell
cd  ~/.emacs.d.customize
git clone --depth=1 https://github.com/akermu/emacs-libvterm.git

cd emacs-libvterm
mkdir build
cd build
cmake ..
make
```

### sly ###

``` shell
cd  ~/.emacs.d.customize
git clone --depth=1 https://github.com/joaotavora/sly.git
cd sly
make compile compile-contrib

#(add-to-list 'load-path "~/dir/to/cloned/sly")
#(require 'sly-autoloads)
#(setq inferior-lisp-program "/usr/bin/sbcl")
```

-------------------------------------------------------------------------------

## Go ##

- need install Go 1.11 or above

``` shell
go get -u -v golang.org/x/tools/cmd/gotype
go get -u -v golang.org/x/tools/cmd/cover
go get -u -v golang.org/x/tools/cmd/guru
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v golang.org/x/tools/cmd/goimports
go get -u -v golang.org/x/tools/cmd/godoc
go get -u -v github.com/derekparker/delve/cmd/dlv
go get -u -v github.com/fatih/gomodifytags
go get -u -v github.com/k0kubun/pp
go get -u -v github.com/motemen/gore
go get -u -v github.com/jstemmer/gotags
go get -u -v github.com/josharian/impl
go get -u -v github.com/rogpeppe/godef
go get -u -v github.com/golang/dep/cmd/dep
go get -u -v github.com/swaggo/swag/cmd/swag
go get -u -v github.com/sourcegraph/go-langserver
go get -u -v github.com/golang/lint/golint
go get -u -v github.com/justjanne/powerline-go
go get -u -v github.com/cweill/gotests/...
go get -u -v github.com/mdempsky/gocode # or github.com/nsf/gocode
gocode set autobuild true

# ghq
go get -u -v github.com/motemen/ghq
# ghqのルートを${GOPATH}/srcにする
git config --global ghq.root "${GOPATH}/src"
# リポジトリclone
#ghq get .../xxx.git

# bingo (Download from github.com/saibing/bingo)
ghq look bingo
GO111MODULE=on go install
```

-------------------------------------------------------------------------------

## Rust ##

``` shell
rustup update
rustup component add rls rust-analysis rust-src
```

-------------------------------------------------------------------------------

## JS ##

``` shell
npm install -g eslint 
npm install -g prettier
npm install -g typescript
```

-------------------------------------------------------------------------------

## .dir-locals.el ##

``` emacs-lisp
;; For example:
;;  You have a project with the C++ source code in ~/myproject.
;;  Add the file ~/myproject/.dir-locals.el with the following content:

((nil . ((eval . (setq flycheck-clang-include-path
                       (list (expand-file-name "~/myproject/include/")))))))

;; .dir-locals.el's template file
;; copy dir-locals.tmpl to you project directory and rename to .dir-locals.el                       
```

-------------------------------------------------------------------------------

## LSP ##

- C/C++/Objective-C
``` shell
;; install ccls
;; https://github.com/MaskRay/ccls

;; -- generates compile_commands.json and .ccls --
;; Build EAR https://github.com/rizsotto/Bear.git
bear make
;; OR
;; scan-build https://github.com/rizsotto/scan-build.git
intercept-build make
```

- Golang
``` shell
go get -u github.com/sourcegraph/go-langserver
```

- Rust
``` shell
rustup component add rls-preview rust-analysis rust-src
```

- Python
``` shell
pip install python-language-server
```

- Ruby
``` shell
gem install solargraph
```

- Javascript/Typescript
``` shell
sudo yarn add global javascript-typescript-langserver
```

- CSS
``` shell
sudo yarn add global vscode-css-languageserver-bin
```

- HTML
``` shell
sudo yarn add global vscode-html-languageserver-bin
```

- Bash/Shell
``` shell
sudo yarn add global bash-language-server
```

-------------------------------------------------------------------------------

## Projectile ##

- C-c p ?
- C-c p D projectile-dired
- C-c p I projectile-ibuffer
- C-c p S projectile-save-project-buffers
- C-c p T projectile-find-test-file
- C-c p V projectile-browse-dirty-projects
- C-c p c projectile-compile-project
- C-c p d projectile-find-dir
- C-c p e projectile-recentf
- C-c p f projectile-find-file
- C-c p p projectile-switch-project
- C-c p s g projectile-grep
- C-c p s r projectile-ripgrep
- C-c p s s projectile-ag

-------------------------------------------------------------------------------

## ag ##

``` shell
apt-get install silversearcher-ag
brew install the_silver_searcher
```

-------------------------------------------------------------------------------

## Error ##

- If show under error message
- `No word lists can be found for the language "en_US".`

``` shell
# Ubuntu
sudo apt-get install aspell-en

# Arch
sudo yay -S aspell-en
```
-------------------------------------------------------------------------------

## Quicklisp ##

### Install ###

- see quicklisp/Install

### Let Emacs know about SBCL and Quicklisp ###

- First in SBCL run:

```
(ql:quickload "quicklisp-slime-helper")
```

- Tell Emacs how to launch your Lisp environment

```
(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))
```

-------------------------------------------------------------------------------

## TAGS ##

``` shell
find . -name "*.el" -or -name "*.c" | etags -
```

-------------------------------------------------------------------------------
