# Centaur Emacs Customize #

## Quick Start ##

-------------------------------------------------------------------------------

### Install ###
```
git clone https://github.com/guodongsoft/.emacs.d.customize.git ~/.emacs.d.customize
```
-------------------------------------------------------------------------------

### Create custom link file ###
```
rm  ~/.emacs.d/custom.el
ln -s  ~/.emacs.d.customize/custom.el ~/.emacs.d/custom.el
ln -s  ~/.emacs.d.customize/custom-post.el ~/.emacs.d/custom-post.el
```

-------------------------------------------------------------------------------

## Go ##

- need install Go 1.11 or above

``` shell
go get -u -v golang.org/x/tools/cmd/cover
go get -u -v golang.org/x/tools/cmd/guru
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v golang.org/x/tools/cmd/goimports
go get -u -v golang.org/x/tools/cmd/godoc
go get -u -v github.com/derekparker/delve/cmd/dlv
go get -u -v github.com/fatih/gomodifytags
go get -u -v github.com/k0kubun/pp
go get -u -v github.com/motemen/gore
go get -u -v github.com/nsf/gocode
go get -u -v github.com/jstemmer/gotags
go get -u -v github.com/rogpeppe/godef
go get -u -v github.com/sourcegraph/go-langserver
go get -u -v github.com/golang/lint/golint
gocode set autobuild true

git clone https://github.com/saibing/bingo.git
cd bingo
GO111MODULE=on go install
```

-------------------------------------------------------------------------------

## JS ##

``` shell
npm install -g eslint 
npm install -g prettier
npm install -g typescript
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
