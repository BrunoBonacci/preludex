[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Gratipay](http://img.shields.io/gratipay/bbatsov.svg)](https://gratipay.com/bbatsov/)

My custom Emacs Prelude
=======================

This is a fork of the popular [Emacs Prelude](https://github.com/bbatsov/prelude) with customizations
for Clojure development.

Prelude is an Emacs distribution that aims to enhance the default
Emacs experience.  Prelude alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

Prelude is compatible **ONLY with GNU Emacs 24.x**. In general you're
advised to always run Prelude with the latest Emacs - currently
**24.4**.

**Table of Contents**

- [Installing Emacs 24](#installing-emacs-24)
- [Installation](#installation)
- [Updating Prelude](#updating-prelude)
    - [Manual update](#manual-update)
        - [Update all bundled packages](#update-all-bundled-packages)
        - [Update Prelude's code](#update-preludes-code)
        - [Restart Prelude](#restart-prelude)
    - [Automatic update](#automatic-update)
- [Enabling additional modules](#enabling-additional-modules)
- [Running](#running)
- [My customizations on top of Prelude](#my-customizations-on-top-of-prelude)
- [Getting to know Prelude](#getting-to-know-prelude)
    - [Keymap](#keymap)
        - [Global](#global)
        - [Prelude Mode](#prelude-mode)
        - [OSX modifier keys](#osx-modifier-keys)
        - [Projectile](#projectile)
        - [Helm](#helm)
        - [Key-chords](#key-chords)
            - [Disabling key-chords](#disabling-key-chords)
- [Automatic package installation](#automatic-package-installation)
    - [Color Themes](#color-themes)
    - [Personalizing](#personalizing)
        - [Disabling whitespace-mode](#disabling-whitespace-mode)
        - [Disable flyspell-mode](#disable-flyspell-mode)
- [Caveats & Pitfalls](#caveats--pitfalls)
    - [Updating bundled packages](#updating-bundled-packages)
    - [Problems with flyspell-mode](#problems-with-flyspell-mode)
    - [Ugly colors in the terminal Emacs version](#ugly-colors-in-the-terminal-emacs-version)
    - [MELPA error on initial startup](#melpa-error-on-initial-startup)
    - [Warnings on arrow navigation in editor buffers](#warnings-on-navigation-in-editor-buffers)
    - [Customized C-a behavior](#customized-c-a-behavior)
    - [Poor ido matching performance on large datasets](#poor-ido-matching-performance-on-large-datasets)
    - [Windows compatibility](#windows-compatibility)
- [Support](#support)
- [License](#License)



## Installing Emacs 24

Obviously to use the Emacs Prelude you have to install Emacs 24
first.

   - Mac OSX see [http://emacsformacosx.com/](http://emacsformacosx.com/)
   - Others platform look at [WikEmacs articles on installing Emacs](http://wikemacs.org/index.php/Installing_Emacs).

## Installation

The easiest way to install it is as folllow:

```bash
# backup installation
mv ~/.emacs.d ~/.emacs.d.`date +%s`
# then clone the repo
git clone https://github.com/BrunoBonacci/my-new-emacs.git ~/.emacs.d
```
Upon the first start it is going to download and compile all require modules,
expect this to last 1-2 minutes. Subsequent start will be very fast.

If you are using Windows, you should check what Emacs thinks the `~` directory is by running Emacs and typing `C-x d ~/<RET>`, and then adjust the command appropriately.

## Updating Prelude

### Manual update

The update procedure is fairly straightforward and consists of 3 steps:

#### Update all bundled packages

Just run <kbd>M-x package-list-packages RET U x</kbd>.

#### Update Prelude's code

```bash
cd path/to/prelude/installation
git pull
```

The `path/to/prelude/installation` is usually `~/.emacs.d` (at least
on Unix systems).

#### Restart Prelude

It's generally a good idea to stop Emacs after you do the update. The
next time Prelude starts it will install any new dependencies (if
there are such).

### Automatic update

Simply run <kbd>M-x prelude-update</kbd> from Emacs itself and restart Emacs afterwards.

## Enabling additional modules

By default I've installed and enabled modules which are common for
Clojure development and Ruby development.

If you want to add more modules please uncomment them into `prelude-modules.el`.

After you've uncommented a module you should either restart Emacs or evaluate the module
`require` expression with <kbd>C-x C-e</kbd>.

## Running

Nothing fancy here. Just start Emacs as usual. Personally I run Emacs
in daemon mode:

```bash
emacs --daemon
```

Afterwards I connect to the server with either a terminal or a GUI
client like this:

```bash
emacsclient -t
emacsclient -c
```

You'd probably do well to put a few aliases in your `.zshrc` (or
`.bashrc`):

```bash
alias e='emacsclient -t'
alias ec='emacsclient -c'
alias vim='emacsclient -t'
alias vi='emacsclient -t'
```

The last two aliases are helpful if you're used to editing files from
the command line using `vi(m)`.

Also you can open a file with cursor on choosen line:

```bash
emacsclient somefile:1234
```

This will open file 'somefile' and set cursor on line 1234.

## My customizations on top of Prelude

* Theme - using 'solarized-dark' from Sanityinc with little improvements
  for readability.

* No scroll bars key-bindings are better way to navigate a file
* Global-auto-revert enabled to make sure that files in the editor
  are always aligned with the files on disk.
* Dired copy across split screens
* Custom Yasnippet
* automatic opening of `.org` files in your $HOME directory
* Use spaces instead of TAB
* Font locking for Clojure symbols like: λ, ƒ, ∈ ... etc
* [Paredit](http://emacswiki.org/emacs/ParEdit) for Clojure development
* [Elfeed](https://github.com/skeeto/elfeed) for consuming RSS/Atom feeds.
  You can add your own feeds to consume by create a file called `.feeds.el` in
  your home direcotry. The file should have the following format
  ```lisp
  ;;; feeds to consume
  (setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "http://www.terminally-incoherent.com/blog/feed/"))

  ```
* [Prodigy](https://github.com/rejeep/prodigy.el) for start/stop local services.
  Prodigy is an excellent tool for managing local services. Very useful when you
  have to start several services for development purposes. You can add you own
  configuration in a file called `.prodigy.el` in your home directory. The file
  should look like this:
  ```lisp
  ;; create prodigy configurtion

  (require 'prodigy)

  ;; reset status
  (setq prodigy-services '())

  ;; add a service
  (prodigy-define-service
    :name "Python app"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6001")
    :cwd "/path/to/my/project"
    :tags '(work)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)
  ```
* [Restclient](https://github.com/pashky/restclient.el) really rocks!!

Here the key bindings added:

Keybinding          | Description
--------------------|------------------------------------------------------------
<kbd>C-RET</kbd>    | Rectangle selection
<kbd>M-SPC</kbd>    | Delete whitespace except one
<kbd>M-s-left</kbd> | Move between windows like in shell's panels
<kbd>M-s-right</kbd>|                    "
<kbd>M-s-up</kbd>   |                    "
<kbd>M-s-down</kbd> |                    "
shell <kbd>up</kbd>   | More natural up/down behaviour in REPL to load previous/next command
shell <kbd>down</kbd> | More natural up/down behaviour in REPL to load previous/next command
REPL <kbd>up</kbd>    | More natural up/down behaviour in REPL to load previous/next command
REPL <kbd>down</kbd>  | More natural up/down behaviour in REPL to load previous/next command
Clojure <kbd>C-S-d</kbd> | Duplicate next `sexp`
Clojure <kbd>C-S-c</kbd> | Copy next `sexp`
Clojure <kbd>C-S-k</kbd> | Kill/Delete next `sexp`
Clojure <kbd>C-;</kbd>| Send last `sexp` to the REPL and evaluate it
Clojure <kbd>C-M-;</kbd>| Send top level `sexp` to the REPL and evaluate it
Clojure <kbd>C-c C-s</kbd>| Open interactive Clojure cheatsheet
<kbd>C-x w</kbd>      | Start [Elfeed](https://github.com/skeeto/elfeed)
<kbd>C-x p</kbd>      | Start [Prodigy](https://github.com/rejeep/prodigy.el)
<kbd>C-x C-/</kbd>    | Jump to dired with focus on current file.
Rest <kbd>C-c C-v</kbd> | Send request at point and show result in popup
<kbd>C-c t</kbd>      | Open terminal
<kbd>C-c p p</kbd>    | Open known projects
<kbd>C-c p s g</kbd>  | Search in current project



## Getting to know Prelude

Certainly the best way to understand how Prelude enhances the default
Emacs experience is to peruse Prelude's source code (which is
obviously written in Emacs Lisp). Understanding the code is not
necessary of course. Prelude includes a `prelude-mode` minor Emacs mode
which collects some of the additional functionality added by
Prelude. It also adds an additional keymap that binds many of those
extensions to keybindings.

### Keymap

#### Global

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-x \\</kbd>   | `align-regexp`
<kbd>C-+</kbd>     | Increase font size(`text-scale-increase`).
<kbd>C--</kbd>     | Decrease font size(`text-scale-decrease`).
<kbd>C-x O</kbd>   | Go back to previous window (the inverse of `other-window` (`C-x o`)).
<kbd>C-^</kbd>     | Join two lines into one(`prelude-top-join-line`).
<kbd>C-x p</kbd>   | Start `proced` (manage processes from Emacs; works only in Linux).
<kbd>C-x m</kbd>   | Start `eshell`.
<kbd>C-x M-m</kbd> | Start your default shell.
<kbd>C-x C-m</kbd> | Alias for `M-x`.
<kbd>M-X</kbd>     | Like `M-x` but limited to commands that are relevant to the active major mode.
<kbd>C-h A</kbd>   | Run `apropos` (search in all Emacs symbols).
<kbd>C-h C-m</kbd> | Display key bindings of current major mode and descriptions of every binding.
<kbd>M-/</kbd>     | Run `hippie-expand` (a replacement for the default `dabbrev-expand`).
<kbd>C-x C-b</kbd> | Open `ibuffer` (a replacement for the default `buffer-list`).
<kbd>F11</kbd>     | Make the window full screen.
<kbd>F12</kbd>     | Toggle the Emacs menu bar.
<kbd>C-x g</kbd>   | Open Magit's status buffer.
<kbd>M-Z</kbd>     | Zap up to char.
<kbd>C-=</kbd>     | Run `expand-region` (incremental text selection).
<kbd>C-a</kbd>     | Run `prelude-move-beginning-of-line`. Read [this](http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/) for details.

#### Prelude Mode

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-c o</kbd>   | Open the currently visited file with an external program.
<kbd>C-c i</kbd>   | Search for a symbol, only for buffers that contain code
<kbd>C-c g</kbd>   | Search in Google for the thing under point (or an interactive query).
<kbd>C-c G</kbd>   | Search in GitHub for the thing under point (or an interactive query).
<kbd>C-c y</kbd>   | Search in YouTube for the thing under point (or an interactive query).
<kbd>C-c U</kbd>   | Search in Duckduckgo for the thing under point (or an interactive query).
<kbd>C-S-RET</kbd> or <kbd>Super-o</kbd> | Insert an empty line above the current line and indent it properly.
<kbd>S-RET</kbd> or <kbd>M-o</kbd> | Insert an empty line and indent it properly (as in most IDEs).
<kbd>C-S-up</kbd> or <kbd>M-S-up</kbd> | Move the current line or region up.
<kbd>C-S-down</kbd> or <kbd>M-S-down</kbd>| Move the current line or region down.
<kbd>C-c n</kbd> | Fix indentation in buffer and strip whitespace.
<kbd>C-c f</kbd> | Open recently visited file.
<kbd>C-M-\\</kbd> | Indent region (if selected) or the entire buffer.
<kbd>C-c u</kbd> | Open a new buffer containing the contents of URL.
<kbd>C-c e</kbd> | Eval a bit of Emacs Lisp code and replace it with its result.
<kbd>C-c s</kbd> | Swap two active windows.
<kbd>C-c D</kbd> | Delete current file and buffer.
<kbd>C-c d</kbd> | Duplicate the current line (or region).
<kbd>C-c M-d</kbd> | Duplicate and comment the current line (or region).
<kbd>C-c r</kbd> | Rename the current buffer and its visiting file if any.
<kbd>C-c t</kbd> | Open a terminal emulator (`ansi-term`).
<kbd>C-c k</kbd> | Kill all open buffers except the one you're currently in.
<kbd>C-c TAB</kbd> | Indent and copy region to clipboard
<kbd>C-c I</kbd> | Open user's init file.
<kbd>C-c S</kbd> | Open shell's init file.
<kbd>C-c . +</kbd> | Increment integer at point. Default is +1.
<kbd>C-c . -</kbd> | Decrement integer at point. Default is -1.
<kbd>C-c . *</kbd> | Multiply integer at point. Default is *2.
<kbd>C-c . /</kbd> | Divide integer at point. Default is /2.
<kbd>C-c . \\</kbd> | Modulo integer at point. Default is modulo 2.
<kbd>C-c . ^</kbd> | Power to the integer at point. Default is ^2.
<kbd>C-c . <</kbd> | Left-shift integer at point. Default is 1 position to the left.
<kbd>C-c . ></kbd> | Right-shift integer at point. Default is 1 position to the right.
<kbd>C-c . #</kbd> | Convert integer at point to specified base. Default is 10.
<kbd>C-c . %</kbd> | Replace integer at point with another specified integer.
<kbd>C-c . '</kbd> | Perform arithmetic operations on integer at point. User specifies the operator.
<kbd>Super-g</kbd> | Toggle between God mode and non-God mode
<kbd>Super-r</kbd> | Recent files
<kbd>Super-j</kbd> | Join lines
<kbd>Super-k</kbd> | Kill whole line
<kbd>Super-m m</kbd> | Magit status
<kbd>Super-m l</kbd> | Magit log
<kbd>Super-m f</kbd> | Magit file log
<kbd>Super-m b</kbd> | Magit blame mode

**Note**: For various arithmetic operations, the prefix `C-c .` only needs to be pressed once for the first operation.
For subsequent operations, only the appropriate operations (i.e. `+`, `-`, `*`, `/`... needs to be pressed).

#### OSX modifier keys

Prelude does not mess by default with the standard mapping of `Command` (to `Super`) and `Option` (to `Meta`).

If you want to swap them add this to your personal config:

```lisp
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
```

You can also temporarily swap them with `C-c w` (`M-x prelude-swap-meta-and-super`).

#### Projectile

Here's a list of functionality provided by [Projectile](https://github.com/bbatsov/projectile):

Keybinding         | Description
-------------------|------------------------------------------------------------
<kbd>C-c p f</kbd> | Display a list of all files in the project. With a prefix argument it will clear the cache first.
<kbd>C-c p d</kbd> | Display a list of all directories in the project. With a prefix argument it will clear the cache first.
<kbd>C-c p T</kbd> | Display a list of all test files(specs, features, etc) in the project.
<kbd>C-c p s g</kbd> | Run grep on the files in the project.
<kbd>M-- C-c p s g</kbd> | Run grep on `projectile-grep-default-files` in the project.
<kbd>C-c p b</kbd> | Display a list of all project buffers currently open.
<kbd>C-c p o</kbd> | Runs `multi-occur` on all project buffers currently open.
<kbd>C-c p r</kbd> | Runs interactive query-replace on all files in the projects.
<kbd>C-c p i</kbd> | Invalidates the project cache (if existing).
<kbd>C-c p R</kbd> | Regenerates the projects `TAGS` file.
<kbd>C-c p k</kbd> | Kills all project buffers.
<kbd>C-c p D</kbd> | Opens the root of the project in `dired`.
<kbd>C-c p e</kbd> | Shows a list of recently visited project files.
<kbd>C-c p s a</kbd> | Runs `ack` on the project. Requires the presence of `ack-and-a-half`.
<kbd>C-c p s s</kbd> | Runs `ag` on the project. Requires the presence of `ag.el`.
<kbd>C-c p a</kbd> | Runs `ack` on the project. Requires the presence of `ack-and-a-half`.
<kbd>C-c p c</kbd> | Runs a standard compilation command for your type of project.
<kbd>C-c p P</kbd> | Runs a standard test command for your type of project.
<kbd>C-c p z</kbd> | Adds the currently visited to the cache.
<kbd>C-c p p</kbd> | Display a list of known projects you can switch to.

Prelude adds an extra keymap prefix `S-p` (`S` stands for
`Super`), so you can use `S-p` instead of `C-c p`.

If you ever forget any of Projectile's keybindings just do a:

<kbd>C-c p C-h</kbd>

#### Helm

Helm is setup according to this guide: [A Package in a league of its own: Helm](http://tuhdo.github.io/helm-intro.html).

You can learn Helm usage and key bindings following the guide. <kbd>C-c h</kbd> is Prelude's default prefix key for Helm.
If you don't remember any key binding, append <kbd>C-h</kbd> after <kbd>C-c h</kbd> for a list of key bindings in Helm.

If you love Helm and want to use Helm globally with enhanced `helm-find-files`, `helm-buffer-lists`..., you will have to also add `(require 'prelude-helm-everywhere)`.
When `prelude-helm-everywhere` is activated, Helm enables these global key bindings:

Key binding        | Description
-------------------|----------------------------------------------
<kbd>M-x</kbd>     | Run [helm-M-x](http://tuhdo.github.io/helm-intro.html#sec-3), an interactive version of <kbd>M-x</kdb>.
<kbd>M-y</kbd>     | Run [helm-show-kill-ring](http://tuhdo.github.io/helm-intro.html#sec-4), shows the content of `kill-ring`.
<kbd>C-x b </kbd>  | Run [helm-mini](http://tuhdo.github.io/helm-intro.html#sec-5), an interactive version of `C-x b` with more features.
<kbd>C-x C-f</kbd> | Run [helm-find-files](http://tuhdo.github.io/helm-intro.html#sec-6), an interactive version of `find-file` with more features.
<kbd>C-h f </kbd>  | Run [helm-apropos](http://tuhdo.github.io/helm-intro.html#sec-13), an interactive version of `apropos-command`.
<kbd>C-h r</kbd>   | Run [helm-info-emacs](http://tuhdo.github.io/helm-intro.html#sec-14), an interactive version of `info-emacs-manual`.
<kbd>C-h C-l </kbd>| Run `helm-locate-library` that can search for locations of any file loaded into Emacs.

This key binding is activated in `shell-mode`:

Key Binding        | Description
-------------------|----------------------------------------------
<kbd>C-c C-l</kbd>     | Run `helm-comint-input-ring` that shows `shell` history using Helm interface.

This key bindings is activated in `eshell-mode`:

Key Binding        | Description
-------------------|----------------------------------------------
<kbd>C-c C-l</kbd>     | Run `helm-eshell-history` that shows `eshell` history using Helm interface.

If you prefer Ido in everywhere, you should not add `prelude-helm-everywhere`, so you can use Helm along with Ido and Prelude's default commands.

You can always reactivate Helm with `(prelude-global-helm-global-mode-on)`.

**NOTICE**: In `helm-M-x`, you have to pass prefix argument *AFTER* you run `helm-M-x`,
because your prefix argument will be displayed in the modeline when in `helm-M-x`
buffer. Passing prefix argument **BEFORE** =helm-M-x= **has no effect**.


#### Key-chords

**Key-chords are available only when the `prelude-key-chord` module has been enabled.**

Keybinding         | Description
-------------------|----------------------------------------------
<kbd>jj</kbd>      | Jump to the beginning of a word(`avy-goto-word-1`)
<kbd>jk</kbd>      | Jump to a character(`avy-goto-char`)
<kbd>jl</kbd>      | Jump to the beginning of a line(`avy-goto-line`)
<kbd>JJ</kbd>      | Jump back to previous buffer(`prelude-switch-to-previous-buffer`)
<kbd>uu</kbd>      | View edits as a tree(`undo-tree-visualize`)
<kbd>xx</kbd>      | Executed extended command(`execute-extended-command`)
<kbd>yy</kbd>      | Browse the kill ring(`browse-kill-ring`)

##### Disabling key-chords

In some cases you may not want to have a key-chord that is defined by prelude,
in which case you can disable the binding in your `personal.el` file by setting
its command to `nil`. For example, to disable the `jj` key-chord add the
following line:

```lisp
(key-chord-define-global "jj" nil)
```

If you're an `evil-mode` user you'll probably do well to disable `key-chord-mode` altogether:

```lisp
(key-chord-mode -1)
```

#### vim emulation

If you want to use vim inside of emacs enable the `prelude-evil` module which provides
support for `evil-mode`.

## Automatic package installation

The default Prelude installation comes with a bare minimum of
functionality. It will however install add-ons for various programming
languages and frameworks on demand. For instance - if you try to open
a `.clj` file `clojure-mode`, `cider` and Prelude's enhanced Lisp
configuration will be installed automatically for you.

You can, of course, install anything you wish manually as well.

### Color Themes

Emacs 24 ships with a new theming facility that effectively renders
the old color-theme package obsolete. Emacs 24 provides a dozen of
built-in themes you can use out-of-the-box by invoking the `M-x
load-theme` command.

[Zenburn](https://github.com/bbatsov/zenburn-emacs) is the default color theme in Prelude, but you can change it
at your discretion. Why Zenburn? I (and lots of hackers around the
world) find it pretty neat for some reason. Personally I find the
default theme pretty tiresome for the eyes, that's why I took that
"controversial" decision to replace it. You can, of course, easily go
back to the default (or select another theme entirely).

To disable Zenburn just put in your personal config the following
line:

```lisp
(disable-theme 'zenburn)
```

Or you can use another theme altogether by adding something in `personal/preload` like:

```lisp
(setq prelude-theme 'solarized-dark)
```

**P.S.** Solarized is not available by default - you'll have to
  install it from MELPA first (`M-x package-install RET
  solarized-theme`).

Finally, if you don't want any theme at all, you can add this to your
`personal/preload`:

```lisp
(setq prelude-theme nil)
```

### Personalizing

Fork the official Prelude repo and add your own touch to it. You're advised to avoid changing stuff outside of the
personal folder to avoid having to deal with git merge conflicts in the future.

If you'd like to add some auto installation of packages in your
personal config use the following code:

```lisp
(prelude-require-packages '(some-package some-other-package))
```

If you require just a single package you can also use:

```lisp
(prelude-require-package 'some-package)
```

#### Preloading personal config

Sometimes you might want to load code before Prelude has started loading. Prelude will automatically preload all
Emacs Lisp files in your `personal/preload` directory. Note that at this point you can't using anything from
Prelude, except a few variables like `prelude-dir`, etc (since nothing is yet loaded).

#### Disabling whitespace-mode

Although `whitespace-mode` is awesome some people might find it too
intrusive. You can disable it in your
personal config with the following bit of code:

```lisp
(setq prelude-whitespace nil)
```

If you like `whitespace-mode` but prefer it to not automatically
cleanup your file on save, you can disable that behavior by setting
prelude-clean-whitespace-on-save to nil in your config file with:

```lisp
(setq prelude-clean-whitespace-on-save nil)
```

The prelude-clean-whitespace-on-save setting can also be set on a
per-file or directory basis by using a file variable or a
.dir-locals.el file.


#### Disable flyspell-mode

If you're not fond of spellchecking on the fly:

```lisp
(setq prelude-flyspell nil)
```

## Caveats & Pitfalls

### Updating bundled packages

Generally it's a good idea to do a package update before running
updating Prelude, since the latest Prelude code might depend on newer
versions of the bundled packages than you would currently have
installed.

If you're doing manual Prelude updates you should always do a package update first.

`M-x package-list-packages RET U x`

That's not necessary if you're using `M-x prelude-update`, since it
will automatically update the installed packages.

### Problems with flyspell-mode

Prelude makes heavy use of the flyspell-mode package for spell
checking of various things. The proper operation of flyspell depends
on the presence of the `aspell` program and an `en` dictionary on your
system. You can install `aspell` and the dictionary on OS X with
`homebrew` like this:

```bash
brew install aspell --with-lang=en
```

On Linux distros - just use your distro's package manager.

### Ugly colors in the terminal Emacs version

If your Emacs looks considerably uglier in a terminal (compared to the
GUI version) try adding this to your `.bashrc` or `.zshrc`:

```bash
export TERM=xterm-256color
```

Source the `.bashrc` file and start Emacs again.

### MELPA error on initial startup

If you get some http connection error related to the MELPA repo
just do a manual `M-x package-refresh-contents` and restart Emacs
afterwards.

### Warnings on arrow navigation in editor buffers

This is not a bug - it's a feature! I firmly believe that the one true
way to use Emacs is by using it the way it was intended to be used (as
far as navigation is concerned at least).

If you'd like to be take this a step further and disable the arrow key navigation
completely put this in your personal config:

```lisp
(setq guru-warn-only nil)
```

To disable `guru-mode` completely add the following snippet to your
personal Emacs config:

```lisp
(setq prelude-guru nil)
```

### Customized C-a behavior

Prelude overrides `C-a` to behave as described
[here](http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/). If
you don't like that simply add this to your personal config:

```lisp
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)
```

### Poor ido matching performance on large datasets

Prelude swaps the default `ido` flex matching with the more powerful
[ido-flx](https://github.com/lewang/flx).

The sorting algorithm `flx` uses is more complex, but yields better results.

On slower machines, it may be necessary to lower `flx-ido-threshold` to
ensure a smooth experience.

```lisp
(setq flx-ido-threshold 1000)
```

You can always disable the improved sorting algorithm all together like this:

```lisp
(flx-ido-mode -1)
```

### Windows compatibility

While everything in Prelude should work fine in Windows, I test it only
with Linux & OSX, so there are Windows related problems from time to
time. This situation will probably improve over time.

## Support

Support is available via the Prelude Google Group <emacs-prelude@googlegroups.com>.
There's also a Freenode channel you can visit - `#prelude-emacs`.

Check out the project's
[issue list](https://github.com/bbatsov/prelude/issues?sort=created&direction=desc&state=open)
a list of unresolved issues. By the way - feel free to fix any of them
and send me a pull request. :-)

## License

Emacs Prelude is developed by [Bozhidar](https://twitter.com/bbatsov)
And my customizations are developed by [Bruno Bonacci](https://twitter.com/BrunoBonacci)

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
