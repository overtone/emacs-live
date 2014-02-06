# [Floobits](https://floobits.com/) plugin for Emacs

Real-time collaborative editing. Think Etherpad, but with native editors. This is the plugin for Emacs. We also have plugins for [Sublime Text](https://github.com/Floobits/floobits-sublime) and [Vim](https://github.com/Floobits/floobits-vim).

### Development status: new, but reasonably stable. We've tested it on Emacs 23 and later, but earlier versions might work. This plugin requires Python 2.7.

## Installation
### MELPA
If you have [MELPA](https://github.com/milkypostman/melpa), simply `M-x package-install <RET> floobits <RET>`

### Manual installation
* `cd ~/.emacs.d/`
* `git clone https://github.com/Floobits/floobits-emacs.git floobits`
* Add Floobits to your `~/.emacs`: `(load "~/.emacs.d/floobits/floobits.el")`


## Initial set-up

* [Create a Floobits account](https://floobits.com/signup/) or [sign in with GitHub](https://floobits.com/login/github/?next=/dash/).
* Set up your `~/.floorc`. You can find a personalized `~/.floorc` on [your settings page](https://floobits.com/dash/settings/). A typical `~/.floorc` looks like this:

```
username myuser
api_key user_kr30t28e
secret gii9Ka8aZei3ej1eighu2vi8D
vim_executable mvim
```


## Usage

All commands are documented in `apropos-command <RET> floobits`
<ul>
  <li><code>M-x floobits-join-workspace &lt;RET&gt; https://floobits.com/owner/workspace/ &lt;RET&gt;</code> &nbsp; Join an existing floobits workspace.</li>
  <li><code>M-x floobits-share-dir &lt;RET&gt; DIR &lt;RET&gt;</code> &nbsp; Create a workspace and populate it with the contents of the directory, DIR (or make it).</li>
  <li><code>M-x floobits-leave-workspace &lt;RET&gt;</code> &nbsp; Leave the current workspace.</li>
  <li><code>M-x floobits-summon &lt;RET&gt;</code> &nbsp; Summon everyone in the workspace to your cursor position.</li>
  <li><code>M-x floobits-follow-mode-toggle &lt;RET&gt;</code> &nbsp; Toggle following of recent changes.</li>
  <li><code>M-x floobits-clear-highlights &lt;RET&gt;</code> &nbsp; Clears all mirrored highlights.</li>
</ul>
