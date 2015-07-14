# [Floobits](https://floobits.com/) plugin for Emacs

Real-time collaborative editing. Think Etherpad, but with native editors. This is the plugin for Emacs. We also have plugins for [Sublime Text](https://github.com/Floobits/floobits-sublime), [Vim](https://github.com/Floobits/floobits-vim), and [IntelliJ](https://github.com/Floobits/floobits-intellij).

### Development status: Reasonably stable. We've tested it on Emacs 24 and later. Earlier versions probably won't work. This plugin requires Python 2.7 or later.

## Installation
### MELPA
If you have [MELPA](https://github.com/milkypostman/melpa), simply `M-x package-install <RET> floobits <RET>`

### el-get
If you have [el-get](https://github.com/dimitri/el-get), simply `M-x el-get-install <RET> floobits <RET>`. To keep everything up to date, you can add the following line to your `~/.emacs`:

```lisp
(el-get-update-all 1)
```

### Manual installation
* `cd ~/.emacs.d/`
* `git clone https://github.com/Floobits/floobits-emacs.git floobits`
* Add Floobits to your `~/.emacs`: `(load "~/.emacs.d/floobits/floobits.el")`


## Initial set-up

* [Create a Floobits account](https://floobits.com/signup/) or [sign in with GitHub](https://floobits.com/login/github/?next=/dash/).
* Set up your `~/.floorc.json`. You can find a personalized `~/.floorc.json` on [your settings page](https://floobits.com/dash/settings/). A typical `~/.floorc.json` looks like this:

```
{
  "auth": {
    "floobits.com": {
      "username": "your_username",
      "api_key": "your_api_key",
      "secret": "your_api_secret_not_your_password"
    }
  }
}
```

## Usage

All commands are documented in `apropos-command <RET> floobits`
<ul>
  <li><code>M-x floobits-join-workspace &lt;RET&gt; https://floobits.com/owner/workspace/ &lt;RET&gt;</code> &nbsp; Join an existing floobits workspace.</li>
  <li><code>M-x floobits-share-dir-private &lt;RET&gt; DIR &lt;RET&gt;</code> &nbsp; Create a workspace and populate it with the contents of the directory, DIR (or make it).</li>
  <li><code>M-x floobits-share-dir-public &lt;RET&gt; DIR &lt;RET&gt;</code> &nbsp; Create a workspace and populate it with the contents of the directory, DIR (or make it).</li>
  <li><code>M-x floobits-leave-workspace &lt;RET&gt;</code> &nbsp; Leave the current workspace.</li>
  <li><code>M-x floobits-summon &lt;RET&gt;</code> &nbsp; Summon everyone in the workspace to your cursor position.</li>
  <li><code>M-x floobits-follow-mode-toggle &lt;RET&gt;</code> &nbsp; Toggle following of recent changes.</li>
  <li><code>M-x floobits-clear-highlights &lt;RET&gt;</code> &nbsp; Clears all mirrored highlights.</li>
</ul>


## Help

If you have trouble setting up or using this plugin, please [contact us](https://floobits.com/help#support).
