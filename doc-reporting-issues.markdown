---

title: Reporting Issues
filename: reporting-issues
layout: doc

---

# --> Reporting Issues

                _.-^^---....,,--
            _--                  --_
           <          SONIC         >)
           |       BOOOOOOOOM!       |
            \._                   _./
               ``-- . , ; .--'''-'
                     | |   |
                  .-=||  | |=-.
                  `-=#$%&%$#=-'
                     | ;  :|
            _____.,-#%&$@%#&#~,._____

Emacs Live weaves together a lot of different functionality, and while a lot of care has been taken to ensure a seamless union, no code is error free. If you have found some behaviour that you think is incorrect, then please consider submitting a [GitHub Issue](https://github.com/overtone/emacs-live/issues). The following should help you make the issue report as useful as possible.

# --> Remove any pre-compiled code

Sometimes errors can occur because there's a mismatch between some pre-compiled code and the normal elisp code. You should therefore remove any pre-compiled code to verify that this isn't the cause of the issue. On Linux/Max OS X this can be achieved with:

    rm -rf ~/.emacs.d/**/*.elc

# --> Emacs Live Safe Mode

Before submitting your issue, please try to reproduce it in safe mode. This can be entered by starting emacs with the safe mode flag:

    emacs --live-safe-mode

This will then only load the default packs. If the error no longer occurs, then the problem is probably in a pack that you are loading yourself. If it still occurs, then it's a bug in Emacs Live.

# --> Obtain a stack trace

If the error occurs during start up, then you should start Emacs in debug mode to get a full stack trace of the error:

    emacs --debug-init

# --> Writing a good issue report

Here are some guidelines which can help your issue be tracked and dealt with quickly and effectively:

* Please read through the list of current issues to ensure you're not creating a duplicate.
* Try to be very specific and clear about the issue - what did you expect and what did it do?
* Provide the simplest and most foolproof route to reproducing the error.
* Provide details about your system. At least: OS, Emacs version, Emacs Live version.

# --> Thank You

Thanks for spending the time to write a good issue report. Your time makes it easier to find and fix the issues resulting with better software for everyone.
