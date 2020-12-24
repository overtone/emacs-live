;;
;;
;; This files is supposed to be run from command line like this:
;;
;; $EMACS -l haskell-gifcasts.el -f gifcast-generate-batch-and-exit
;;

(require 'gifcast)
(require 'company)
(require 'shakespeare-mode)

(setq load-path `("../.." ,@load-path))
(require 'haskell-mode-autoloads)
(require 'haskell-mode)
(require 'haskell-interactive-mode)

(setq debug-on-error t)

(gifcast-animation
 font-lock
 (progn
   (set-frame-size (window-frame (get-buffer-window)) 40 10)
   (when (get-buffer "Main.hs")
     (kill-buffer "Main.hs"))
   (switch-to-buffer (get-buffer-create "Main.hs"))
   (delete-other-windows)
   (tabbar-mode -1)
   (tool-bar-mode -1)
   (linum-mode -1)
   (message nil)
   (scroll-bar-mode -1)

   (haskell-mode)

   (insert (concat
            "-- | Program entry point\n"
            "main :: IO ()\n"
            "main = do\n"
            "  if flag\n"
            "     then putStrLn \"Flag is True\"\n"
            "     else putStrLn \"Flag is False\"\n"
            )))
 (global-font-lock-mode -1)
 (gifcast-capture)
 (global-font-lock-mode t)
 (gifcast-capture)
 (gifcast-generate "../anim/font-lock.gif")
 (kill-buffer "Main.hs"))

(gifcast-animation
 font-lock-types
 (progn
   (set-frame-size (window-frame (get-buffer-window)) 60 10)
   (when (get-buffer "Main.hs")
     (kill-buffer "Main.hs"))
   (switch-to-buffer (get-buffer-create "Main.hs"))
   (delete-other-windows)
   (tabbar-mode -1)
   (tool-bar-mode -1)
   (linum-mode -1)
   (message nil)
   (scroll-bar-mode -1)

   (haskell-mode)

   (insert "data ExampleType a where\n"
           "  ExampleConstructor :: Int -> ExampleType Int\n"
           "\n"
           "fun :: Maybe a -> IO Int\n"
           "fun (Just x) = do\n"
           "  y :: Int <- (Prelude.++ 1) `fmap` readInt (_ :: Proxy Int)\n")
   (custom-set-faces
    '(haskell-type-face ((t :inherit font-lock-function-name-face)))))
 (global-font-lock-mode -1)
 (gifcast-capture)
 (global-font-lock-mode t)
 (gifcast-capture)
 (gifcast-generate "../anim/font-lock-types.gif")
 (kill-buffer "Main.hs"))


(gifcast-animation
 font-lock-quasi-quotes
 (progn
   (set-frame-size (window-frame (get-buffer-window)) 50 10)
   (when (get-buffer "Main.hs")
     (kill-buffer "Main.hs"))
   (switch-to-buffer (get-buffer-create "Main.hs"))
   (delete-other-windows)
   (tabbar-mode -1)
   (tool-bar-mode -1)
   (linum-mode -1)
   (message nil)
   (scroll-bar-mode -1)

   (haskell-mode)

   (insert "html = [shamlet|\n"
           "  $doctype 5\n"
           "  <html>\n"
           "    <head>\n"
           "      <title>#{pageTitle} - My Site\n"
           "        <link rel=stylesheet href=@{Stylesheet}>\n"
           "       |]\n")
   (custom-set-faces
    '(haskell-type-face ((t :inherit font-lock-function-name-face)))))
 (global-font-lock-mode -1)
 (gifcast-capture)
 (global-font-lock-mode t)
 (gifcast-capture)
 (gifcast-generate "../anim/font-lock-quasi-quotes.gif")
 (kill-buffer "Main.hs"))

(gifcast-animation
 company-mode-language-pragma
 (set-frame-size (window-frame (get-buffer-window)) 40 10)
 (progn
   (when (get-buffer "Main.hs")
     (kill-buffer "Main.hs"))
   (switch-to-buffer (get-buffer-create "Main.hs"))
   (delete-other-windows)
   (tabbar-mode -1)
   (tool-bar-mode -1)
   (linum-mode -1)
   (blink-cursor-mode -1)
   (message nil)
   (scroll-bar-mode -1)

   (haskell-mode)
   (interactive-haskell-mode)
   (company-mode)
   (setq company-idle-delay 0.01)
   (linum-mode -1)

   (insert (concat
            "{-# LANGUAGE  #-}\n"
            "main :: IO ()\n"
            "main = return ()\n"))
   (goto-char (+ 13 (point-min))))
 (gifcast-capture)
 (gifcast-keys "F")
 (gifcast-capture)
 (gifcast-keys "l")
 (gifcast-capture)
 (gifcast-keys "e")
 (gifcast-capture)
 (gifcast-keys (kbd "<down>"))
 (gifcast-capture)
 (gifcast-keys "\C-m")
 (gifcast-capture)
 (gifcast-generate "../anim/company-mode-language-pragma.gif")

 (kill-buffer "Main.hs")
 nil)

(gifcast-animation
 company-mode-import-statement
 (progn
   (set-frame-size (window-frame (get-buffer-window)) 70 15)
   (when (get-buffer "Main.hs")
     (kill-buffer "Main.hs"))
   (switch-to-buffer (set-buffer (get-buffer-create "Main.hs")))
   (set-visited-file-name "Main.hs" t)
   (delete-other-windows)
   (tabbar-mode -1)
   (tool-bar-mode -1)
   (linum-mode -1)
   (blink-cursor-mode -1)
   (message nil)
   (scroll-bar-mode -1)

   (haskell-mode)
   (interactive-haskell-mode)
   (company-mode)
   (setq company-idle-delay 0.01)
   (linum-mode -1)

   (insert (concat
            "\n\n\n"
            "main :: IO ()\n"
            "main = return ()\n"))
   (save-buffer)

   (haskell-process-load-file)
   (sit-for 2)
   (select-window (get-buffer-window "Main.hs"))

   (goto-char (+ 1 (point-min))))
 (gifcast-keys "")
 (gifcast-capture)
 (gifcast-keys "import")
 (gifcast-capture)
 (gifcast-keys " Control.")
 (gifcast-capture)
 (gifcast-keys "A")
 (gifcast-capture)
 (gifcast-keys "\C-m")
 (gifcast-capture)
 (gifcast-generate "../anim/company-mode-import-statement.gif")

 (haskell-kill-session-process)
 (set-buffer-modified-p nil)
 (kill-buffer "Main.hs"))

(gifcast-animation
 string-escape-highlight
 (progn
   (set-frame-size (window-frame (get-buffer-window)) 40 5)
   (when (get-buffer "Main.hs")
     (kill-buffer "Main.hs"))
   (switch-to-buffer (set-buffer (get-buffer-create "Main.hs")))
   (delete-other-windows)
   (tabbar-mode -1)
   (tool-bar-mode -1)
   (linum-mode -1)
   (blink-cursor-mode -1)
   (message nil)
   (scroll-bar-mode -1)

   (haskell-mode)
   (linum-mode -1))

 (gifcast-keys "\nmsg = ")
 (gifcast-capture 10)
 (gifcast-keys "\"")
 (gifcast-capture 10)
 (gifcast-keys "Hello")
 (gifcast-capture 10)
 (gifcast-keys "World")
 (gifcast-capture 10)
 (gifcast-keys "\"")
 (gifcast-capture 10)
 (gifcast-keys (kbd "<left><left><left><left><left><left>"))
 (gifcast-capture 10)
 (gifcast-keys "\\")
 (gifcast-capture 10)
 (gifcast-keys "x")
 (gifcast-capture 10)
 (gifcast-keys "20")
 (gifcast-capture 500)
 (gifcast-generate "../anim/string-escape-highlight.gif")

 (kill-buffer "Main.hs")
 nil)

(gifcast-animation
 flyspell-prog-mode
 (progn
   (set-frame-size (window-frame (get-buffer-window)) 70 10)
   (when (get-buffer "Main.hs")
     (kill-buffer "Main.hs"))
   (switch-to-buffer (get-buffer-create "Main.hs"))
   (delete-other-windows)
   (tabbar-mode -1)
   (tool-bar-mode -1)
   (linum-mode -1)
   (message nil)
   (scroll-bar-mode -1)

   (haskell-mode)

   (insert "\n\n\n\n"
           "main = do\n"
           "    putStrLn \"Heskell is a realy nice lanuage\"\n"
           "\n\n\n\n\n\n\n\n\n")
   (goto-char (point-min)))

 (gifcast-capture)

 (gifcast-keys (kbd "M-x"))
 (gifcast-capture)
 (gifcast-keys "flyspell-prog-mode")
 (gifcast-capture)
 (gifcast-keys (kbd "RET"))

 (progn
   (flyspell-prog-mode)
   (flyspell-region (point-min) (point-max)))

 (gifcast-capture)

 (re-search-forward "Heskell")
 (gifcast-capture)
 (gifcast-keys (kbd "M-$"))
 (gifcast-capture)
 (gifcast-keys "0")
 (gifcast-capture)


 (re-search-forward "real")
 (gifcast-capture)
 (gifcast-keys (kbd "M-$"))
 (gifcast-capture)
 (gifcast-keys "0")
 (gifcast-capture)

 (re-search-forward "lan")
 (gifcast-capture)
 (gifcast-keys (kbd "M-$"))
 (gifcast-capture)
 (gifcast-keys "0")
 (gifcast-capture)

 (gifcast-generate "../anim/flyspell-prog-mode.gif")
 (kill-buffer "Main.hs"))
