
(let ((load-path '(".")))
  (require 'gifcast))

(global-flycheck-mode -1)

(gifcast-animate
 (set-frame-size (window-frame (get-buffer-window)) 40 10)
 (when (get-buffer "Main.hs")
   (kill-buffer "Main.hs"))
 (switch-to-buffer (get-buffer-create "Main.hs"))
 (delete-other-windows)
 (tabbar-mode -1)
 (tool-bar-mode -1)
 (linum-mode -1)
 (message nil)

 (haskell-mode)

 (insert (concat
          "-- | Program entry point\n"
          "main :: IO ()\n"
          "main = do\n"
          "  if flag\n"
          "     then putStrLn \"Flag is True\"\n"
          "     else putStrLn \"Flag is False\"\n"
          ))
 (global-font-lock-mode -1)
 (gifcast-capture)
 (global-font-lock-mode t)
 (gifcast-capture)
 (gifcast-generate "../anim/font-lock.gif")
 (kill-buffer "Main.hs")
 nil)

(gifcast-animate
 (set-frame-size (window-frame (get-buffer-window)) 40 10)
 (when (get-buffer "Main.hs")
   (kill-buffer "Main.hs"))
 (switch-to-buffer (get-buffer-create "Main.hs"))
 (delete-other-windows)
 (tabbar-mode -1)
 (tool-bar-mode -1)
 (linum-mode -1)
 (blink-cursor-mode -1)
 (message nil)

 (haskell-mode)
 (company-mode)
 (setq company-idle-delay 0.01)
 (linum-mode -1)

 (insert (concat
          "{-# LANGUAGE  #-}\n"
          "main :: IO ()\n"
          "main = return ()\n"))
 (goto-char (+ 13 (point-min)))
 (gifcast-keys-async
  ""
  (gifcast-capture)
  (gifcast-keys-async
   "F"
   (gifcast-capture)
   (gifcast-keys-async
    "l"
    (gifcast-capture)
    (gifcast-keys-async
     "e"
     (gifcast-capture)
     (gifcast-keys-async
      (kbd "<down>")
      (gifcast-capture)
      (gifcast-keys-async
       "\C-m"
       (gifcast-capture)
       (gifcast-generate "../anim/company-mode-language-pragma.gif")

       (kill-buffer "Main.hs")))))))
 nil)

(gifcast-animate
 (set-frame-size (window-frame (get-buffer-window)) 70 15)
 (when (get-buffer "Main.hs")
   (kill-buffer "Main.hs"))
 (switch-to-buffer (get-buffer-create "Main.hs"))
 (set-visited-file-name "Main.hs" t)
 (delete-other-windows)
 (tabbar-mode -1)
 (tool-bar-mode -1)
 (linum-mode -1)
 (blink-cursor-mode -1)
 (message nil)

 (haskell-mode)
 (company-mode)
 (setq company-idle-delay 0.01)
 (linum-mode -1)

 (insert (concat
          "\n\n\n"
          "main :: IO ()\n"
          "main = return ()\n"))
 (save-buffer)

 (haskell-process-load-file)
 (sit-for 5)
 (select-window (get-buffer-window "Main.hs"))

 (goto-char (+ 1 (point-min)))
 (gifcast-keys-async
  ""
  (gifcast-capture)
  (gifcast-keys-async
   "import"
   (gifcast-capture)
   (gifcast-keys-async
    " Control."
    (gifcast-capture)
    (gifcast-keys-async
     "A"
     (gifcast-capture)
     (gifcast-keys-async
      "\C-m"
      (gifcast-capture)
      (gifcast-generate "../anim/company-mode-import-statement.gif")

      (haskell-kill-session-process)
      (set-buffer-modified-p nil)
      (kill-buffer "Main.hs"))))))
 nil)
