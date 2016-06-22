

(defun get-gif-dimensions (filename)
  "Get GIF dimensions, return a cons of (w,h).

Get GIF dimensions directly from binary. Does not need external
tools.


GIF Header

Offset   Length   Contents
  0      3 bytes  \"GIF\"
  3      3 bytes  \"87a\" or \"89a\"
  6      2 bytes  <Logical Screen Width>
  8      2 bytes  <Logical Screen Height>
 10      1 byte   bit 0:    Global Color Table Flag (GCTF)
                  bit 1..3: Color Resolution
                  bit 4:    Sort Flag to Global Color Table
                  bit 5..7: Size of Global Color Table: 2^(1+n)
 11      1 byte   <Background Color Index>
 12      1 byte   <Pixel Aspect Ratio>
 13      ? bytes  <Global Color Table(0..255 x 3 bytes) if GCTF is one>
         ? bytes  <Blocks>
         1 bytes  <Trailer> (0x3b)"
  (interactive "fFile name:")
  (with-current-buffer (get-buffer-create "*GIF*")
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename nil 0 10 t)
    (when (not (looking-at-p "GIF8[79]a"))
      (error "File '%s' is not a GIF" filename))
    (let ((result
           (cons (+ (char-after 7) (* 256 (char-after 8)))
                 (+ (char-after 9) (* 256 (char-after 10))))))
      (if (called-interactively-p)
          (message "Dimensions: %dx%d" (car result) (cdr result)))
      result)))

(defun haskell-manual-fixup-buffer (&optional buffer)
  "Fix contents of HTML from makeinfo in a BUFFER.

Currently it looks for image references and adds an explicit
width and height. GIFs are generate on Retina so their resolution
is double of what it should be. Here we halve it to compensate
dimensions and to keep it crisp when viewed on Retina again."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<img src=\"\\(.*\\)\" alt=\"\\(.*\\)\">" nil t)
        (let* ((filename (match-string-no-properties 1))
               (alttext (match-string-no-properties 2))
               (default-directory (file-name-directory (buffer-file-name)))
               (dim (get-gif-dimensions filename))
               (img (format "<img width=\"%d\" height=\"%d\" src=\"%s\" alt=\"%s\">"
                            (/ (car dim) 2) (/ (cdr dim) 2) filename alttext)))
          (delete-region (match-beginning 0) (match-end 0))
          (insert img))))))

(defun haskell-manual-fixup-file (filename)
  "Run `haskell-manual-fixup-buffer' on a file."
  (interactive "fFile name:")
  (with-temp-buffer
    (insert-file-contents filename t)
    (haskell-manual-fixup-buffer)
    (when (buffer-modified-p)
      (basic-save-buffer))))

(defun haskell-manual-fixups-batch-and-exit ()
  "Run `haskell-manual-fixup-buffer' on files given as arguments.

Should be invoked as:

   emacs -l haskell-manual-fixups.el -f haskell-manual-fixups-batch-and-exit doc/html/*.html"
  (dolist (filename command-line-args-left)
    (haskell-manual-fixup-file filename))
  (kill-emacs 0))
