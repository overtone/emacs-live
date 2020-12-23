;;; ob-arduino.el --- Org-mode Babel support for Arduino.
;;
;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "24.4") (org "24.1"))
;; Package-Version: 1.0
;; Keywords: arduino org babel
;; homepage: https://github.com/stardiviner/arduino-mode
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; 
;; Like the following src block, press =[C-c C-c]= to upload to Arduino board.
;; 
;; #+begin_src arduino
;; // the setup function runs once when you press reset or power the board
;; void setup() {
;;   // initialize digital pin LED_BUILTIN as an output.
;;   pinMode(LED_BUILTIN, OUTPUT);
;; }
;; 
;; // the loop function runs over and over again forever
;; void loop() {
;;   digitalWrite(LED_BUILTIN, HIGH);   // turn the LED on (HIGH is the voltage level)
;;   delay(100);                       // wait for 0.1 second
;;   digitalWrite(LED_BUILTIN, LOW);    // turn the LED off by making the voltage LOW
;;   delay(100);                       // wait for 0.1 second
;; }
;; #+end_src
;;
;;; Code:

(require 'org)
(require 'ob)
(require 'arduino-mode)

(defgroup ob-arduino nil
  "org-mode blocks for Arduino."
  :group 'org)

(defcustom ob-arduino:program "arduino"
  "Default Arduino program name."
  :group 'ob-arduino
  :type 'string)

(defcustom ob-arduino:port "/dev/ttyACM0"
  "Default Arduino port."
  :group 'ob-arduino
  :type 'string)

(defcustom ob-arduino:board "arduino:avr:uno"
  "Default Arduino board."
  :group 'ob-arduino
  :type 'string)


(defvar org-babel-default-header-args:sclang nil)

;;;###autoload
(defun org-babel-execute:arduino (body params)
  "org-babel arduino hook."
  (let* ((port (cdr (assoc :port params)))
         (board (cdr (assoc :board params)))
         (cmd (mapconcat 'identity (list
                                    ob-arduino:program "--upload"
                                    (if port (concat "--port " port))
                                    (if board (concat "--board " board))
                                    ) " "))
         (code (org-babel-expand-body:generic body params))
         (src-file (org-babel-temp-file "ob-arduino-" ".ino")))
    ;; delete all `ob-arduino' temp files, otherwise arduino will compile all
    ;; ob-arduino temp files, and report error.
    (mapc
     (lambda (f)
       (unless (file-directory-p f)
         (delete-file (expand-file-name f org-babel-temporary-directory))))
     (directory-files
      (file-name-directory (org-babel-temp-file "ob-arduino-" ".ino"))
      nil ".ino"))
    ;; specify file for arduino command.
    (with-temp-file src-file
      (insert code))
    (org-babel-eval
     (concat ob-arduino:program
             " " "--upload"
             " " (if port (concat "--port " port))
             " " (if board (concat "--board " board))
             " " src-file)
     "" ; pass empty string "" as `BODY' to `org-babel--shell-command-on-region'
     ;; to fix command `arduino' don't accept string issue.
     )
    ))


;;;###autoload
(eval-after-load 'org
  '(add-to-list 'org-src-lang-modes '("arduino" . arduino)))




(provide 'ob-arduino)

;;; ob-arduino.el ends here
