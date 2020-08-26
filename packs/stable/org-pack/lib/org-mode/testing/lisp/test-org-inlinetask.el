;;; test-org-inlinetask.el --- Tests for org-inlinetask.el

;; Copyright (c)  Marco Wahl
;; Authors: Marco Wahl

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comments:

;; Tests for org-inlinetask.el.

;;; Code:

(require 'org-inlinetask)


;;; Test movement

(ert-deftest test-org-inlinetask/org-inlinetask-goto-end ()
  ;; Goto end.
  (should
   (equal
    (let ((org-inlinetask-min-level 5)
  	  (org-adapt-indentation t))
      (org-test-with-temp-text
	  "** H
<point>***** I
***** END
foo"
	(org-inlinetask-goto-end)
	(insert "<point>")
	(buffer-string)))
    "** H
***** I
***** END
<point>foo"))

  ;; Goto end.  End is buffer end.
  (should
   (equal
    (let ((org-inlinetask-min-level 5)
  	  (org-adapt-indentation t))
      (org-test-with-temp-text
	  "** H
<point>***** I
***** END"
	(org-inlinetask-goto-end)
	(insert "<point>")
	(buffer-string)))
    "** H
***** I
***** END<point>"))

  ;; Goto end.  Starting somewhere.
  (should
   (equal
    (let ((org-inlinetask-min-level 5)
  	  (org-adapt-indentation t))
      (org-test-with-temp-text
	  "** H
****<point>* I
***** END
***** I
***** END"
	(org-inlinetask-goto-end)
	(insert "<point>")
	(buffer-string)))
    "** H
***** I
***** END
<point>***** I
***** END"))

  (should
   (equal
    (let ((org-inlinetask-min-level 5)
  	  (org-adapt-indentation t))
      (org-test-with-temp-text
	  "** H
***** I
<point> inside
***** END
***** I
***** END"
	(org-inlinetask-goto-end)
	(insert "<point>")
	(buffer-string)))
    "** H
***** I
 inside
***** END
<point>***** I
***** END")))

(ert-deftest test-org-inlinetask/inlinetask-within-plain-list ()
  "Fold inlinetasks in plain-lists.
Report:
http://lists.gnu.org/archive/html/emacs-orgmode/2017-12/msg00502.html"
  (should
   (org-test-with-temp-text
       "* Test
<point>- x
  - a
*************** List folding stopped here
*************** END
  - b
"
     (org-cycle-internal-local)
     (invisible-p (1- (search-forward "- b"))))))

(ert-deftest test-org-inlinetask/folding-directly-consecutive-tasks/0 ()
  "Fold directly consecutive inlinetasks."
  (should
   (org-test-with-temp-text
       "* Test
<point>- x
  - a
*************** List folding stopped here
*************** END
*************** List folding stopped here
*************** END
  - b
"
     (org-cycle-internal-local)
     (invisible-p (1- (search-forward "- b"))))))

(ert-deftest test-org-inlinetask/folding-directly-consecutive-tasks/1 ()
  "Fold directly consecutive inlinetasks."
  (should
   (org-test-with-temp-text
       "<point>* Test
*************** p1
p2
*************** END
*************** p3
p4
*************** END

"
     (org-flag-subtree t)
     (org-cycle)
     (and
      (not (invisible-p (1- (search-forward "p1"))))
      (invisible-p (1- (search-forward "p2")))
      (not (invisible-p (1- (search-forward "p3"))))
      (invisible-p (1- (search-forward "p4")))))))



(provide 'test-org-inlinetask)

;;; test-org-inlinetask.el ends here
