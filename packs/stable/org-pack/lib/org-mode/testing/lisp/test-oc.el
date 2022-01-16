;;; test-oc.el --- Tests for Org Cite library        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Nicolas Goaziou

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'oc)
(require 'ox)
;; We need `org-test-with-parsed-data' macro.
(require 'test-ox "../testing/lisp/test-ox.el")

(ert-deftest test-org-cite/register-processor ()
  "Test `org-cite-register-processor'."
  ;; Default test.
  (should
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name)))
  ;; Handle duplicate processor.
  (should
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name)
     (org-cite-register-processor 'name)))
  ;; Invalid name type.
  (should-error (org-cite-register-processor "name"))
  ;; Unknown property.
  (should-error
   (let ((org-cite--processors nil))
     (org-cite-register-processor :foo 'bar))))

(ert-deftest test-org-cite/unregister-processor ()
  "Test `org-cite-unregister-processor'."
  ;; Default test.
  (should-not
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name)
     (org-cite-unregister-processor 'name)
     org-cite--processors))
  ;; Error out with an unknown processor.
  (should-error
   (let ((org-cite--processors nil))
     (org-cite-unregister-processor 'name))))

(ert-deftest test-org-cite/inside-footnote-p ()
  "Test `org-cite-inside-footnote-p'."
  ;; Regular tests.
  (should
   (org-test-with-parsed-data "[fn:1] <point>[cite:@key]"
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t))))
  (should
   (org-test-with-parsed-data "[fn::<point>[cite:@key]]"
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t))))
  (should-not
   (org-test-with-parsed-data "[cite:@key]"
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t))))
  (should
   (org-test-with-parsed-data "[fn:1] Text.<point>[cite:@key]"
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t))))
  (should
   (org-test-with-parsed-data "[fn:1] <point>[cite:@key]\n: fixed width"
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t))))
  (should
   (org-test-with-parsed-data "[fn:1] <point>[cite:@key]  "
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t))))
  ;; Test optional argument.
  (should
   (org-test-with-parsed-data "[fn:1] <point>[cite:@key]"
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t)
      t)))
  (should-not
   (org-test-with-parsed-data "[fn:1] <point>See [cite:@key]."
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t)
      t)))
  (should
   (org-test-with-parsed-data "[fn::<point>[cite:@key]]"
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t)
      t)))
  (should-not
   (org-test-with-parsed-data "[fn::<point>See [cite:@key].]"
     (org-cite-inside-footnote-p
      (org-element-map tree 'citation #'identity info t)
      t))))

(ert-deftest test-org-cite/processor-has-capability-p ()
  "Test `org-cite-processor-has-capability-p'."
  ;; Unknown capability error.
  (should-error
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name :activate #'ignore)
     (org-cite-processor-has-capability-p 'name 'unknown)))
  ;; Test `activate' capability.
  (should
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name :activate #'ignore)
     (org-cite-processor-has-capability-p 'name 'activate)))
  (should-not
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name :follow #'ignore)
     (org-cite-processor-has-capability-p 'name 'activate)))
  ;; Test `export' capability.
  (should
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name
       :export-bibliography #'ignore
       :export-citation #'ignore)
     (org-cite-processor-has-capability-p 'name 'export)))
  (should
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name :export-citation #'ignore)
     (org-cite-processor-has-capability-p 'name 'export)))
  (should-not
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name :export-bibliography #'ignore)
     (org-cite-processor-has-capability-p 'name 'export)))
  ;; Test `follow' capability.
  (should
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name :follow #'ignore)
     (org-cite-processor-has-capability-p 'name 'follow)))
  (should-not
   (let ((org-cite--processors nil))
     (org-cite-register-processor 'name :activate #'ignore)
     (org-cite-processor-has-capability-p 'name 'follow)))
  ;; Unknown processors have no capabilities.
  (should-not (org-cite-processor-has-capability-p 'foo 'activate))
  (should-not (org-cite-processor-has-capability-p 'foo 'export))
  (should-not (org-cite-processor-has-capability-p 'foo 'follow)))

(ert-deftest test-org-cite/get-references ()
  "Test `org-cite-get-references'."
  ;; Return a list of citation reference objects.
  (should
   (equal '(citation-reference)
          (org-test-with-temp-text "[cite:@a]"
            (mapcar #'org-element-type
                    (org-cite-get-references (org-element-context))))))
  (should
   (equal '("a")
          (org-test-with-temp-text "[cite:@a]"
            (mapcar (lambda (r) (org-element-property :key r))
                    (org-cite-get-references (org-element-context))))))
  ;; Preserve order of references.
  (should
   (equal '("a" "b")
          (org-test-with-temp-text "[cite:@a;@b]"
            (mapcar (lambda (r) (org-element-property :key r))
                    (org-cite-get-references (org-element-context))))))
  ;; Parse prefix and suffix.
  (should
   (equal '("a" "b")
          (org-test-with-temp-text "[cite:prefix @a suffix;@b]"
            (mapcar (lambda (r) (org-element-property :key r))
                    (org-cite-get-references (org-element-context))))))
  (should
   (equal '(("prefix ") nil)
          (org-test-with-temp-text "[cite:prefix @a suffix;@b]"
            (mapcar (lambda (r) (org-element-property :prefix r))
                    (org-cite-get-references (org-element-context))))))
  (should
   (equal '((" suffix") nil)
          (org-test-with-temp-text "[cite:prefix @a suffix;@b]"
            (mapcar (lambda (r) (org-element-property :suffix r))
                    (org-cite-get-references (org-element-context))))))
  ;; Ignore common prefix and suffix.
  (should
   (equal '("a")
          (org-test-with-temp-text "[cite:common prefix; @a ; common suffix]"
            (mapcar (lambda (r) (org-element-property :key r))
                    (org-cite-get-references (org-element-context))))))
  ;; Preserve buffer positions.
  (should
   (org-test-with-temp-text "[cite:@a] [cite<point>:@b]"
     (= (1+ (point))
        (org-element-property :begin
                              (car (org-cite-get-references (org-element-context)))))))
  ;; Handle citation from a full parse tree.
  (should
   (equal '(1 2)
          (org-test-with-temp-text "[cite:@a] [cite:@a;@b]"
            (org-element-map (org-element-parse-buffer) 'citation
              (lambda (c) (length (org-cite-get-references c)))))))
  ;; Test optional argument.
  (should
   (equal '("a" "b")
          (org-test-with-temp-text "[cite:@a;@b]"
            (org-cite-get-references (org-element-context) t))))
  (should
   (equal '("a" "b")
          (org-test-with-temp-text "[cite:@a;@b]"
            (org-element-map (org-element-parse-buffer) 'citation
              (lambda (c) (org-cite-get-references c t)) nil t)))))

(ert-deftest test-org-cite/key-boundaries ()
  "Test `org-cite-key-boundaries'."
  (should
   (equal "@key"
          (org-test-with-temp-text "[cite:<point>@key]"
            (let ((boundaries (org-cite-key-boundaries (org-element-context))))
              (buffer-substring-no-properties
               (car boundaries)
               (cdr boundaries))))))
  (should
   (equal "@key"
          (org-test-with-temp-text "[cite:<point>prefix @key]"
            (let ((boundaries (org-cite-key-boundaries (org-element-context))))
              (buffer-substring-no-properties
               (car boundaries)
               (cdr boundaries))))))
  (should
   (equal "@key"
          (org-test-with-temp-text "[cite:<point>@key suffix]"
            (let ((boundaries (org-cite-key-boundaries (org-element-context))))
              (buffer-substring-no-properties
               (car boundaries)
               (cdr boundaries))))))
  (should
   (equal "@key"
          (org-test-with-temp-text "[cite:global ;<point>@key]"
            (let ((boundaries (org-cite-key-boundaries (org-element-context))))
              (buffer-substring-no-properties
               (car boundaries)
               (cdr boundaries))))))
  (should
   (equal "@key"
          (org-test-with-temp-text "[cite:<point>@key; global]"
            (let ((boundaries (org-cite-key-boundaries (org-element-context))))
              (buffer-substring-no-properties
               (car boundaries)
               (cdr boundaries)))))))

(ert-deftest test-org-cite/main-affixes ()
  "Test `org-cite-main-affixes'."
  (should
   (equal '(nil . nil)
          (org-test-with-temp-text "[cite:@key]"
            (org-cite-main-affixes (org-element-context)))))
  (should
   (equal '(nil . nil)
          (org-test-with-temp-text "[cite:@key1;@key2]"
            (org-cite-main-affixes (org-element-context)))))
  (should
   (equal '(("pre ") . nil)
          (org-test-with-temp-text "[cite:pre @key]"
            (org-cite-main-affixes (org-element-context)))))
  (should
   (equal '(("pre ") . (" post"))
          (org-test-with-temp-text "[cite:pre @key post]"
            (org-cite-main-affixes (org-element-context)))))
  (should
   (equal '(("pre ") . nil)
          (org-test-with-temp-text "[cite:global pre;pre @key]"
            (org-cite-main-affixes (org-element-context)))))
  (should
   (equal '(nil . (" post"))
          (org-test-with-temp-text "[cite:@key post;global post]"
            (org-cite-main-affixes (org-element-context)))))
  (should
   (equal '(("global pre") . ("global post"))
          (org-test-with-temp-text "[cite:global pre;@key1;@key2;global post]"
            (org-cite-main-affixes (org-element-context)))))
  (should
   (equal '(("global pre") . nil)
          (org-test-with-temp-text "[cite:global pre;pre1 @key1;pre2 @key2]"
            (org-cite-main-affixes (org-element-context)))))
  (should
   (equal '(nil . (" global post"))
          (org-test-with-temp-text "[cite:@key1 post1;@key2 post2; global post]"
            (org-cite-main-affixes (org-element-context))))))

(ert-deftest test-org-cite/supported-styles ()
  "Test `org-cite-supported-styles'."
  ;; Default behavior is to use export processors.
  (should
   (equal '((("")))
          (let ((org-cite--processors nil)
                (org-cite-export-processors '((t test))))
            (org-cite-register-processor 'test :cite-styles '(((""))))
            (org-cite-supported-styles))))
  (should
   (equal '((("foo" "f")) (("")))
          (let ((org-cite--processors nil)
                (org-cite-export-processors '((t test))))
            (org-cite-register-processor 'test
              :cite-styles '((("foo" "f")) ((""))))
            (org-cite-supported-styles))))
  ;; Also support functions generating the list.
  (should
   (equal '((("foo" "f")) (("")))
          (let ((org-cite--processors nil)
                (org-cite-export-processors '((t test))))
            (org-cite-register-processor 'test
              :cite-styles (lambda () '((("foo" "f")) (("")))))
            (org-cite-supported-styles))))
  ;; Explicitly provide a processor.
  (should
   (equal '((("")))
          (let ((org-cite--processors nil))
            (org-cite-register-processor 'test :cite-styles '(((""))))
            (org-cite-supported-styles '(test)))))
  ;; Merge style shortcuts.
  (should
   (equal '((("foo" "f" "g")))
          (let ((org-cite--processors nil))
            (org-cite-register-processor 'test :cite-styles '((("foo" "f"))))
            (org-cite-register-processor 'test2 :cite-styles '((("foo" "g"))))
            (org-cite-supported-styles '(test test2)))))
  ;; Merge style variants.
  (should
   (equal '((("foo") ("bar") ("baz")))
          (let ((org-cite--processors nil))
            (org-cite-register-processor 'test
              :cite-styles '((("foo") ("bar"))))
            (org-cite-register-processor 'test2
              :cite-styles '((("foo") ("baz"))))
            (org-cite-supported-styles '(test test2)))))
  ;; Merge variant shortcuts.
  (should
   (equal '((("foo") ("bar" "b" "c")))
          (let ((org-cite--processors nil))
            (org-cite-register-processor 'test
              :cite-styles '((("foo") ("bar" "b"))))
            (org-cite-register-processor 'test2
              :cite-styles '((("foo") ("bar" "c"))))
            (org-cite-supported-styles '(test test2)))))
  ;; Ignore duplicates.
  (should
   (equal '((("foo") ("bar")))
          (let ((org-cite--processors nil))
            (org-cite-register-processor 'test
              :cite-styles '((("foo") ("bar"))))
            (org-cite-register-processor 'test2
              :cite-styles '((("foo") ("bar"))))
            (org-cite-supported-styles '(test test2)))))
  (should
   (equal '((("foo") ("bar" "b")))
          (let ((org-cite--processors nil))
            (org-cite-register-processor 'test
              :cite-styles '((("foo") ("bar" "b"))))
            (org-cite-register-processor 'test2
              :cite-styles '((("foo") ("bar" "b"))))
            (org-cite-supported-styles '(test test2))))))

(ert-deftest test-org-cite/delete-citation ()
  "Test `org-cite-delete-citation'."
  ;; Error when not on a citation or citation reference.
  (should-error
   (org-test-with-temp-text "Text"
     (org-cite-delete-citation (org-element-context))))
  ;; When argument is a citation, delete it completely.  Manage
  ;; properly blanks around it.
  (should
   (equal ""
          (org-test-with-temp-text "[cite:@key]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal "Before After"
          (org-test-with-temp-text "Before [<point>cite:@key] After"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal "Before After"
          (org-test-with-temp-text "Before [<point>cite:@key]After"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal "Before After"
          (org-test-with-temp-text "Before[<point>cite:@key] After"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  ;; Ensure there is at least a blank to separate consecutive objects.
  (should
   (equal "Before After"
          (org-test-with-temp-text "Before[<point>cite:@key]After"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  ;; Remove trailing blanks when removing the citation.
  (should
   (equal "Before"
          (org-test-with-temp-text "Before[<point>cite:@key]  "
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  ;; Preserve indentation if citation is at the beginning of the line.
  (should
   (equal "  After"
          (org-test-with-temp-text "  [<point>cite:@key] After"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  ;; When the citation is alone on a line, remove the whole line.
  (should
   (equal "Line 1\nLine 3"
          (org-test-with-temp-text "Line 1\n[<point>cite:@key]\nLine 3"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  ;; When there is only one citation reference object, remove the full
  ;; citation.
  (should
   (equal ""
          (org-test-with-temp-text "[cite:@<point>key]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal ""
          (org-test-with-temp-text "[cite:pre @<point>key post]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal ""
          (org-test-with-temp-text "[cite:pre; @<point>key ;post]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  ;; Otherwise, remove the reference, including any affix.
  (should
   (equal "[cite:@before;@after]"
          (org-test-with-temp-text "[cite:@before;@<point>key;@after]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal "[cite:@before;@after]"
          (org-test-with-temp-text "[cite:@before;pre @<point>key post;@after]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal "[cite:@before]"
          (org-test-with-temp-text "[cite:@before;@<point>key]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal "[cite:@before;post]"
          (org-test-with-temp-text "[cite:@before;@<point>key;post]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal "[cite:@after]"
          (org-test-with-temp-text "[cite:@<point>key;@after]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string))))
  (should
   (equal "[cite:pre;@after]"
          (org-test-with-temp-text "[cite:pre;@<point>key;@after]"
            (org-cite-delete-citation (org-element-context))
            (buffer-string)))))

(ert-deftest test-org-cite/list-bibliography-files ()
  "Test `org-cite-list-bibliography-files'."
  (should
   (equal '("/bibliography")
          (org-test-with-temp-text "#+bibliography: /bibliography"
            (let ((org-cite-global-bibliography nil))
              (org-cite-list-bibliography-files)))))
  (should
   (equal '("/bibliography")
          (org-test-with-temp-text "#+bibliography: \"/bibliography\""
            (let ((org-cite-global-bibliography nil))
              (org-cite-list-bibliography-files)))))
  (should
   (equal '("/bibliography" "/other-bibliography")
          (org-test-with-temp-text "#+bibliography: /bibliography"
            (let ((org-cite-global-bibliography '("/other-bibliography")))
              (org-cite-list-bibliography-files)))))
  (should
   (equal '(t)
          (org-test-with-temp-text "#+bibliography: ./bibliography"
            (let ((org-cite-global-bibliography nil))
              (mapcar #'file-name-absolute-p (org-cite-list-bibliography-files))))))
  (should
   (equal '("/bibliographyA" "/bibliographyB")
          (org-test-with-temp-text
              "#+bibliography: /bibliographyA\n#+bibliography: /bibliographyB"
            (let ((org-cite-global-bibliography nil))
              (org-cite-list-bibliography-files)))))
  (should
   (equal '("/bibliographyA")
          (org-test-with-temp-text
              "#+bibliography: /bibliographyA\n#+bibliography: /bibliographyA"
            (let ((org-cite-global-bibliography nil))
              (org-cite-list-bibliography-files))))))

(ert-deftest test-org-cite/bibliography-style ()
  "Test `org-cite-bibliography-style'."
  ;; Extract style from global processor definition.
  (should
   (equal "a"
          (catch :exit
            (org-test-with-temp-text "#+print_bibliography:"
              (let ((org-cite-export-processors '((t . (foo "a" "b"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-bibliography (lambda (_ _ s _ _ _) (throw :exit s))
                  :export-citation #'ignore)
                (org-export-as (org-export-create-backend)))))))
  ;; Extract style from local processor definition.
  (should
   (equal "a"
          (catch :exit
            (org-test-with-temp-text
                "#+cite_export: foo a b\n#+print_bibliography:"
              (let ((org-cite-export-processors nil)
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-bibliography (lambda (_ _ s _ _ _) (throw :exit s))
                  :export-citation #'ignore)
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal "a b"
          (catch :exit
            (org-test-with-temp-text
                "#+cite_export: foo \"a b\" c\n#+print_bibliography:"
              (let ((org-cite-export-processors nil)
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-bibliography (lambda (_ _ s _ _ _) (throw :exit s))
                  :export-citation #'ignore)
                (org-export-as (org-export-create-backend)))))))
  ;; Test priority: first keyword, then local.
  (should
   (equal "local"
          (catch :exit
            (org-test-with-temp-text
                "#+print_bibliography:\n#+cite_export: foo local a\n[cite:@a]"
              (let ((org-cite-export-processors '((t . (foo "global" "b"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-bibliography (lambda (_ _ s _ _ _) (throw :exit s))
                  :export-citation #'ignore)
                (org-export-as (org-export-create-backend)))))))
  ;; Explicit "nil" styles forces default style.
  (should-not
   (catch :exit
     (org-test-with-temp-text
         "#+print_bibliography:\n#+cite_export: foo nil a\n[cite:@a]"
       (let ((org-cite-export-processors '((t . (foo "global" "b"))))
             (org-cite--processors nil))
         (org-cite-register-processor 'foo
           :export-bibliography (lambda (_ _ s _ _ _) (throw :exit s))
           :export-citation #'ignore)
         (org-export-as (org-export-create-backend)))))))

(ert-deftest test-org-cite/bibliography-properties ()
  "Test `org-cite-bibliography-properties'."
  ;; Return nil without properties.
  (should-not
   (org-test-with-parsed-data "#+print_bibliography:"
     (org-element-map tree 'keyword
       #'org-cite-bibliography-properties info t)))
  ;; Regular tests.
  (should
   (equal
    '(:key "value")
    (org-test-with-parsed-data "#+print_bibliography: :key value"
      (org-element-map tree 'keyword
        #'org-cite-bibliography-properties info t))))
  (should
   (equal
    '(:key "value" :key2 "value2")
    (org-test-with-parsed-data "#+print_bibliography: :key value :key2 value2"
      (org-element-map tree 'keyword
        #'org-cite-bibliography-properties info t))))
  ;; Allow empty values.
  (should
   (equal
    '(:key)
    (org-test-with-parsed-data "#+print_bibliography: :key"
      (org-element-map tree 'keyword
        #'org-cite-bibliography-properties info t))))
  (should
   (equal
    '(:key "")
    (org-test-with-parsed-data "#+print_bibliography: :key \"\""
      (org-element-map tree 'keyword
        #'org-cite-bibliography-properties info t))))
  ;; Allow space with double quotes.
  (should
   (equal
    '(:key "space space")
    (org-test-with-parsed-data "#+print_bibliography: :key \"space space\""
      (org-element-map tree 'keyword
        #'org-cite-bibliography-properties info t))))
  ;; Ignore spurious values.
  (should
   (equal
    '(:key "space")
    (org-test-with-parsed-data "#+print_bibliography: :key space space"
      (org-element-map tree 'keyword
        #'org-cite-bibliography-properties info t))))
  ;; Gracefully handle incomplete quotations.
  (should
   (equal
    '(:key "\"space" :key2 "value2")
    (org-test-with-parsed-data "#+print_bibliography: :key \"space :key2 value2"
      (org-element-map tree 'keyword
        #'org-cite-bibliography-properties info t)))))

(ert-deftest test-org-cite/citation-style ()
  "Test `org-cite-citation-style'."
  ;; Extract style from global processor definition.
  (should
   (equal '("b")
          (catch :exit
            (org-test-with-temp-text "[cite:@a]"
              (let ((org-cite-export-processors '((t . (foo "a" "b"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '("b" . "variant")
          (catch :exit
            (org-test-with-temp-text "[cite:@a]"
              (let ((org-cite-export-processors '((t . (foo "a" "b/variant"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  ;; Extract style from local processor definition.
  (should
   (equal '("b")
          (catch :exit
            (org-test-with-temp-text "#+cite_export: foo a b\n[cite:@a]"
              (let ((org-cite-export-processors nil)
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '("b c")
          (catch :exit
            (org-test-with-temp-text "#+cite_export: foo a \"b c\"\n[cite:@a]"
              (let ((org-cite-export-processors nil)
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '("b" . "variant")
          (catch :exit
            (org-test-with-temp-text "#+cite_export: foo a b/variant\n[cite:@a]"
              (let ((org-cite-export-processors nil)
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '("b c" . "variant")
          (catch :exit
            (org-test-with-temp-text
                "#+cite_export: foo a \"b c/variant\"\n[cite:@a]"
              (let ((org-cite-export-processors nil)
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  ;; Extract style from citation itself.
  (should
   (equal '("b")
          (catch :exit
            (org-test-with-temp-text "[cite/b:@a]"
              (let ((org-cite-export-processors '((t . (foo nil nil))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '("b" . "variant")
          (catch :exit
            (org-test-with-temp-text "[cite/b/variant:@a]"
              (let ((org-cite-export-processors '((t . (foo nil nil))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  ;; Test priority: first object, then local.
  (should
   (equal '("object")
          (catch :exit
            (org-test-with-temp-text
                "#+cite_export: foo nil local\n[cite/object:@a]"
              (let ((org-cite-export-processors '((t . (foo nil "global"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '("local")
          (catch :exit
            (org-test-with-temp-text
                "#+cite_export: foo nil local\n[cite:@a]"
              (let ((org-cite-export-processors '((t . (foo nil "global"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  ;; Force default style with "nil".
  (should
   (equal '(nil)
          (catch :exit
            (org-test-with-temp-text
                "#+cite_export: foo nil nil\n[cite:@a]"
              (let ((org-cite-export-processors '((t . (foo nil "global"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '(nil)
          (catch :exit
            (org-test-with-temp-text "[cite/nil:@a]"
              (let ((org-cite-export-processors '((t . (foo nil "global"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  ;; Test variant inheritance.
  (should
   (equal '("local" . "v2")
          (catch :exit
            (org-test-with-temp-text "[cite/local/v2:@a]"
              (let ((org-cite-export-processors '((t . (foo nil "global/v1"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '("global" . "v2")
          (catch :exit
            (org-test-with-temp-text "[cite//v2:@a]"
              (let ((org-cite-export-processors '((t . (foo nil "global/v1"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '(nil . "v2")
          (catch :exit
            (org-test-with-temp-text "[cite/nil/v2:@a]"
              (let ((org-cite-export-processors '((t . (foo nil "global/v1"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend)))))))
  (should
   (equal '("local" . nil)
          (catch :exit
            (org-test-with-temp-text "[cite/local:@a]"
              (let ((org-cite-export-processors '((t . (foo nil "global/v1"))))
                    (org-cite--processors nil))
                (org-cite-register-processor 'foo
                  :export-citation (lambda (_ s _ _) (throw :exit s)))
                (org-export-as (org-export-create-backend))))))))

(ert-deftest test-org-cite/read-processor-declaration ()
  "Test `org-cite-read-processor-declaration'."
  ;; Argument should contain 1-3 tokens.
  (should-error
   (org-cite-read-processor-declaration ""))
  (should
   (equal '(foo nil nil)
          (org-cite-read-processor-declaration "foo")))
  (should
   (equal '(foo "bar" nil)
          (org-cite-read-processor-declaration "foo bar")))
  (should
   (equal '(foo "bar" "baz")
          (org-cite-read-processor-declaration "foo bar baz")))
  (should-error
   (org-cite-read-processor-declaration "foo bar baz qux"))
  ;; nil in second and third arguments is read as `nil'.
  (should
   (equal '(foo nil "baz")
          (org-cite-read-processor-declaration "foo nil baz")))
  (should
   (equal '(foo "bar" nil)
          (org-cite-read-processor-declaration "foo bar nil")))
  ;; Second and third arguments may contain spaces if they are quoted.
  (should
   (equal '(foo "bar baz" nil)
          (org-cite-read-processor-declaration "foo \"bar baz\"")))
  (should
   (equal '(foo "bar" "baz qux")
          (org-cite-read-processor-declaration "foo bar \"baz qux\"")))
  ;; Spurious spaces are ignored.
  (should
   (equal '(foo "bar" "baz")
          (org-cite-read-processor-declaration "  foo   bar    baz  "))))

(ert-deftest test-org-cite/list-citations ()
  "Test `org-cite-list-citations'."
  (should
   (equal '("a")
          (org-test-with-parsed-data "Test [cite:@a]"
            (cl-mapcan (lambda (c)
                         (mapcar (lambda (ref)
                                   (org-element-property :key ref))
                                 (org-element-contents c)))
                       (org-cite-list-citations info)))))
  (should
   (equal '("a" "b")
          (org-test-with-parsed-data "Test [cite:@a] [cite:@b]"
            (cl-mapcan (lambda (c)
                         (mapcar (lambda (ref)
                                   (org-element-property :key ref))
                                 (org-element-contents c)))
                       (org-cite-list-citations info)))))
  (should
   (equal '("a")
          (org-test-with-parsed-data "Test[fn:1]\n[fn:1] [cite:@a]"
            (cl-mapcan (lambda (c)
                         (mapcar (lambda (ref)
                                   (org-element-property :key ref))
                                 (org-element-contents c)))
                       (org-cite-list-citations info)))))
  (should
   (equal '("a" "b")
          (org-test-with-parsed-data "First[cite:@a] Second[fn:1]\n[fn:1] [cite:@b]"
            (cl-mapcan (lambda (c)
                         (mapcar (lambda (ref)
                                   (org-element-property :key ref))
                                 (org-element-contents c)))
                       (org-cite-list-citations info)))))
  (should
   (equal '("b" "a")
          (org-test-with-parsed-data "First[fn:1] Second[cite:@a]\n[fn:1] [cite:@b]"
            (cl-mapcan (lambda (c)
                         (mapcar (lambda (ref)
                                   (org-element-property :key ref))
                                 (org-element-contents c)))
                       (org-cite-list-citations info)))))
  (should
   (equal '("a" "b")
          (org-test-with-parsed-data
              "Text[fn:1][fn:2]\n[fn:1] [cite:@a]\n\n[fn:2] [cite:@b]"
            (cl-mapcan (lambda (c)
                         (mapcar (lambda (ref)
                                   (org-element-property :key ref))
                                 (org-element-contents c)))
                       (org-cite-list-citations info)))))
  (should
   (equal '("b" "a")
          (org-test-with-parsed-data
              "Text[fn:1]\n[fn:1] [fn:2][cite:@a]\n\n[fn:2] [cite:@b]"
            (cl-mapcan (lambda (c)
                         (mapcar (lambda (ref)
                                   (org-element-property :key ref))
                                 (org-element-contents c)))
                       (org-cite-list-citations info)))))
  (should
   (equal '("a" "b")
          (org-test-with-parsed-data
              "Text[fn:1]\n[fn:1] [cite:@a][fn:2]\n\n[fn:2] [cite:@b]"
            (cl-mapcan (lambda (c)
                         (mapcar (lambda (ref)
                                   (org-element-property :key ref))
                                 (org-element-contents c)))
                       (org-cite-list-citations info)))))
  (should
   (equal '("a")
          (org-test-with-parsed-data "Text[fn::[cite:@a]]"
            (cl-mapcan (lambda (c)
                         (mapcar (lambda (ref)
                                   (org-element-property :key ref))
                                 (org-element-contents c)))
                       (org-cite-list-citations info))))))

(ert-deftest test-org-cite/list-keys ()
  "Test `org-cite-list-keys'."
  (should
   (equal '("a")
          (org-test-with-parsed-data "Test [cite:@a]"
            (org-cite-list-keys info))))
  (should
   (equal '("a" "b")
          (org-test-with-parsed-data "Test [cite:@a] [cite:@b]"
            (org-cite-list-keys info))))
  ;; Remove duplicates.
  (should
   (equal '("a")
          (org-test-with-parsed-data "Test [cite:@a] [cite:@a]"
            (org-cite-list-keys info))))
  ;; Keys are ordered by first appearance in the document.
  (should
   (equal '("a" "b")
          (org-test-with-parsed-data "Test [cite:@a] [cite:@b] [cite:@a]"
            (org-cite-list-keys info))))
  (should
   (equal '("a" "b" "c")
          (org-test-with-parsed-data
              "Test [cite:@a][fn:1] [cite:@c] [cite:@a]\n[fn:1] [cite:@b]"
            (org-cite-list-keys info)))))

(ert-deftest test-org-cite/key-number ()
  "Test `org-cite-key-number'."
  (should
   (= 1 (org-test-with-parsed-data "[cite:@key]"
          (org-cite-key-number "key" info))))
  (should
   (equal '(1 2)
          (org-test-with-parsed-data "[cite:@key] [cite:@key2] [cite:@key]"
            (list (org-cite-key-number "key" info)
                  (org-cite-key-number "key2" info)))))
  ;; When "predicate" is nil, keys are sorted by appearance order in
  ;; the buffer.
  (should
   (equal '((1 . "a") (2 . "c") (3 . "b"))
          (org-test-with-parsed-data
              "[cite:@a][fn:1] [cite:@b]\n[fn:1] [cite:@c]"
            (sort (mapcar (lambda (key)
                            (cons (org-cite-key-number key info) key))
                          '("a" "b" "c"))
                  #'car-less-than-car))))
  (should
   (equal '((1 . "a") (2 . "b") (3 . "c"))
          (org-test-with-parsed-data
              "[cite:@a][fn:1] [cite:@b]\n[fn:1] [cite:@c]"
            (sort (mapcar (lambda (key)
                            (cons (org-cite-key-number key info #'string<) key))
                          '("a" "b" "c"))
                  #'car-less-than-car)))))

(ert-deftest test-org-cite/wrap-citation ()
  "Test `org-cite-wrap-citation'."
  ;; Reference test.
  (should
   (org-test-with-parsed-data "[cite:@key]"
     (org-element-map tree 'citation
       (lambda (c)
         (org-cite-wrap-citation c info)
         (org-cite-inside-footnote-p c))
       info)))
  ;; Created footnote is anonymous.
  (should-not
   (org-test-with-parsed-data "[cite:@key]  "
     (org-element-map tree 'citation
       (lambda (c)
         (org-cite-wrap-citation c info)
         (org-element-property :label (org-cite-inside-footnote-p c)))
       info)))
  ;; Created footnote is inline.
  (should
   (equal '(inline)
          (org-test-with-parsed-data "[cite:@key]"
            (org-element-map tree 'citation
              (lambda (c)
                (org-cite-wrap-citation c info)
                (org-element-property :type
                                      (org-cite-inside-footnote-p c)))
              info))))
  ;; Preserve `:post-blank' property.
  (should
   (equal '(2)
          (org-test-with-parsed-data "[cite:@key]  "
            (org-element-map tree 'citation
              (lambda (c)
                (org-cite-wrap-citation c info)
                (org-element-property :post-blank
                                      (org-cite-inside-footnote-p c)))
              info))))
  ;; Set `:post-blank' to 0 in the element before new footnote.
  (should-not
   (org-test-with-parsed-data "Text [cite:@key]"
     (org-element-map tree 'citation
       (lambda (c)
         (org-cite-wrap-citation c info)
         (let ((previous
                (org-export-get-previous-element
                 (org-cite-inside-footnote-p c) info)))
           (string-match (rx blank string-end) previous)))
       info)))
  (should
   (equal '(0)
          (org-test-with-parsed-data "*Text* [cite:@key]"
            (org-element-map tree 'citation
              (lambda (c)
                (org-cite-wrap-citation c info)
                (let ((previous
                       (org-export-get-previous-element
                        (org-cite-inside-footnote-p c) info)))
                  (org-element-property :post-blank previous)))
              info))))
  (should
   (equal '("Text")
          (org-test-with-parsed-data "Text [cite:@key]"
            (org-element-map tree 'citation
              (lambda (c)
                (org-cite-wrap-citation c info)
                (org-export-get-previous-element
                 (org-cite-inside-footnote-p c) info))
              info)))))

(defun test-org-cite--export-with-rule (text &optional rule punct)
  "Export TEXT string using RULE for punctuation positioning.
Call `org-cite-adjust-note' on each citation object with RULE and, PUNCT
arguments.  Replace citation with \"@\" character in the output."
  (org-test-with-temp-text text
    (let ((org-cite--processors nil))
      (org-cite-register-processor 'test
        :export-citation
        (lambda (citation _s _b info)
          (org-cite-adjust-note citation info rule punct)
          "@"))
      (let ((org-cite-export-processors '((t . (test nil nil)))))
        (org-trim
         (org-export-as
	  (org-export-create-backend
	   :transcoders
	   '((section . (lambda (_s c _i) (replace-regexp-in-string " @" "@" c)))
             (paragraph . (lambda (_s c _i) c))))))))))

(ert-deftest test-org-cite/adjust-note ()
  "Test `org-cite-adjust-note' function."
  ;; Basic tests for all rules.  In the output, @ replaces citation.
  (let ((cases '("\"[cite:@k]!"
                 ".\"[cite:@k]!"
                 "\"[cite:@k]"
                 ".\"[cite:@k]"
                 ".[cite:@k]"
                 "[cite:@k]!")))
    (should                             ;test (inside inside after)
     (equal
      '(iia "!@\"" ".@\"!" "@\"" ".@\"" ".@" "!@")
      (cons 'iia
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(inside inside after)))
                    cases))))
    (should                             ;test (inside inside before)
     (equal
      '(iib "@!\"" "@.\"!" "@\"" "@.\"" "@." "@!")
      (cons 'iib
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(inside inside before)))
                    cases))))
    (should                             ;test (inside outside after)
     (equal
      '(ioa "!\"@" ".\"!@" "\"@" ".\"@" ".@" "!@")
      (cons 'ioa
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(inside outside after)))
                    cases))))
    (should                             ;test (inside outside before)
     (equal
      '(iob "!\"@" ".\"@!" "\"@" ".\"@" "@." "@!")
      (cons 'iob
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(inside outside before)))
                    cases))))
    (should                             ;test (inside same after)
     (equal
      '(isa "!@\"" ".\"!@" "\"@" ".@\"" ".@" "!@")
      (cons 'isa
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(inside same after)))
                    cases))))
    (should                             ;test (inside same before)
     (equal
      '(isb "@!\"" ".\"@!" "\"@" "@.\"" "@." "@!")
      (cons 'isb
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(inside same before)))
                    cases))))
    (should                             ;test (outside inside after)
     (equal
      '(oia "@\"!" ".@\"!" "@\"" "@\"." ".@" "!@")
      (cons 'oia
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(outside inside after)))
                    cases))))
    (should                             ;test (outside inside before)
     (equal
      '(oib "@\"!" "@.\"!" "@\"" "@\"." "@." "@!")
      (cons 'oib
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(outside inside before)))
                    cases))))
    (should                             ;test (outside outside after)
     (equal
      '(ooa "\"!@" ".\"!@" "\"@" "\".@" ".@" "!@")
      (cons 'ooa
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(outside outside after)))
                    cases))))
    (should                             ;test (outside outside before)
     (equal
      '(oob "\"@!" ".\"@!" "\"@" "\"@." "@." "@!")
      (cons 'oob
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(outside outside before)))
                    cases))))
    (should                             ;test (outside same after)
     (equal
      '(osa "\"!@" ".\"!@" "\"@" "\".@" ".@" "!@")
      (cons 'osa
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(outside same after)))
                    cases))))
    (should                             ;test (outside same before)
     (equal
      '(osb "\"@!" ".\"@!" "\"@" "\"@." "@." "@!")
      (cons 'osb
            (mapcar (lambda (c)
                      (test-org-cite--export-with-rule
                       c '(outside same before)))
                    cases)))))
  ;; Test `adaptive' behaviour.
  (should
   (equal "@\"."
          (test-org-cite--export-with-rule ".\" [cite:@k]"
                                           '(adaptive inside after))))
  (should
   (equal "@\"!"
          (test-org-cite--export-with-rule "\" [cite:@k]!"
                                           '(adaptive inside after))))
  (should
   (equal ".@\""
          (test-org-cite--export-with-rule ".\"[cite:@k]"
                                           '(adaptive inside after))))
  (should
   (equal "!@\""
          (test-org-cite--export-with-rule "\"[cite:@k]!"
                                           '(adaptive inside after))))
  ;; Handle white space when inserting citation before quotation mark
  ;; or punctuation.
  (should
   (equal ",@\" next"
          (test-org-cite--export-with-rule ",\" [cite:@k] next"
                                           '(inside inside after))))
  (should
   (equal "@,\" next"
          (test-org-cite--export-with-rule ",\" [cite:@k] next"
                                           '(inside inside before))))
  (should
   (equal "@\"."
          (test-org-cite--export-with-rule "\" [cite:@k]."
                                           '(outside inside before))))
  (should
   (equal "@\" !"
          (test-org-cite--export-with-rule "\" [cite:@k] !"
                                           '(outside inside before))))
  (should
   (equal "text@ !"
          (test-org-cite--export-with-rule "text ![cite:@k]"
                                           '(inside outside before))))
  ;; Preserve white space between citation and final punctuation when
  ;; moving citation past final punctuation.
  (should
   (equal "text !@"
          (test-org-cite--export-with-rule "text [cite:@k] !"
                                           '(inside inside after))))
  (should
   (equal "text\n !@"
          (test-org-cite--export-with-rule "text [cite:@k]\n !"
                                           '(inside inside after))))
  ;; Choose punctuation with optional argument.
  (should-not
   (equal "!@"
          (test-org-cite--export-with-rule "[cite:@k]!"
                                           '(inside outside after)
                                           '("."))))
  (should
   (equal ".@"
          (test-org-cite--export-with-rule "[cite:@k]."
                                           '(inside outside after)
                                           '(".")))))

(ert-deftest test-org-cite/parse-elements ()
  "Test `org-cite-parse-elements' function."
  (should-error (org-cite-parse-elements "* H"))
  (should-error (org-cite-parse-elements "Paragraph\n* H"))
  (should
   (equal '(paragraph)
          (mapcar #'org-element-type (org-cite-parse-elements "s"))))
  (should
   (equal '(paragraph paragraph)
          (mapcar #'org-element-type (org-cite-parse-elements "Text\n\nText")))))

(ert-deftest test-org-cite/parse-objects ()
  "Test `org-cite-parse-objects' function."
  (should
   (equal '(plain-text)
          (mapcar #'org-element-type (org-cite-parse-objects "s"))))
  (should
   (equal '(plain-text bold)
          (mapcar #'org-element-type (org-cite-parse-objects "s *b*"))))
  (should
   (equal '(link)
          (mapcar #'org-element-type (org-cite-parse-objects "[[link]]"))))
  ;; When optional argument is non-nil, only recognize types allowed
  ;; in as a citation reference affix.
  (should-not
   (equal '(link)
          (mapcar #'org-element-type (org-cite-parse-objects "[[link]]" t))))
  (should
   (equal '(bold)
          (mapcar #'org-element-type (org-cite-parse-objects "*b*" t)))))

(ert-deftest test-org-cite/make-paragraph ()
  "Test `org-cite-make-paragraph' function."
  ;; Check string as argument.
  (should
   (eq 'paragraph
       (org-element-type (org-cite-make-paragraph "a"))))
  (should
   (equal '("a")
          (org-element-contents (org-cite-make-paragraph "a"))))
  ;; Check object as argument.
  (should
   (eq 'paragraph
       (org-element-type
        (org-cite-make-paragraph (org-element-create 'bold nil "b")))))
  (should
   (equal '(bold)
          (mapcar #'org-element-type
                  (org-element-contents
                   (org-cite-make-paragraph (org-element-create 'bold nil "b"))))))
  ;; Check secondary string as argument.
  (should
   (eq 'paragraph
       (org-element-type (org-cite-make-paragraph '("a")))))
  (should
   (equal '("a")
          (org-element-contents (org-cite-make-paragraph '("a")))))
  ;; Mix all types of arguments.
  (should
   (equal '(plain-text bold plain-text)
          (mapcar #'org-element-type
                  (org-element-contents
                   (org-cite-make-paragraph
                    "a" (org-element-create 'bold nil "b") '("c"))))))
  ;; Check `:parent' property.
  (should
   (eq 'paragraph
       (org-element-type
        (org-element-property
         :parent
         (car (org-element-contents (org-cite-make-paragraph "a"))))))))

(ert-deftest test-org-cite/emphasize ()
  "Test `org-cite-emphasize' function."
  ;; Raise an error if first argument has wrong type.
  (should-error (org-cite-emphasize 'code "a"))
  ;; Check string argument.
  (should (eq 'bold (org-element-type (org-cite-emphasize 'bold "a"))))
  (should (equal '("a") (org-element-contents (org-cite-emphasize 'bold "a"))))
  ;; Check object argument.
  (should
   (eq 'bold
       (org-element-type
        (org-cite-emphasize 'bold (org-element-create 'bold nil "a")))))
  (should
   (equal '(italic)
          (mapcar #'org-element-type
                  (org-element-contents
                   (org-cite-emphasize 'bold
                     (org-element-create 'italic nil "a"))))))
  ;; Check secondary string argument.
  (should (eq 'bold (org-element-type (org-cite-emphasize 'bold '("a")))))
  (should (equal '("a") (org-element-contents (org-cite-emphasize 'bold '("a")))))
  ;; Mix all types of arguments.
  (should
   (equal '(plain-text italic plain-text)
          (mapcar #'org-element-type
                  (org-element-contents
                   (org-cite-emphasize 'bold
                     "a" (org-element-create 'italic nil "b") '("c"))))))
  ;; Check `:parent' property.
  (should
   (eq 'bold
       (org-element-type
        (org-element-property
         :parent
         (car (org-element-contents (org-cite-emphasize 'bold "a"))))))))

(ert-deftest test-org-cite/concat ()
  "Test `org-cite-concat' function."
  ;; Return nil when there is no data.
  (should
   (equal "" (org-element-interpret-data (org-cite-concat))))
  ;; Concatenate strings, objects and secondary strings.
  (should
   (equal "ab"
          (org-element-interpret-data (org-cite-concat "a" "b"))))
  (should
   (equal "*a* b"
          (org-element-interpret-data
           (org-cite-concat (org-element-create 'bold nil "a") " b"))))
  (should
   (equal "*a* b"
          (org-element-interpret-data
           (org-cite-concat
            (list (org-element-create 'bold nil "a")) " b"))))
  ;; Return an error for any other object type.
  (should-error (org-cite-concat 2)))

(ert-deftest test-org-cite/mapconcat ()
  "Test `org-cite-mapconcat' function."
  (should
   (equal ""
          (org-element-interpret-data
           (org-cite-mapconcat #'identity nil ""))))
  (should
   (equal "ab"
          (org-element-interpret-data
           (org-cite-mapconcat #'identity '("a" "b") ""))))
  (should
   (equal "*a* b *c*"
          (org-element-interpret-data
           (org-cite-mapconcat
            #'identity
            (list (org-element-create 'bold nil "a")
                  (list " b " (org-element-create 'bold nil "c"))) ""))))
  (should
   (equal "*a* *b*"
          (org-element-interpret-data
           (org-cite-mapconcat
            (lambda (s) (org-element-create 'bold nil s))
            '("a" "b") " "))))
  (should
   (equal "*a* b*c*"
          (org-element-interpret-data
           (org-cite-mapconcat
            #'identity
            (list (org-element-create 'bold nil "a")
                  (list "b" (org-element-create 'bold nil "c"))) " ")))))


;;; Test capabilities.
(ert-deftest test-org-cite/activate-capability ()
  "Test \"activate\" capability."
  ;; Standard test.
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "[cite:@key]"
           (let ((org-cite--processors nil)
                 (org-cite-activate-processor 'foo))
             (org-cite-register-processor 'foo
               :activate (lambda (_) (throw :exit 'success)))
             (font-lock-ensure))))))
  ;; If there is no "follow" processor, or if processor does not
  ;; handle this capability, fall back to fontifying whole citation
  ;; with `org-cite' face and each key with `org-cite-key' face.
  (should
   (eq 'org-cite
       (org-test-with-temp-text "[cite:@key]"
         (let ((org-cite-activate-processor nil))
           (font-lock-ensure)
           (face-at-point)))))
  (should
   (eq 'org-cite-key
       (org-test-with-temp-text "[cite:@<point>key]"
         (let ((org-cite-activate-processor nil))
           (font-lock-ensure)
           (face-at-point)))))
  (should
   (eq 'org-cite
       (org-test-with-temp-text "[cite:@key]"
         (let ((org-cite--processors nil)
               (org-cite-activate-processor 'foo))
           (org-cite-register-processor 'foo)
           (font-lock-ensure)
           (face-at-point))))))

(ert-deftest test-org-cite/export-capability ()
  "Test \"export\" capability."
  ;; Regular citations export.
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "[cite:@key]"
           (let ((org-cite--processors nil)
                 (org-cite-export-processors '((t . (foo nil nil)))))
             (org-cite-register-processor 'foo
               :export-citation (lambda (&rest _) (throw :exit 'success)))
             (org-export-as (org-export-create-backend)))))))
  ;; Export citation as string.
  (should
   (equal "citation\n"
          (org-test-with-temp-text "[cite:@key]"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-citation (lambda (&rest _) "citation"))
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c)))))))))
  ;; Export citation as parsed object.
  (should
   (equal "success\n"
          (org-test-with-temp-text "[cite:@key]"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-citation (lambda (&rest _)
                                   (org-element-create 'bold nil "cite")))
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c))
                                (bold . (lambda (&rest _) "success")))))))))
  ;; Export citation as a secondary string.
  (should
   (equal "boldtwo\n"
          (org-test-with-temp-text "[cite:@key]"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-citation (lambda (&rest _)
                                   (list (org-element-create 'bold nil "one")
                                         "two")))
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c))
                                (bold . (lambda (&rest _) "bold")))))))))
  ;; When exporting citation as a secondary string, last object
  ;; inherits post-blank from initial citation.
  (should
   (equal "twobold one-space\n"
          (org-test-with-temp-text "[cite:@key] one-space"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-citation (lambda (&rest _)
                                   (list "two"
                                         (org-element-create 'bold nil "one"))))
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c))
                                (bold . (lambda (&rest _) "bold")))))))))
  (should
   (equal "boldtwo one-space\n"
          (org-test-with-temp-text "[cite:@key] one-space"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-citation (lambda (&rest _)
                                   (list (org-element-create 'bold nil "one")
                                         "two")))
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c))
                                (bold . (lambda (&rest _) "bold")))))))))
  ;; Make sure to have a space between a quote and a citation.
  (should
   (equal "\"quotation\" citation\n"
          (org-test-with-temp-text "\"quotation\"[cite:@key]"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-citation (lambda (&rest _) "citation"))
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c)))))))))
  (should
   (equal "\"quotation\"  citation\n"
          (org-test-with-temp-text "\"quotation\"  [cite:@key]"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-citation (lambda (&rest _) "citation"))
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c)))))))))
  ;; Regular bibliography export.
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "#+print_bibliography:"
           (let ((org-cite--processors nil)
                 (org-cite-export-processors '((t . (foo nil nil)))))
             (org-cite-register-processor 'foo
               :export-bibliography (lambda (&rest _) (throw :exit 'success))
               :export-citation #'ignore)
             (org-export-as (org-export-create-backend)))))))
  (should
   (equal ""
          (org-test-with-temp-text "#+print_bibliography:"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-citation #'ignore)
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c)))))))))
  ;; Export bibliography as string.
  (should
   (equal "bibliography\n"
          (org-test-with-temp-text "#+print_bibliography:"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-bibliography (lambda (&rest _) "bibliography")
                :export-citation #'ignore)
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c)))))))))
  ;; Export bibliography as a parsed element.
  (should
   (equal "success\n"
          (org-test-with-temp-text "#+print_bibliography:"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-bibliography
                (lambda (&rest _)
                  (org-element-create 'example-block '(:value "foo")))
                :export-citation #'ignore)
              (org-export-as
               (org-export-create-backend
                :transcoders
                '((section . (lambda (_ c _) c))
                  (example-block . (lambda (&rest _) "success")))))))))
  ;; Export bibliography as a list of parsed elements.
  (should
   (equal "success\nsuccess\n"
          (org-test-with-temp-text "#+print_bibliography:"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-bibliography
                (lambda (&rest _)
                  (list (org-element-create 'example-block '(:value "foo"))
                        (org-element-create 'example-block '(:value "bar"))))
                :export-citation #'ignore)
              (org-export-as
               (org-export-create-backend
                :transcoders
                '((section . (lambda (_ c _) c))
                  (example-block . (lambda (&rest _) "success")))))))))
  ;; When exporting bibliography as a list of parsed elements, the
  ;; last element inherits post-blank from initial keyword.
  (should
   (equal "success\nsuccess\n\nText\n"
          (org-test-with-temp-text "#+print_bibliography:\n\nText"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-bibliography
                (lambda (&rest _)
                  (list (org-element-create 'example-block '(:value "foo"))
                        (org-element-create 'example-block '(:value "bar"))))
                :export-citation #'ignore)
              (org-export-as
               (org-export-create-backend
                :transcoders
                '((section . (lambda (_ c _) c))
                  (example-block . (lambda (&rest _) "success"))
                  (paragraph . (lambda (_ c _) c)))))))))
  ;; Use more appropriate citation processor.
  (should
   (equal
    '(p1 p1 p1 p3)
    (org-test-with-temp-text "[cite:@a]"
      (let ((org-export-registered-backends nil)
            (org-cite--procesors nil)
            (org-cite-export-processors
             '((b1 . (p1))
               (t  . (p3)))))
        (org-cite-register-processor 'p1
          :export-citation (lambda (&rest _) (throw :exit 'p1)))
        (org-cite-register-processor 'p2
          :export-citation (lambda (&rest _) (throw :exit 'p2)))
        (org-cite-register-processor 'p3
          :export-citation (lambda (&rest _) (throw :exit 'p3)))
        (org-export-define-backend 'b1 nil)
        (org-export-define-derived-backend 'b2 'b1)
        (org-export-define-derived-backend 'b3 'b2)
        (list (catch :exit (org-export-as 'b1))
              (catch :exit (org-export-as 'b2))
              (catch :exit (org-export-as 'b3))
              (catch :exit (org-export-as (org-export-create-backend))))))))
  (should
   (eq 'p2
       (org-test-with-temp-text "#+cite_export: p2\n[cite:@a]"
         (let ((org-export-registered-backends nil)
               (org-cite--procesors nil)
               (org-cite-export-processors '((t  . (p1)))))
           (org-cite-register-processor 'p1
             :export-citation (lambda (&rest _) (throw :exit 'p1)))
           (org-cite-register-processor 'p2
             :export-citation (lambda (&rest _) (throw :exit 'p2)))
           (catch :exit (org-export-as (org-export-create-backend)))))))
  ;; Test finalizer.
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "[cite:@key]"
           (let ((org-cite--processors nil)
                 (org-cite-export-processors '((t . (foo nil nil)))))
             (org-cite-register-processor 'foo
               :export-citation (lambda (&rest _) "")
               :export-finalizer (lambda (&rest _) (throw :exit 'success)))
             (org-export-as (org-export-create-backend)))))))
  (should
   (equal "finalized!"
          (org-test-with-temp-text "[cite:@key]"
            (let ((org-cite--processors nil)
                  (org-cite-export-processors '((t . (foo nil nil)))))
              (org-cite-register-processor 'foo
                :export-citation #'ignore
                :export-finalizer (lambda (&rest _) "finalized!"))
              (org-export-as (org-export-create-backend))))))
  ;; Ignore citations when there is no selected "export" processor.
  ;; In that case, white space is removed before the citation, not
  ;; after.
  (should
   (equal ""
          (org-test-with-temp-text "[cite:@key]"
            (let ((org-cite-export-processors nil))
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c)))))))))
  (should
   (equal "Text.\n"
          (org-test-with-temp-text "Text [cite:@key]."
            (let ((org-cite-export-processors nil))
              (org-export-as (org-export-create-backend
                              :transcoders
                              '((section . (lambda (_ c _) c))
                                (paragraph . (lambda (_ c _) c)))))))))
  ;; Throw an error if selected processor does not handle "export"
  ;; capability.
  (should-error
   (org-test-with-temp-text "[cite:@key]"
     (let ((org-cite--processors nil)
           (org-cite-export-processors '((t . (foo nil nil)))))
       (org-cite-register-processor 'foo)
       (org-export-as (org-export-create-backend))))))

(ert-deftest test-org-cite/follow-capability ()
  "Test \"follow\" capability."
  ;; Standard test.
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "[cite:@key]"
           (let ((org-cite--processors nil)
                 (org-cite-follow-processor 'foo))
             (org-cite-register-processor 'foo
               :follow (lambda (_ _) (throw :exit 'success)))
             (org-open-at-point))))))
  ;; Throw an error if there is no "follow" processor, or if it is
  ;; unable to follow a citation.
  (should-error
   (org-test-with-temp-text "[cite:@key]"
     (let ((org-cite-follow-processor nil))
       (org-open-at-point))))
  (should-error
   (org-test-with-temp-text "[cite:@key]"
     (let ((org-cite--processors nil)
           (org-cite-follow-processor 'foo))
       (org-cite-register-processor 'foo)
       (org-open-at-point)))))

(ert-deftest test-org-cite/make-insert-processor ()
  "Test `org-cite-make-insert-processor'."
  (should-error (org-cite-make-insert-processor 1 2))
  (should-error
   (org-test-with-temp-text "[cite:@<point>a]"
     (let ((org-cite--processors nil)
           (org-cite-insert-processor 'foo))
       (org-cite-register-processor 'foo
         :insert (org-cite-make-insert-processor
                  #'ignore (lambda (&rest _) "s")))
       (org-cite-insert nil))))
  (should
   (equal "[cite:@k]"
          (org-test-with-temp-text "[cite:@<point>a]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal "[cite:@k;@a]"
          (org-test-with-temp-text "[cite:<point>@a]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal "[cite:@k;pre @a]"
          (org-test-with-temp-text "[cite:<point>pre @a]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal "[cite:pre;@k;@a]"
          (org-test-with-temp-text "[cite:<point>pre;@a]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal "[cite:@a;@k]"
          (org-test-with-temp-text "[cite:@a<point>]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal "[cite:@a post;@k]"
          (org-test-with-temp-text "[cite:@a post<point>]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal "[cite:@a;@k;post]"
          (org-test-with-temp-text "[cite:@a;post<point>]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal ""
          (org-test-with-temp-text "[cite:@<point>a]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "s")))
              (org-cite-insert t)
              (buffer-string)))))
  (should
   (equal "[cite/s:@a]"
          (org-test-with-temp-text "[cite<point>:@a]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal "[cite:@a]"
          (org-test-with-temp-text "[cite<point>/style:@a]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) "k") (lambda (&rest _) "")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should-error
   (org-test-with-temp-text "[cite<point>/style:@a]"
     (let ((org-cite--processors nil)
           (org-cite-insert-processor 'foo))
       (org-cite-register-processor 'foo
         :insert (org-cite-make-insert-processor
                  (lambda (&rest _) "k") #'ignore))
       (org-cite-insert nil))))
  (should
   (equal "[cite:@a][cite:@k]"
          (org-test-with-temp-text "[cite:@a]<point>"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) '("k")) (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal "[cite:@k][cite:@a]"
          (org-test-with-temp-text "<point>[cite:@a]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) '("k")) (lambda (&rest _) "s")))
              (org-cite-insert nil)
              (buffer-string)))))
  (should
   (equal "[cite/s:@k][cite:@a]"
          (org-test-with-temp-text "<point>[cite:@a]"
            (let ((org-cite--processors nil)
                  (org-cite-insert-processor 'foo))
              (org-cite-register-processor 'foo
                :insert (org-cite-make-insert-processor
                         (lambda (&rest _) '("k")) (lambda (&rest _) "s")))
              (org-cite-insert t)
              (buffer-string))))))

(ert-deftest test-org-cite/insert-capability ()
  "Test \"insert\" capability."
  ;; Standard test.
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text ""
           (let ((org-cite--processors nil)
                 (org-cite-insert-processor 'foo))
             (org-cite-register-processor 'foo
               :insert (lambda (_ _) (throw :exit 'success)))
             (call-interactively #'org-cite-insert))))))
  ;; Throw an error if there is no "insert" processor, or if it is
  ;; unable to insert a citation.
  (should-error
   (org-test-with-temp-text ""
     (let ((org-cite-insert-processor nil))
       (call-interactively #'org-cite-insert))))
  (should-error
   (org-test-with-temp-text ""
     (let ((org-cite--processors nil)
           (org-cite-insert-processor 'foo))
       (org-cite-register-processor 'foo)
       (call-interactively #'org-cite-insert))))
  ;; Throw an error if the location is inappropriate for a citation.
  (should-error
   (org-test-with-temp-text "=verbatim<point> text="
     (let ((org-cite--processors nil)
           (org-cite-insert-processor 'foo))
       (org-cite-register-processor 'foo
         :insert (lambda (_ _) (throw :exit 'success)))
       (call-interactively #'org-cite-insert))))
  ;; Allow inserting citations at the beginning of a footnote
  ;; definition, right after the label.
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "[fn:1]<point>"
           (let ((org-cite--processors nil)
                 (org-cite-insert-processor 'foo))
             (org-cite-register-processor 'foo
               :insert (lambda (_ _) (throw :exit 'success)))
             (call-interactively #'org-cite-insert))))))
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "[fn:1] <point>"
           (let ((org-cite--processors nil)
                 (org-cite-insert-processor 'foo))
             (org-cite-register-processor 'foo
               :insert (lambda (_ _) (throw :exit 'success)))
             (call-interactively #'org-cite-insert))))))
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "[fn:1]<point>\nParagraph"
           (let ((org-cite--processors nil)
                 (org-cite-insert-processor 'foo))
             (org-cite-register-processor 'foo
               :insert (lambda (_ _) (throw :exit 'success)))
             (call-interactively #'org-cite-insert))))))
  (should-error
   (org-test-with-temp-text "[fn:1<point>]"
     (let ((org-cite--processors nil)
           (org-cite-insert-processor 'foo))
       (org-cite-register-processor 'foo
         :insert (lambda (_ _) (throw :exit 'success)))
       (call-interactively #'org-cite-insert))))
  (should-error
   (org-test-with-temp-text "<point>[fn:1]"
     (let ((org-cite--processors nil)
           (org-cite-insert-processor 'foo))
       (org-cite-register-processor 'foo
         :insert (lambda (_ _) (throw :exit 'success)))
       (call-interactively #'org-cite-insert))))
  ;; Allow inserting citations in captions.
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "#+caption: <point>\n| table |"
           (let ((org-cite--processors nil)
                 (org-cite-insert-processor 'foo))
             (org-cite-register-processor 'foo
               :insert (lambda (_ _) (throw :exit 'success)))
             (call-interactively #'org-cite-insert))))))
  ;; Allow inserting citations in table cells.
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "| <point>table |"
           (let ((org-cite--processors nil)
                 (org-cite-insert-processor 'foo))
             (org-cite-register-processor 'foo
               :insert (lambda (_ _) (throw :exit 'success)))
             (call-interactively #'org-cite-insert))))))
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "| table<point> |"
           (let ((org-cite--processors nil)
                 (org-cite-insert-processor 'foo))
             (org-cite-register-processor 'foo
               :insert (lambda (_ _) (throw :exit 'success)))
             (call-interactively #'org-cite-insert))))))
  (should
   (eq 'success
       (catch :exit
         (org-test-with-temp-text "| table  <point> |"
           (let ((org-cite--processors nil)
                 (org-cite-insert-processor 'foo))
             (org-cite-register-processor 'foo
               :insert (lambda (_ _) (throw :exit 'success)))
             (call-interactively #'org-cite-insert)))))))

(provide 'test-oc)
;;; test-oc.el ends here
