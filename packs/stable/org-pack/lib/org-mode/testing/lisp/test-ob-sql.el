;;; test-ob-sql.el --- tests for ob-sql.el

;; Copyright (C) 2021 Robin Joy

;; Author: Robin Joy <rcj@robinjoy.net>
;; Keywords: lisp

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

;;; Code:

(unless (featurep 'ob-sql)
  (signal 'missing-test-dependency "Support for sql code blocks"))

(defmacro ob-sql/command (&rest body)
  "Execute body and return the command that would have been executed."
  `(cl-letf (((symbol-function 'org-babel-eval)
              (lambda (command &rest _) (throw 'sql-command command))))
     (catch 'sql-command
       ,@body)))

(defmacro ob-sql/command-should-contain (regexp sql-block)
  "Check that REGEXP is contained in the command executed when evaluating SQL-BLOCK."
  `(let ((regexps ,(if (listp regexp) regexp `(list ,regexp)))
         (command (ob-sql/command (org-test-with-temp-text
                                      ,sql-block
                                    (org-babel-next-src-block)
                                    (org-babel-execute-src-block)))))
     (dolist (regexp regexps)
       (should (string-match-p regexp command)))))

(defmacro ob-sql/command-should-not-contain (regexp sql-block)
  "Check that REGEXP is not contained in the command executed when evaluating SQL-BLOCK."
  `(let ((command (ob-sql/command
                   (org-test-with-temp-text
                       ,sql-block
                     (org-babel-next-src-block)
                     (org-babel-execute-src-block)))))
     (should-not (string-match-p ,regexp command))))

;;; dbish
(ert-deftest ob-sql/engine-dbi-uses-dbish ()
  (ob-sql/command-should-contain "^dbish " "
#+begin_src sql :engine dbi
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-dbish-uses-batch-mode ()
  (ob-sql/command-should-contain " --batch " "
#+begin_src sql :engine dbi :dbuser dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-dbish-can-pass-additional-cmdline-params ()
  (ob-sql/command-should-contain " cmdlineparams " "
#+begin_src sql :engine dbi :dbpassword dummy :cmdline cmdlineparams
  select * from dummy;
#+end_src"))

;;; monetdb
(ert-deftest ob-sql/engine-monetdb-uses-mclient ()
  (ob-sql/command-should-contain "^mclient " "
#+begin_src sql :engine monetdb
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-monetdb-outputs-values-tab-separated ()
  (ob-sql/command-should-contain " -f tab " "
#+begin_src sql :engine monetdb
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-monetdb-can-pass-additional-cmdline-params ()
  (ob-sql/command-should-contain " cmdlineparams " "
#+begin_src sql :engine monetdb :dbpassword dummy :cmdline cmdlineparams
  select * from dummy;
#+end_src"))

;;; mssql
(ert-deftest ob-sql/engine-mssql-uses-sqlcmd ()
  (ob-sql/command-should-contain "^sqlcmd " "
#+begin_src sql :engine mssql
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mssql-outputs-values-tab-separated ()
  (ob-sql/command-should-contain " -s \"\t\" " "
#+begin_src sql :engine mssql
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mssql-can-pass-additional-cmdline-params ()
  (ob-sql/command-should-contain " cmdlineparams " "
#+begin_src sql :engine mssql :dbpassword dummy :cmdline cmdlineparams
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mssql-passes-user-if-provided ()
  (ob-sql/command-should-contain " -U \"dummy\" " "
#+begin_src sql :engine mssql :dbuser dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mssql-passes-password-if-provided ()
  (ob-sql/command-should-contain " -P \"dummy\" " "
#+begin_src sql :engine mssql :dbpassword dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mssql-passes-dbhost-if-provided ()
  (ob-sql/command-should-contain " -S \"localhost\" " "
#+begin_src sql :engine mssql :dbhost localhost
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mssql-passes-database-if-provided ()
  (ob-sql/command-should-contain " -d \"R01\" " "
#+begin_src sql :engine mssql :database R01
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mssql-passes-all-parameter-provided ()
  (ob-sql/command-should-contain '(" -d \"R01\" " " -S \"localhost\" " " -P \"pwd\" " " -U \"usr\" ") "
#+begin_src sql :engine mssql :database R01 :dbhost localhost :dbport 30101 :dbinstance 1 :dbuser usr :dbpassword pwd
  select * from dummy;
#+end_src"))

;;; MySQL
(ert-deftest ob-sql/engine-mysql-uses-mysql ()
  (ob-sql/command-should-contain "^mysql " "
#+begin_src sql :engine mysql
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mysql-passes-user-if-provided ()
  (ob-sql/command-should-contain " -udummy " "
#+begin_src sql :engine mysql :dbuser dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mysql-passes-password-if-provided ()
  (ob-sql/command-should-contain " -pdummy " "
#+begin_src sql :engine mysql :dbpassword dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mysql-passes-dbhost-if-provided ()
  (ob-sql/command-should-contain " -hlocalhost " "
#+begin_src sql :engine mysql :dbhost localhost
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mysql-passes-host-if-provided ()
  (ob-sql/command-should-contain " -P30101 " "
#+begin_src sql :engine mysql :dbport 30101
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mysql-passes-database-if-provided ()
  (ob-sql/command-should-contain " -dR01 " "
#+begin_src sql :engine mysql :database R01
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-mysql-passes-all-parameter-provided ()
  (ob-sql/command-should-contain '(" -dR01 " " -hlocalhost " " -P30101 " " -ppwd " " -uusr ") "
#+begin_src sql :engine mysql :database R01 :dbhost localhost :dbport 30101 :dbinstance 1 :dbuser usr :dbpassword pwd
  select * from dummy;
#+end_src"))

;;; oracle
(ert-deftest ob-sql/engine-oracle-uses-sqlplus ()
  (ob-sql/command-should-contain "^sqlplus " "
#+begin_src sql :engine oracle :dbuser dummy :dbpassword dummy :database dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-oracle-passes-user-pwd-database-host-port-if-provided ()
  (ob-sql/command-should-contain " dummy/dummypwd@localhost:12345/R01 " "
#+begin_src sql :engine oracle :dbuser dummy :dbpassword dummypwd :dbhost localhost :database R01 :dbport 12345
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-oracle-passes-user-pwd-database-if-no-host-port-provided ()
  (ob-sql/command-should-contain " dummy/dummypwd@R01 " "
#+begin_src sql :engine oracle :dbuser dummy :dbpassword dummypwd :database R01
  select * from dummy;
#+end_src"))

;;; postgresql
(ert-deftest ob-sql/engine-postgresql-uses-psql ()
  (ob-sql/command-should-contain "^psql " "
#+begin_src sql :engine postgresql
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-postgresql-passes-password-if-provided ()
  (ob-sql/command-should-contain "^PGPASSWORD=dummy " "
#+begin_src sql :engine postgresql :dbpassword dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-postgresql-stop-on-error ()
  (ob-sql/command-should-contain " --set=\"ON_ERROR_STOP=1\" " "
#+begin_src sql :engine postgresql
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-postgresql-does-not-output-column-names-if-requested ()
  (ob-sql/command-should-contain " -t " "
#+begin_src sql :engine postgresql :colnames no
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-postgresql-outputs-column-names-by-default ()
  (ob-sql/command-should-not-contain " -t " "
#+begin_src sql :engine postgresql
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-postgresql-can-pass-additional-cmdline-params ()
  (ob-sql/command-should-contain " cmdlineparams$" "
#+begin_src sql :engine postgresql :dbpassword dummy :cmdline cmdlineparams
  select * from dummy;
#+end_src"))

;;; SAP HANA
(ert-deftest ob-sql/engine-saphana-uses-hdbsql ()
  (ob-sql/command-should-contain "^hdbsql " "
#+begin_src sql :engine saphana
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-saphana-passes-user-if-provided ()
  (ob-sql/command-should-contain " -u dummy " "
#+begin_src sql :engine saphana :dbuser dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-saphana-passes-password-if-provided ()
  (ob-sql/command-should-contain " -p dummy " "
#+begin_src sql :engine saphana :dbpassword dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-saphana-passes-dbinstance-if-provided ()
  (ob-sql/command-should-contain " -i 1 " "
#+begin_src sql :engine saphana :dbinstance 1
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-saphana-passes-dbhost-if-provided ()
  (ob-sql/command-should-contain " -n localhost " "
#+begin_src sql :engine saphana :dbhost localhost
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-saphana-passes-dbhost-and-dbport-if-provided ()
  (ob-sql/command-should-contain " -n localhost:30101 " "
#+begin_src sql :engine saphana :dbhost localhost :dbport 30101
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-saphana-does-not-pass-host-port-if-only-port-provided ()
  (ob-sql/command-should-not-contain " -n" "
#+begin_src sql :engine saphana :dbport 30101
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-saphana-passes-database-if-provided ()
  (ob-sql/command-should-contain " -d R01 " "
#+begin_src sql :engine saphana :database R01
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-saphana-passes-all-parameter-provided ()
  (ob-sql/command-should-contain '(" -d R01 " " -n localhost:30101 " " -i 1 " " -p pwd " " -u usr") "
#+begin_src sql :engine saphana :database R01 :dbhost localhost :dbport 30101 :dbinstance 1 :dbuser usr :dbpassword pwd
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-saphana-can-pass-additional-cmdline-params ()
  (ob-sql/command-should-contain " cmdlineparams$" "
#+begin_src sql :engine saphana :dbpassword dummy :cmdline cmdlineparams
  select * from dummy;
#+end_src"))

;;; sqsh
(ert-deftest ob-sql/engine-sqsh-uses-sqsh ()
  (ob-sql/command-should-contain "^sqsh " "
#+begin_src sql :engine sqsh
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-sqsh-can-pass-additional-cmdline-params ()
  (ob-sql/command-should-contain " cmdlineparams " "
#+begin_src sql :engine sqsh :dbpassword dummy :cmdline cmdlineparams
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-sqsh-passes-user-if-provided ()
  (ob-sql/command-should-contain " -U \"dummy\" " "
#+begin_src sql :engine sqsh :dbuser dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-sqsh-passes-password-if-provided ()
  (ob-sql/command-should-contain " -P \"dummy\" " "
#+begin_src sql :engine sqsh :dbpassword dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-sqsh-passes-host-if-provided ()
  (ob-sql/command-should-contain " -S \"localhost\" " "
#+begin_src sql :engine sqsh :dbhost localhost
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-sqsh-passes-database-if-provided ()
  (ob-sql/command-should-contain " -D \"R01\" " "
#+begin_src sql :engine sqsh :database R01
  select * from dummy;
#+end_src"))


;;; vertica
(ert-deftest ob-sql/engine-vertica-uses-vsql ()
  (ob-sql/command-should-contain "^vsql " "
#+begin_src sql :engine vertica
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-vertica-can-pass-additional-cmdline-params ()
  (ob-sql/command-should-contain " cmdlineparams$" "
#+begin_src sql :engine vertica :dbpassword dummy :cmdline cmdlineparams
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-vertica-passes-user-if-provided ()
  (ob-sql/command-should-contain " -U dummy " "
#+begin_src sql :engine vertica :dbuser dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-vertica-passes-password-if-provided ()
  (ob-sql/command-should-contain " -w dummy " "
#+begin_src sql :engine vertica :dbpassword dummy
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-vertica-passes-host-if-provided ()
  (ob-sql/command-should-contain " -h localhost " "
#+begin_src sql :engine vertica :dbhost localhost
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-vertica-passes-database-if-provided ()
  (ob-sql/command-should-contain " -d R01 " "
#+begin_src sql :engine vertica :database R01
  select * from dummy;
#+end_src"))

(ert-deftest ob-sql/engine-vertica-passes-port-if-provided ()
  (ob-sql/command-should-contain " -p 12345 " "
#+begin_src sql :engine vertica :dbport 12345
  select * from dummy;
#+end_src"))

;;; test-ob-sqlite.el ends here
