;;; apache-mode.el --- major mode for editing Apache configuration files

;; Copyright (c) 2004, 2005 Karl Chen <quarl@nospam.quarl.org>
;; Copyright (c) 1999 Jonathan Marten  <jonathan.marten@uk.sun.com>

;; Author: Karl Chen <quarl@nospam.quarl.org>

;; Keywords: languages, faces
;; Last edit: 2005-01-06
;; Version: 2.0 $Id: apache-mode.el 8264 2005-06-29 23:34:41Z quarl $

;; apache-mode.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with your copy of Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;;
;;   (autoload 'apache-mode "apache-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
;;   (add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
;;   (add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
;;   (add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
;;   (add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
;;

;;; History:

;; 1999-10 Jonathan Marten <jonathan.marten@uk.sun.com>
;;   initial version

;; 2004-09-12 Karl Chen <quarl@nospam.quarl.org>
;;   rewrote pretty much everything using define-derived-mode; added support
;;   for Apache 2.x; fixed highlighting in GNU Emacs; created indentation
;;   function
;;
;; 2005-06-29 Kumar Appaiah <akumar_NOSPAM@ee.iitm.ac.in>
;;   use syntax table instead of font-lock-keywords to highlight comments.

;;; Code:

;; Requires
(require 'regexp-opt)

(defvar apache-indent-level 4
  "*Number of spaces to indent per level")

(defvar apache-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_   "_"    table)
    (modify-syntax-entry ?-   "_"    table)
    (modify-syntax-entry ?(   "()"   table)
    (modify-syntax-entry ?)   ")("   table)
    (modify-syntax-entry ?<   "(>"   table)
    (modify-syntax-entry ?>   ")<"   table)
    (modify-syntax-entry ?\"   "\""  table)
    (modify-syntax-entry ?,   "."    table)
    (modify-syntax-entry ?#   "<"    table)
    (modify-syntax-entry ?\n  ">#"   table)
    table))

;;;###autoload
(define-derived-mode apache-mode fundamental-mode "Apache"
  "Major mode for editing Apache configuration files."

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#\\W*")
  (set (make-local-variable 'comment-column) 48)

  (set (make-local-variable 'indent-line-function) 'apache-indent-line)

  (set (make-local-variable 'font-lock-defaults)
       '(apache-font-lock-keywords nil t
                                   ((?_ . "w")
                                    (?- . "w"))
                                   beginning-of-line)))

;; Font lock
(defconst apache-font-lock-keywords
  (purecopy
   (list

    ;; see syntax table for comment highlighting

    ;; (list "^[ \t]*#.*" 0 'font-lock-comment-face t)

    (list (concat                       ; sections
           "^[ \t]*</?"
           (regexp-opt '(

                         "Directory"
                         "DirectoryMatch"
                         "Files"
                         "FilesMatch"
                         "IfDefine"
                         "IfModule"
                         "Limit"
                         "LimitExcept"
                         "Location"
                         "LocationMatch"
                         "Proxy"
                         "ProxyMatch"
                         "VirtualHost"

                         ) 'words)
           ".*?>")
          1 'font-lock-function-name-face)

    (list (concat                       ; directives
           "^[ \t]*"
           (regexp-opt '
            (

             "AcceptMutex"
             "AcceptPathInfo"
             "AccessConfig"
             "AccessFileName"
             "Action"
             "AddAlt"
             "AddAltByEncoding"
             "AddAltByType"
             "AddCharset"
             "AddDefaultCharset"
             "AddDescription"
             "AddEncoding"
             "AddHandler"
             "AddIcon"
             "AddIconByEncoding"
             "AddIconByType"
             "AddInputFilter"
             "AddLanguage"
             "AddModule"
             "AddModuleInfo"
             "AddOutputFilter"
             "AddOutputFilterByType"
             "AddType"
             "AgentLog"
             "Alias"
             "AliasMatch"
             "Allow from"
             "Allow"
             "AllowCONNECT"
             "AllowEncodedSlashes"
             "AllowOverride"
             "Anonymous"
             "Anonymous_Authoritative"
             "Anonymous_LogEmail"
             "Anonymous_MustGiveEmail"
             "Anonymous_NoUserID"
             "Anonymous_VerifyEmail"
             "AssignUserID"
             "AuthAuthoritative"
             "AuthDBAuthoritative"
             "AuthDBGroupFile"
             "AuthDBMAuthoritative"
             "AuthDBMGroupFile"
             "AuthDBMType"
             "AuthDBMUserFile"
             "AuthDBUserFile"
             "AuthDigestAlgorithm"
             "AuthDigestDomain"
             "AuthDigestFile"
             "AuthDigestGroupFile"
             "AuthDigestNcCheck"
             "AuthDigestNonceFormat"
             "AuthDigestNonceLifetime"
             "AuthDigestQop"
             "AuthDigestShmemSize"
             "AuthGroupFile"
             "AuthLDAPAuthoritative"
             "AuthLDAPBindDN"
             "AuthLDAPBindPassword"
             "AuthLDAPCharsetConfig"
             "AuthLDAPCompareDNOnServer"
             "AuthLDAPDereferenceAliases"
             "AuthLDAPEnabled"
             "AuthLDAPFrontPageHack"
             "AuthLDAPGroupAttribute"
             "AuthLDAPGroupAttributeIsDN"
             "AuthLDAPRemoteUserIsDN"
             "AuthLDAPUrl"
             "AuthName"
             "AuthType"
             "AuthUserFile"
             "BS2000Account"
             "BindAddress"
             "BrowserMatch"
             "BrowserMatchNoCase"
             "CGIMapExtension"
             "CacheDefaultExpire"
             "CacheDirLength"
             "CacheDirLevels"
             "CacheDisable"
             "CacheEnable"
             "CacheExpiryCheck"
             "CacheFile"
             "CacheForceCompletion"
             "CacheGcClean"
             "CacheGcDaily"
             "CacheGcInterval"
             "CacheGcMemUsage"
             "CacheGcUnused"
             "CacheIgnoreCacheControl"
             "CacheIgnoreNoLastMod"
             "CacheLastModifiedFactor"
             "CacheMaxExpire"
             "CacheMaxFileSize"
             "CacheMinFileSize"
             "CacheNegotiatedDocs"
             "CacheRoot"
             "CacheSize"
             "CacheTimeMargin"
             "CharsetDefault"
             "CharsetOptions"
             "CharsetSourceEnc"
             "CheckSpelling"
             "ChildPerUserID"
             "ClearModuleList"
             "ContentDigest"
             "CookieDomain"
             "CookieExpires"
             "CookieLog"
             "CookieName"
             "CookieStyle"
             "CookieTracking"
             "CoreDumpDirectory"
             "CustomLog"
             "Dav"
             "DavDepthInfinity"
             "DavLockDB"
             "DavMinTimeout"
             "DefaultIcon"
             "DefaultLanguage"
             "DefaultMode"
             "DefaultType"
             "DeflateBufferSize"
             "DeflateCompressionLevel"
             "DeflateFilterNote"
             "DeflateMemLevel"
             "DeflateWindowSize"
             "Deny"
             "DirectoryIndex"
             "DirectorySlash"
             "DocTitle"
             "DocTrailer"
             "DocumentRoot"
             "EnableExceptionHook"
             "EnableMMAP"
             "EnableSendfile"
             "ErrorDocument"
             "ErrorLog"
             "Example"
             "ExpiresActive"
             "ExpiresByType"
             "ExpiresDefault"
             "ExtFilterDefine"
             "ExtFilterOptions"
             "ExtendedStatus"
             "FancyIndexing"
             "FileETag"
             "ForceLanguagePriority"
             "ForceType"
             "ForensicLog"
             "Group"
             "HTMLDir"
             "HTTPLogFile"
             "HeadPrefix"
             "HeadSuffix"
             "Header"
             "HeaderName"
             "HideSys"
             "HideURL"
             "HostNameLookups"
             "HostnameLookups"
             "ISAPIAppendLogToErrors"
             "ISAPIAppendLogToQuery"
             "ISAPICacheFile"
             "ISAPIFakeAsync"
             "ISAPILogNotSupported"
             "ISAPIReadAheadBuffer"
             "IdentityCheck"
             "ImapBase"
             "ImapDefault"
             "ImapMenu"
             "Include"
             "IndexIgnore"
             "IndexOptions"
             "IndexOrderDefault"
             "KeepAlive"
             "KeepAliveTimeout"
             "LDAPCacheEntries"
             "LDAPCacheTTL"
             "LDAPOpCacheEntries"
             "LDAPOpCacheTTL"
             "LDAPSharedCacheFile"
             "LDAPSharedCacheSize"
             "LDAPTrustedCA"
             "LDAPTrustedCAType"
             "LanguagePriority"
             "LastURLs"
             "LimitInternalRecursion"
             "LimitRequestBody"
             "LimitRequestFields"
             "LimitRequestFieldsize"
             "LimitRequestLine"
             "LimitXMLRequestBody"
             "Listen"
             "ListenBacklog"
             "LoadFile"
             "LoadModule"
             "LockFile"
             "LogFormat"
             "LogLevel"
             "MCacheMaxObjectCount"
             "MCacheMaxObjectSize"
             "MCacheMaxStreamingBuffer"
             "MCacheMinObjectSize"
             "MCacheRemovalAlgorithm"
             "MCacheSize"
             "MMapFile"
             "MaxClients"
             "MaxKeepAliveRequests"
             "MaxMemFree"
             "MaxRequestsPerChild"
             "MaxRequestsPerThread"
             "MaxSpareServers"
             "MaxSpareThreads"
             "MaxThreads"
             "MaxThreadsPerChild"
             "MetaDir"
             "MetaFiles"
             "MetaSuffix"
             "MimeMagicFile"
             "MinSpareServers"
             "MinSpareThreads"
             "ModMimeUsePathInfo"
             "MultiviewsMatch"
             "NWSSLTrustedCerts"
             "NWSSLUpgradeable"
             "NameVirtualHost"
             "NoCache"
             "NoProxy"
             "NumServers"
             "Options"
             "Order"
             "PassEnv"
             "PidFile"
             "Port"
             "PrivateDir"
             "ProtocolEcho"
             "ProxyBadHeader"
             "ProxyBlock"
             "ProxyDomain"
             "ProxyErrorOverride"
             "ProxyIOBufferSize"
             "ProxyMaxForwards"
             "ProxyPass"
             "ProxyPassReverse"
             "ProxyPreserveHost"
             "ProxyReceiveBufferSize"
             "ProxyRemote"
             "ProxyRemoteMatch"
             "ProxyRequests"
             "ProxyTimeout"
             "ProxyVia"
             "RLimitCPU"
             "RLimitMEM"
             "RLimitNPROC"
             "ReadmeName"
             "Redirect"
             "RedirectMatch"
             "RedirectPermanent"
             "RedirectTemp"
             "RefererIgnore"
             "RefererLog"
             "RemoveCharset"
             "RemoveEncoding"
             "RemoveHandler"
             "RemoveInputFilter"
             "RemoveLanguage"
             "RemoveOutputFilter"
             "RemoveType"
             "RequestHeader"
             "Require"
             "ResourceConfig"
             "RewriteBase"
             "RewriteCond"
             "RewriteEngine"
             "RewriteLock"
             "RewriteLog"
             "RewriteLogLevel"
             "RewriteMap"
             "RewriteOptions"
             "RewriteRule"
             "SSIEndTag"
             "SSIErrorMsg"
             "SSIStartTag"
             "SSITimeFormat"
             "SSIUndefinedEcho"
             "SSLCACertificateFile"
             "SSLCACertificatePath"
             "SSLCARevocationFile"
             "SSLCARevocationPath"
             "SSLCertificateChainFile"
             "SSLCertificateFile"
             "SSLCertificateKeyFile"
             "SSLCipherSuite"
             "SSLEngine"
             "SSLMutex"
             "SSLOptions"
             "SSLPassPhraseDialog"
             "SSLProtocol"
             "SSLProxyCACertificateFile"
             "SSLProxyCACertificatePath"
             "SSLProxyCARevocationFile"
             "SSLProxyCARevocationPath"
             "SSLProxyCipherSuite"
             "SSLProxyEngine"
             "SSLProxyMachineCertificateFile"
             "SSLProxyMachineCertificatePath"
             "SSLProxyProtocol"
             "SSLProxyVerify"
             "SSLProxyVerifyDepth"
             "SSLRandomSeed"
             "SSLRequire"
             "SSLRequireSSL"
             "SSLSessionCache"
             "SSLSessionCacheTimeout"
             "SSLVerifyClient"
             "SSLVerifyDepth"
             "Satisfy"
             "ScoreBoardFile"
             "Script"
             "ScriptAlias"
             "ScriptAliasMatch"
             "ScriptInterpreterSource"
             "ScriptLog"
             "ScriptLogBuffer"
             "ScriptLogLength"
             "ScriptSock"
             "SecureListen"
             "SendBufferSize"
             "ServerAdmin"
             "ServerAlias"
             "ServerLimit"
             "ServerName"
             "ServerPath"
             "ServerRoot"
             "ServerSignature"
             "ServerTokens"
             "ServerType"
             "SetEnv"
             "SetEnvIf"
             "SetEnvIfNoCase"
             "SetHandler"
             "SetInputFilter"
             "SetOutputFilter"
             "StartServers"
             "StartThreads"
             "SuexecUserGroup"
             "ThreadLimit"
             "ThreadStackSize"
             "ThreadsPerChild"
             "TimeOut"
             "TopSites"
             "TopURLs"
             "TransferLog"
             "TypesConfig"
             "UnsetEnv"
             "UseCanonicalName"
             "User"
             "UserDir"
             "VirtualDocumentRoot"
             "VirtualDocumentRootIP"
             "VirtualScriptAlias"
             "VirtualScriptAliasIP"
             "Win32DisableAcceptEx"
             "XBitHack"
             "deny"
             "order"
             "require"

             )
            'words))
          1 'font-lock-keyword-face)

    (list                               ; values
     (regexp-opt '
      (
       "All"
       "AuthConfig"
       "Basic"
       "CONNECT"
       "DELETE"
       "Digest"
       "ExecCGI"
       "FancyIndexing"
       "FileInfo"
       "FollowSymLinks"
       "Full"
       "GET"
       "IconsAreLinks"
       "Includes"
       "IncludesNOEXEC"
       "Indexes"
       "Limit"
       "Minimal"
       "MultiViews"
       "None"
       "OPTIONS"
       "OS"
       "Options"
       "Options"
       "POST"
       "PUT"
       "ScanHTMLTitles"
       "SuppressDescription"
       "SuppressLastModified"
       "SuppressSize"
       "SymLinksIfOwnerMatch"
       "URL"
       "add"
       "allow"
       "any"
       "append"
       "deny"
       "double"
       "downgrade-1.0"
       "email"
       "env"
       "error"
       "force-response-1.0"
       "formatted"
       "from"
       "full"
       "gone"
       "group"
       "inetd"
       "inherit"
       "map"
       "mutual-failure"
       "nocontent"
       "nokeepalive"
       "none"
       "off"
       "on"
       "permanent"
       "referer"
       "seeother"
       "semi-formatted"
       "set"
       "standalone"
       "temporary"
       "unformatted"
       "unset"
       "user"
       "valid-user"
       ) 'words)
     1 'font-lock-type-face)))
  "Expressions to highlight in Apache config buffers.")

(defun apache-indent-line ()
   "Indent current line of Apache code."
   (interactive)
   (let ((savep (> (current-column) (current-indentation)))
         (indent (max (apache-calculate-indentation) 0)))
     (if savep
         (save-excursion (indent-line-to indent))
       (indent-line-to indent))))


(defun apache-previous-indentation ()
  ;; Return the previous (non-empty/comment) indentation.  Doesn't save
  ;; position.
  (let (indent)
    (while (and (null indent)
                (zerop (forward-line -1)))
      (unless (looking-at "[ \t]*\\(#\\|$\\)")
        (setq indent (current-indentation))))
    (or indent 0)))

(defun apache-calculate-indentation ()
  ;; Return the amount the current line should be indented.
  (save-excursion
    (forward-line 0)
    (if (bobp)
        0
      (let ((ends-section-p (looking-at "[ \t]*</"))
            (indent (apache-previous-indentation)) ; moves point!
            (previous-starts-section-p (looking-at "[ \t]*<[^/]")))
        (if ends-section-p
            (setq indent (- indent apache-indent-level)))
        (if previous-starts-section-p
            (setq indent (+ indent apache-indent-level)))
        indent))))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

(provide 'apache-mode)

;;; apache-mode.el ends here
