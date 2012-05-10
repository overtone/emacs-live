;;; Here is the sort of configuration to use to add scratch into one's
;;; configuration if using el-get
;;; http://www.emacswiki.org/emacs/el-get.el


(setq el-get-sources
      '(append 

	'(:name scratch
		:type git
		:url "git@github.com:cbbrowne/scratch-el.git"
		;;; or :url "http://github.com/ieure/scratch-el.git"
		;; :info nil
		;; :build nil
		)
	el-get-sources))


;; Then, init.el will run el-get
(require 'scratch)