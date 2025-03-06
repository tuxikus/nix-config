{
  home.file.".config/nyxt/config.lisp" = {
    text = ''
	(define-configuration buffer
	    ((default-modes (append '(emacs-mode) %slot-value%))))
	(defvar *my-search-engines*
	  (list
	   '("google" "https://google.com/search?q=~a" "https://google.com"))
	  "List of search engines.")
	
	(define-configuration context-buffer
	    "Go through the search engines above and make-search-engine out of them."
	  ((search-engines
	    (append %slot-default%
		      (mapcar
		       (lambda (engine) (apply 'make-search-engine engine))
		       *my-search-engines*)))))
	(define-configuration web-buffer
	    ((default-modes
		   (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))
   '';
  };
}
