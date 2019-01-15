;;; Rails --- Rails keymap setting
;;; Commentary:

;;; Code:
(projectile-rails-global-mode)

(defun Em ()
  "Find a model."
  (interactive)
  (projectile-rails-find-model))

(defun Cm ()
  "Go to a model connected with the current resource."
  (interactive)
  (projectile-rails-find-current-model))

(defun Ev ()
  "Find a view."
  (interactive)
  (projectile-rails-find-view))

(defun Cv ()
  "Go to a view connected with the current resource."
  (interactive)
  (projectile-rails-find-current-view))

(defun Ec ()
  "Find a controller."
  (interactive)
  (projectile-rails-find-controller))

(defun Cc ()
  "Go to a controller connected with the current resource."
  (interactive)
  (projectile-rails-find-current-controller))

(defun Eserializer ()
  "Find a serializer."
  (interactive)
  (projectile-rails-find-serializer))

(defun Ehelper ()
  "Find a helper."
  (interactive)
  (projectile-rails-find-helper))

(defun Chelper ()
  "Go to a helper connected with the current resource."
  (interactive)
  (projectile-rails-find-current-helper))

(defun Elib ()
  "Find a lib."
  (interactive)
  (projectile-rails-find-lib))

(defun Efeature ()
  "Find a feature."
  (interactive)
  (projectile-rails-find-feature))

(defun Efixture ()
  "Find a fixture."
  (interactive)
  (projectile-rails-find-fixture))

(defun Cfixture ()
  "Go to a fixture connected with the current resource."
  (interactive)
  (projectile-rails-find-current-fixture))

(defun Espec ()
  "Find a spec."
  (interactive)
  (projectile-rails-find-spec))

(defun Cspec ()
  "Go to a spec connected with the current resource."
  (interactive)
  (projectile-rails-find-current-spec))

(defun Etest ()
  "Find a test."
  (interactive)
  (projectile-rails-find-test))

(defun Ctest ()
  "Go to a test connected with the current resource."
  (interactive)
  (projectile-rails-find-current-test))
;;Eunittest
;;Efunctionaltest

(defun Emigration ()
  "Find a migration."
  (interactive)
  (projectile-rails-find-migration))

(defun Cmigration ()
  "Go to a migration connected with the current resource."
  (interactive)
  (projectile-rails-find-current-migration))

(defun Ejavascript ()
  "Find a javascript."
  (interactive)
  (projectile-rails-find-javascript))

(defun Estylesheet ()
  "Find a stylesheet."
  (interactive)
  (projectile-rails-find-stylesheet))

(defun Elog ()
  "Find a log."
  (interactive)
  (projectile-rails-find-log))

(defun Einitializer ()
  "Find a initializer."
  (interactive)
  (projectile-rails-find-initializer))

(defun Eenvironment ()
  "Find a environment."
  (interactive)
  (projectile-rails-find-environment))

(defun Elocale ()
  "Find a locale."
  (interactive)
  (projectile-rails-find-locale))

(defun Emailer ()
  "Find a mailer."
  (interactive)
  (projectile-rails-find-mailer))

(defun Evalidator ()
  "Find a validator."
  (interactive)
  (projectile-rails-find-validator))

(defun Elayout ()
  "Find a layout."
  (interactive)
  (projectile-rails-find-layout))

(defun Ejob ()
  "Find a job."
  (interactive)
  (projectile-rails-find-job))

(defun Etask ()
  "Find a rake task."
  (interactive)
  (projectile-rails-find-rake-task))

(defun Eschema ()
  "Go to schema."
  (interactive)
  (projectile-rails-goto-schema))

(defun Egemfile ()
  "Go to gemfile."
  (interactive)
  (projectile-rails-goto-gemfile))

(defun Eroutes ()
  "Go to routes."
  (interactive)
  (projectile-rails-goto-routes))

(defun Eseeds ()
  "Go to seeds."
  (interactive)
  (projectile-rails-goto-seeds))

(defun Efile_at_point ()
  "Go to a file at point."
  (interactive)
  (projectile-rails-goto-file-at-point))

(defun Espec_helper ()
  "Go to spec helper."
  (interactive)
  (projectile-rails-goto-spec-helper))

;;projectile-rails-extract-region	C-c r x	Extract the selected region to a partial.

(defun Rr ()
  "Select a rake task."
  (interactive)
  (projectile-rails-rake))

(defun Rs ()
  "Run rails server."
  (interactive)
  (projectile-rails-server))

(defun Rdbconsole ()
  "Run rails dbconsole command."
  (interactive)
  (projectile-rails-dbconsole))

(defun Rconsole ()
  "Run rails console command."
  (interactive)
  (projectile-rails-console))

(defun Rgenerate ()
  "Run rails generate command."
  (interactive)
  (projectile-rails-generate))

(defun projectile-rails-generate-cmd (command)
  (projectile-rails-with-root
   (let ((command-prefix (projectile-rails-with-preloader
                          :spring (concat projectile-rails-spring-command " rails generate " command)
                          :zeus (concat projectile-rails-zeus-command " generate " command)
                          :vanilla (concat projectile-rails-vanilla-command " generate " command))))
     (compile
      (projectile-rails--generate-with-completion command-prefix)
      'projectile-rails-generate-mode))))

(defun Rgc ()
  "Run rails generate controller command."
  (interactive)
  (projectile-rails-generate-cmd "controller "))

(defun Rgm ()
  "Run rails generate model command."
  (interactive)
  (projectile-rails-generate-cmd "model "))

(defun Rgd ()
  "Run rails generate migration command."
  (interactive)
  (projectile-rails-generate-cmd "migration "))

(defun Rgt ()
  "Run rails generate integration_test command."
  (interactive)
  (projectile-rails-generate-cmd "integration_test "))

(defun projectile-rails-cmd (command)
    (projectile-rails-with-root
     (compile (projectile-rails-with-preloader
	       :spring (concat projectile-rails-spring-command " rails " command)
	       :zeus (concat projectile-rails-zeus-command " " command)
	       :vanilla (concat projectile-rails-vanilla-command " " command))
	      'projectile-rails-mode)))

(provide 'my-rails)
;;; my-rails.el ends here
