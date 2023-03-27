(defun col/generic-conan-heading (heading)
  "docstring"
  (lexical-let (
                (heading heading))
    (lambda(vals) (concat heading "\n  " (mapconcat #'identity vals "\n  ")))
    ))


(setq required-fn (col/generic-conan-heading "[requires]"))
(setq generator-fn (col/generic-conan-heading "[generators]"))
(setq conan-install-cmd "conan install . --output-folder=./out --build=missing")
(setq output-path "/tmp/conan-elisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              construct               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; (funcall generator-fn '("Apa" "bepa"))

(defun col/construct (libs generators)
  "docstring"
  (let (
        ( require-str (funcall required-fn libs))
        ( generator-str (funcall generator-fn generators))
        )

    (concat require-str "\n\n" generator-str "\n")
    ))
;; So the idea is to provide a list of conan libraries
;; (col/construct '("fmt" "sml") '("elisp-generator"))



(defun col/make-buffer (buffer-name libs gens)
  "docstring"
  (let (
        (buffer (get-buffer-create buffer-name))
        (content (col/construct libs gens))
        (conan-file (f-join output-path "conanfile.txt"))
        )
    (with-current-buffer buffer
      (erase-buffer)
      (insert content)
      (write-file conan-file)
      buffer)))





(defun col/conan-install (dir libs generators)
  "docstring"
  (let (
        (default-directory dir)
        (buffer  (col/make-buffer "test.txt" '("fmt/8.1.1" "sml/1.1.4") '("PkgConfigDeps") ))
        )
    (shell-command-to-string conan-install-cmd )

  ))


(col/conan-install output-path '("fmt/8.1.1" "sml/1.1.4") '("PkgConfigDeps"))
;;(col/make-buffer "test.txt" '("fmt/8.1.1" "sml/1.1.4") '("PkgConfigDeps") )
;;(col/conan-install output-path)
;; The idea is to use pkgconf


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Use the configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun col/remove-version-from-libs (libs)
  "Removes the version from the list "
  (let ((transform-fn (lambda (lib) (car (s-split "/" lib 'omit-nulls) ))))
    (mapcar transform-fn libs)
    ))


;; PKG_CONFIG_PATH=/tmp/conan/out pkgconf --libs --cflags fmt
(defun col/conan-get-compile-flags (conan-libs)
  ""
  (let* ((cmd (format "PKG_CONFIG_PATH=%s pkgconf --libs --cflags " (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (col/remove-version-from-libs conan-libs) " ")))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))



(defun col/conan-get-include (conan-libs)
  "Gets the include directories"
  (let* ((cmd (format "PKG_CONFIG_PATH=%s pkgconf --cflags " (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (col/remove-version-from-libs conan-libs) " ")))
    (shell-command-to-string (concat cmd lib-no-ver))))

;;(col/conan-get-include '("fmt/8.1.1" "sml/1.1.4"))
