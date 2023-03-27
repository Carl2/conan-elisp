(defun col/generic-conan-heading (heading)
  "docstring"
  (lexical-let ((heading heading))
    (lambda(vals) (concat heading "\n  " (mapconcat #'identity vals "\n  ")))
    ))


(setq required-fn (col/generic-conan-heading "[requires]"))
(setq generator-fn (col/generic-conan-heading "[generators]"))
(setq conan-install-cmd "conan install . --output-folder=./out --build=missing")
;;(setq output-path "/tmp/conan-elisp-")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              construct               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun col/construct (libs generators)
  "Generate the content of a conanfile.txt (just requires and generators)"
  (let (( require-str (funcall required-fn libs))
        ( generator-str (funcall generator-fn generators)))
    (concat require-str "\n\n" generator-str "\n")
    ))
;; So the idea is to provide a list of conan libraries
;; (col/construct '("fmt" "sml") '("elisp-generator"))


(defun col/make-buffer ( libs gens)
  "Creates a conanfile. and returns the filename"
  (let* (
         (content (col/construct libs gens))
         (output-dir (make-temp-file "conan-install-" t))
         (conan-file (f-join output-dir "conanfile.txt")))
    (with-temp-file conan-file
      (insert content))
    conan-file
    ))

;; (col/make-buffer  '("fmt/8.1.1" "sml/1.1.4") '("PkgConfigDeps"))



(defun col/conan-install ( libs generators)
  "Creates a directory structure and adds a conanfile.txt and runs the conan install."
  (let ((conan-file  (col/make-buffer  libs  generators)))
    (cd (f-dirname conan-file))
    (shell-command-to-string conan-install-cmd )
    conan-file
    ))


;;(col/conan-install '("fmt/8.1.1" "sml/1.1.4") '("PkgConfigDeps"))

;; (col/conan-install '("asio/1.27.0" "sml/1.1.4") '("PkgConfigDeps"))
;; (col/make-buffer "test.txt" '("fmt/8.1.1" "sml/1.1.4") '("PkgConfigDeps") )
;; (col/conan-install output-path)
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
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))

;;(concat (col/conan-get-include '("fmt/8.1.1" "sml/1.1.4")) "apa")
