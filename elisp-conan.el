(defun col/generic-conan-heading (heading)
  "docstring"
  (lexical-let (
                (heading heading))
    (lambda(vals) (concat heading "\n  " (mapconcat #'identity vals "\n  ")))
    ))


(setq required-fn (col/generic-conan-heading "[requires]"))
(setq generator-fn (col/generic-conan-heading "[generators]"))
(setq conan-install-cmd "conan install . --output-folder=./out --build=missing")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              construct               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(funcall generator-fn '("Apa" "bepa"))

(defun col/construct (libs generators)
  "docstring"
  (let (
        ( require-str (funcall required-fn libs))
        ( generator-str (funcall generator-fn generators))
        )

    (concat require-str "\n\n" generator-str "\n")
    ))
;; So the idea is to provide a list of conan libraries
(col/construct '("fmt" "sml") '("elisp-generator"))



(defun col/make-buffer (buffer-name libs gens)
  "docstring"
  (let (
        (buffer (get-buffer-create buffer-name))
        (content (col/construct libs gens))
        )
    (with-current-buffer buffer
      (erase-buffer)
      (insert content)
      (write-file "/tmp/conan/conanfile.txt")
      buffer)))





(defun col/conan-install (dir)
  "docstring"
  (let ((default-directory dir))
    (shell-command conan-install-cmd )
  ))


(col/make-buffer "test.txt" '("fmt/8.1.1" "sml/1.1.4") '("PkgConfigDeps") )
(col/conan-install "/tmp/conan")
