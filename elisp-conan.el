;;; conan-elisp.el --- Generate header for c++ -*- lexical-binding: t -*-

;; Author: Carl Olsen
;; Maintainer: Calle
;; Version: 0.0.1
;; Package-Requires: (dependencies)
;; Homepage: https://cocode.se
;; Keywords: conan


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; todo

;;; Code:
(provide 'conan-elisp)

;; (defgroup conan-elisp
;;   "Provides conan support for org-mode source block."
;;   :prefix "conan-elisp-"
;;   :group 'convenience)


(defun col/generic-conan-heading (heading)
  "docstring"
  (lexical-let ((heading heading))
    (lambda(vals) (concat heading "\n  " (mapconcat #'identity vals "\n  ")))
    ))


(defcustom required-fn (col/generic-conan-heading "[requires]")
  "Requires heading in conanfile."
  :group 'conan-elisp
  :type 'function)

;;(setq required-fn (col/generic-conan-heading "[requires]"))
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




;; (col/conan-install '("fmt/8.1.1" "sml/1.1.4") '("PkgConfigDeps"))

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
(defun col/conan-get-compile-flags (conan-libs output-path)
  ""
  (let* (
         (cmd (format "PKG_CONFIG_PATH=%s pkgconf --libs --cflags " (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (col/remove-version-from-libs conan-libs) " ")))
    (message "cmd %s " (concat cmd lib-no-ver))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))
    ))



(defun col/conan-get-include (conan-libs output-path)
  "Gets the include directories"
  (let* ((cmd (format "PKG_CONFIG_PATH=%s pkgconf --cflags " (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (col/remove-version-from-libs conan-libs) " ")))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))

(concat (col/conan-get-include '("fmt/8.1.1" "sml/1.1.4")) "apa")


(defun col/conan-elisp-install (conan-libs-list)
  "docstring"
  (interactive "sEnter a space sepparated list of conan packages: ")
  (let* (
         (current-dir default-directory)
         (conan-libs (s-split " " conan-libs-list 'omit-nulls))
         (temp-conan-file (col/conan-install conan-libs '("PkgConfigDeps")))
         (compile-flags (col/conan-get-compile-flags conan-libs (f-dirname temp-conan-file)))
         )
    (message " conan libs %s \ntemp-conan-dir: %s flags %s" conan-libs temp-conan-file compile-flags )
    (cd current-dir)

    compile-flags
    ))

(col/conan-elisp-install "fmt/8.1.1 sml/1.1.6 zlib/1.2.13")
;; (cd "~/git/cocode/conan-elisp")
;;; conan-elisp.el ends here
