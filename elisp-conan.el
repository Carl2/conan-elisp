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
;;; This can be used in org-source to get the compile flags for source
;;; blocks. see https://github.com/Carl2/conan-elisp

;; col/conan-elisp-install
;;
;; Install Conan packages for `libs', with `flags' specifying the options.
;;
;; Usage:
;; (col/conan-elisp-install CONAN-LIBS-LIST FLAGS)
;;
;; CONAN-LIBS-LIST is a string containing the names and versions of the Conan
;; packages to be installed, separated by space. For example: "fmt/8.1.1 zlib/1.2.13".
;;
;; FLAGS is expected to be one of the symbols 'include, 'libs, 'both, or 'all.
;; These options specify which Conan package information to extract:
;;
;; - 'include extracts the include paths for the Conan packages.
;; - 'libs extracts the library paths for the Conan packages.
;; - 'both extracts both the include and library paths for the Conan packages.
;; - 'all is equivalent to 'both.
;;
;; The function first extracts the Conan package names and versions from CONAN-LIBS-LIST
;; and calls the `col/conan-install` function to install the packages.
;; It then extracts the include and/or library paths for the installed packages
;; based on the FLAGS option, and sets the corresponding Emacs Lisp variables to
;; these paths.
;;
;; Example usage:
;; (col/conan-elisp-install "fmt/8.1.1 zlib/1.2.13" 'libs)
;;
;; This installs the Conan packages for fmt and zlib with version numbers 8.1.1 and 1.2.13,
;; respectively, and extracts the library paths for these packages.
;;
;; Note: This function assumes that Conan is installed on the system and that the
;; necessary Conan packages are available.

;; todo alot! Buts its a start..

;;; Code:

(require 'cl)
(require 's)
(require 'f)



(defgroup conan-elisp nil
  "Provides conan support for org-mode source block."
  :prefix "conan-elisp-"
  :group 'convenience)

(defun col/generic-conan-heading (heading)
  "Generates a function that takes a list of strings as input and
returns a formatted string that includes a specified heading
followed by the list of strings."
  (lexical-let ((heading heading))
    (lambda(vals) (concat heading "\n  " (mapconcat #'identity vals "\n  ")))
    ))


(defcustom required-fn (col/generic-conan-heading "[requires]")
  "Requires heading in conanfile."
  :group 'conan-elisp
  :type 'function)

(defcustom generator-fn (col/generic-conan-heading "[generators]")
  "generator tag in conanfile."
  :group 'conan-elisp
  :type 'function)


(defcustom  conan-install-cmd "conan install . --output-folder=./out --build=missing"
  "Conan install command ."
  :group 'conan-elisp
  :type 'string
  )

(defcustom  pkgconfig-flags-cmd "PKG_CONFIG_PATH=%s pkgconf --libs --cflags "
  "The command to execute to get compile flags.
the path is included with %s from the function that needs it.
"
  :group 'conan-elisp
  :type 'string
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              construct               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun col/construct (libs generators)
  "Generate the content of a conanfile.txt (just requires and generators)"
  (let (( require-str (funcall required-fn libs))
        ( generator-str (funcall generator-fn generators)))
    (concat require-str "\n\n" generator-str "\n")
    ))


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


(defun col/conan-install ( libs generators)
  "Creates a directory structure and adds a conanfile.txt and runs the conan install."
  (let (
        (current-dir default-directory)
        (conan-file  (col/make-buffer  libs  generators)))

    (cd (f-dirname conan-file))
    (shell-command-to-string conan-install-cmd )
    (cd current-dir)
    conan-file
    ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Use the configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun col/remove-version-from-libs (libs)
  "Removes the version from the list "
  (let ((transform-fn (lambda (lib) (car (s-split "/" lib 'omit-nulls) ))))
    (mapcar transform-fn libs)
    ))


(defun col/conan-get-compile-flags (conan-libs output-path)
  "Retuns the complete compile flags for the conan-libs , using output-path.
Both cflags and libs are included"
  (let* (
         (cmd (format pkgconfig-flags-cmd (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (col/remove-version-from-libs conan-libs) " ")))
    (message "cmd %s " (concat cmd lib-no-ver))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))
    ))



(defun col/conan-get-include (conan-libs output-path)
  "Gets the include directories(-I)"
  (let* ((cmd (format pkgconfig-flags-cmd (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (col/remove-version-from-libs conan-libs) " ")))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))

(defun col/conan-get-libs (conan-libs output-path)
  "Gets the libraries(-l) and paths (-L) "
  (let* ((cmd (format pkgconfig-flags-cmd (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (col/remove-version-from-libs conan-libs) " ")))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check options and install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun col/check-options (opts )
  "Takes a single argument opts which is expected to be one of the
symbols include, libs, both, or all. and returns a function pointer
to retrieve the compile flags (based on argument)."
  (cl-case opts
    (include #'col/conan-get-include)
    (libs #'col/conan-get-libs )
    (both #'col/conan-get-compile-flags)
    (all #'col/conan-get-compile-flags)
    ))



(defun col/conan-elisp-install (conan-libs-list flags)
  "Install Conan packages for `libs', with `flags' specifying the options.
- libs should be in the format \"fmt/8.1.1 zlib/1.2.13\"
- flags could be either 'include, 'libs or 'all or 'both,
(the last two means the samething)
"
  (let* (
         (current-dir default-directory)
         (compile-fn (col/check-options flags))
         (conan-libs (s-split " " conan-libs-list 'omit-nulls))
         (temp-conan-file (col/conan-install conan-libs '("PkgConfigDeps")))
         (conan-dir (f-dirname temp-conan-file))

        )
    (cd current-dir)
    (funcall compile-fn conan-libs conan-dir)
    ))

(provide 'conan-elisp)
;; Examples
;;(col/conan-elisp-install "fmt/8.1.1 sml/1.1.6 zlib/1.2.13" 'all)
;;; conan-elisp.el ends here
