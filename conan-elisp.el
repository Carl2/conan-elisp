;;; conan-elisp.el --- Generate header for c++ -*- lexical-binding: t -*-

;; Author: Carl Olsen
;; Maintainer: Calle
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (s "1.7.0") (f "0.20.0"))
;; Homepage: https://github.com/Carl2/conan-elisp
;; Keywords: tools


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
;;; blocks.  see https://github.com/Carl2/conan-elisp

;; conan-elisp-install
;;
;; Install Conan packages for `libs', with `flags' specifying the options.
;;
;; Usage:
;; (conan-elisp-install CONAN-LIBS-LIST FLAGS &optional PRE POST)
;;
;; CONAN-LIBS-LIST is a string containing the names and versions of the Conan
;; packages to be installed, separated by space.  For example: "fmt/8.1.1 zlib/1.2.13".
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
;; and calls the `conan-elisp-conan-install` function to install the packages.
;; It then extracts the include and/or library paths for the installed packages
;; based on the FLAGS option, and sets the corresponding Emacs Lisp variables to
;; these paths.
;;
;; PRE - These are compile flags that are added before the conan flags when compiling
;;
;; POST - Flags that are added to the compile flags after the conan flags.
;;
;; Example usage:
;; (conan-elisp-install "fmt/8.1.1 zlib/1.2.13" 'libs "-std=c++20 -Wall -Wextra" "-O3")
;;
;; This installs the Conan packages for fmt and zlib with version numbers 8.1.1 and 1.2.13,
;; respectively, and extracts the library paths for these packages.
;; It will also add the PRE-flags "-std=c++20 -Wall -Wextra" before the conan flags
;;
;; this would possibly look something like:
;; g++ -std=c++20 -Wall -Wextra -I/home/user/.conan2/p/b/fmt038de2f1e357a/p/include \
;;    -I/home/user/.conan2/p/sml056dfa7919d57/p/include \
;;    -L/home/user/.conan2/p/b/fmt038de2f1e357a/p/lib \
;;    -lfmt -lm -O3 main.cpp
;;
;; When compiling
;; Note: This function assumes that Conan 2.0 is installed on the system and that the
;; necessary Conan packages are available.

;; todo alot! Buts its a start..

;;; Code:
(require 'cl-lib)
(require 's)
(require 'f)



(defgroup conan-elisp nil
  "Provides conan support for `org-mode' source block."
  :prefix "conan-elisp-"
  :group 'convenience)


(defun conan-elisp-generic-conan-heading (heading)
  "Generates a header function.
Where the input is a list of string and returns a formatted
string that includes a specified heading followed by the list of
strings."
  (let ((heading heading))
    (lambda(vals) (concat heading "\n  " (mapconcat #'identity vals "\n  ")))))


(defcustom conan-elisp-required-fn (conan-elisp-generic-conan-heading "[requires]")
  "Requires heading in conanfile."
  :group 'conan-elisp
  :type 'function)

(defcustom conan-elisp-generator-fn (conan-elisp-generic-conan-heading "[generators]")
  "Generator tag in conanfile."
  :group 'conan-elisp
  :type 'function)


(defcustom  conan-elisp-conan-install-cmd "conan install . --output-folder=./out --build=missing"
  "Conan install command ."
  :group 'conan-elisp
  :type 'string)

(defcustom  conan-elisp-pkgconfig-flags-cmd "PKG_CONFIG_PATH=%s pkgconf --libs --cflags "
  "The command to execute to get compile flags.
the path is included with %s from the function that needs it.
"
  :group 'conan-elisp
  :type 'string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              construct               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun conan-elisp-construct (libs generators)
  "Generate the content of a conanfile.txt (just requires and generators)"
  (let (( require-str (funcall conan-elisp-required-fn libs))
        ( generator-str (funcall conan-elisp-generator-fn generators)))
    (concat require-str "\n\n" generator-str "\n")))


(defun conan-elisp-make-buffer ( libs gens)
  "Creates a conanfile. and returns the filename"
  (let* (
         (content (conan-elisp-construct libs gens))
         (output-dir (make-temp-file "conan-install-" t))
         (conan-file (f-join output-dir "conanfile.txt")))
    (with-temp-file conan-file
      (insert content))
    conan-file))


(defun conan-elisp-conan-install ( libs generators)
  "Creates a directory structure and adds a conanfile.txt
and runs the conan install."
  (let (
        (current-dir default-directory)
        (conan-file  (conan-elisp-make-buffer libs generators)))
    (cd (f-dirname conan-file))
    (shell-command-to-string conan-elisp-conan-install-cmd )
    (cd current-dir)
    conan-file))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Use the configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conan-elisp-remove-version-from-libs (libs)
  "Removes the version from the list "
  (let ((transform-fn (lambda (lib) (car (s-split "/" lib 'omit-nulls) ))))
    (mapcar transform-fn libs)))


(defun conan-elisp-conan-get-compile-flags (conan-libs output-path)
  "Retuns the complete compile flags for the conan-libs , using output-path.
Both cflags and libs are included"
  (let* (
         (cmd (format conan-elisp-pkgconfig-flags-cmd (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (conan-elisp-remove-version-from-libs conan-libs) " ")))
    (message "cmd %s " (concat cmd lib-no-ver))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))



(defun conan-elisp-conan-get-include (conan-libs output-path)
  "Gets the include directories(-I)"
  (let* ((cmd (format conan-elisp-pkgconfig-flags-cmd (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (conan-elisp-remove-version-from-libs conan-libs) " ")))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))

(defun conan-elisp-conan-get-libs (conan-libs output-path)
  "Gets the libraries(-l) and paths (-L) "
  (let* ((cmd (format conan-elisp-pkgconfig-flags-cmd (f-join output-path "out")))
         (lib-no-ver (mapconcat #'identity (conan-elisp-remove-version-from-libs conan-libs) " ")))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check options and install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun conan-elisp-check-options (opts)
  "Takes a single argument opts which is expected to be one of the
symbols include, libs, both, or all. and returns a function pointer
to retrieve the compile flags (based on argument)."
  (cl-case opts
    (include #'conan-elisp-conan-get-include)
    (libs #'conan-elisp-conan-get-libs )
    (both #'conan-elisp-conan-get-compile-flags)
    (all #'conan-elisp-conan-get-compile-flags)))



(defun conan-elisp-install (conan-libs-list flags &optional pre-flags post-flags)
  "Install Conan packages for `conan-libs-list', with `flags' specifying the
options.
- `conan-libs-list' should be in the format \"fmt/8.1.1 zlib/1.2.13\"
- `flags' could be either ``'include``, ``'libs``, ``'all``, or ``'both``
  (the last two mean the same thing)
- `pre-flags' are optional flags that will be included before the conan compile
flags.
- `post-flags' are optional flags that will be included after the conan compile
flags."
  (let* (
         (current-dir default-directory)
         (compile-fn (conan-elisp-check-options flags))
         (conan-libs (s-split " " conan-libs-list 'omit-nulls))
         (temp-conan-file (conan-elisp-conan-install conan-libs '("PkgConfigDeps")))
         (conan-dir (f-dirname temp-conan-file))
         ;; Concatenating pre-flags, result of compile-fn, and post-flags
         (result (concat (or (concat pre-flags " ") "")
                         (funcall compile-fn conan-libs conan-dir)
                         (or (concat " " post-flags) ""))))
    (cd current-dir)
    result))

(provide 'conan-elisp)
;; Examples
;;(conan-elisp-install "fmt/10.1.1 sml/1.1.6 zlib/1.2.13" 'all)
;; With pre flags
;;(conan-elisp-install "fmt/10.1.1 sml/1.1.9" 'all "-std=c++20 -Wall -Wextra")
;; With post flags
;; (conan-elisp-install "fmt/10.1.1 sml/1.1.9" 'all nil "-std=c++20 -Wall -Wextra")
;; With both pre and post flags
;; (conan-elisp-install "fmt/10.1.1 sml/1.1.9" 'all "-std=c++20 -Wall -Wextra" "-o /tmp/myElf")
;;; conan-elisp.el ends here
