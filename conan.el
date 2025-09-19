;;; conan.el --- Generate flags for c++ using conan 2.0 -*- lexical-binding: t -*-

;; Author: Carl Olsen
;; Maintainer: Calle
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (s "1.7.0") (f "0.20.0"))
;; Homepage: https://github.com/Carl2/conan-elisp
;; Keywords: tools


;; This file is not part of GNU Emacs
;;
;; MIT License
;;
;; Copyright (c) 2023 Carl Olsen
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;; This can be used in org-source to get the compile flags for source
;; blocks.  see https://github.com/Carl2/conan-elisp

;; conan-install
;;
;; Install Conan packages for `libs', with `flags' specifying the options.
;;
;; Usage:
;; (conan-install CONAN-LIBS-LIST FLAGS &optional PRE POST)
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
;; and calls the `conan-conan-install` function to install the packages.
;; It then extracts the include and/or library paths for the installed packages
;; based on the FLAGS option, and sets the corresponding Emacs Lisp variables to
;; these paths.
;;
;; PRE - These are compile flags that are added before the conan flags when compiling
;;
;; POST - Flags that are added to the compile flags after the conan flags.
;;
;; Example usage:
;; (conan-install "fmt/8.1.1 zlib/1.2.13" 'libs "-std=c++20 -Wall -Wextra" "-O3")
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
;; Examples
;; (conan-install "fmt/10.1.1 sml/1.1.6 zlib/1.2.13" 'all)
;; With pre flags
;; (conan-install "fmt/10.1.1 sml/1.1.9" 'all "-std=c++20 -Wall -Wextra")
;; With post flags
;; (conan-install "fmt/10.1.1 sml/1.1.9" 'all nil "-std=c++20 -Wall -Wextra")
;; With both pre and post flags
;; (conan-install "fmt/10.1.1 sml/1.1.9" 'all "-std=c++20 -Wall -Wextra" "-o /tmp/myElf")
;;
;; When compiling
;; Note: This function assumes that Conan 2.0 is installed on the system and that the
;; necessary Conan packages are available.

;; todo alot! Buts its a start..

;;; Code:
(require 'cl-lib)
(require 's)
(require 'f)



(defgroup conan nil
  "Provides conan support for `org-mode' source block."
  :prefix "conan"
  :group 'convenience)


(defun conan-generic-conan-heading (heading)
  "Generate a header function.
Where the input is a list of string and returns a formatted
string that includes a specified HEADING followed by the list of
strings."
  (let ((heading heading))
    (lambda(vals) (concat heading "\n  " (mapconcat #'identity vals "\n  ")))))


(defcustom conan-required-fn (conan-generic-conan-heading "[requires]")
  "Requires heading in conanfile."
  :group 'conan
  :type 'function)

(defcustom conan-generator-fn (conan-generic-conan-heading "[generators]")
  "Generator tag in conanfile."
  :group 'conan
  :type 'function)


(defcustom  conan-conan-install-cmd "conan install . --output-folder=./out --build=missing"
  "Conan install command ."
  :group 'conan
  :type 'string)

(defcustom  conan-pkgconfig-flags-cmd "PKG_CONFIG_PATH=%s pkgconf --libs --cflags "
  "The command to execute to get compile flags.
the path is included with %s from the function that needs it."
  :group 'conan
  :type 'string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              construct               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun conan-construct (libs generators)
  "Generate the content of a conanfile.txt (just requires and GENERATORS).
Argument LIBS The cpp dependable libraries."
  (let (( require-str (funcall conan-required-fn libs))
        ( generator-str (funcall conan-generator-fn generators)))
    (concat require-str "\n\n" generator-str "\n")))


(defun conan-make-buffer ( libs gens)
  "Create a conanfile.  and return the filename.
Argument LIBS Dependable cpp libraries.
Argument GENS generator function."
  (let* (
         (content (conan-construct libs gens))
         (output-dir (make-temp-file "conan-install-" t))
         (conan-file (f-join output-dir "conanfile.txt")))
    (with-temp-file conan-file
      (insert content))
    conan-file))


(defun conan-conan-install ( libs generators &optional profile)
  "Generate conan structure and run conan install.
Argument LIBS Depedable cpp libraries.
Argument GENERATORS conan config generator functions.
Argument PROFILE conan profile to use.
"
  (let (
        (current-dir default-directory)
        (conan-file  (conan-make-buffer libs generators))
        (conan-shell-cmd
         (if profile
             (concat conan-conan-install-cmd " --profile=" profile)
           conan-conan-install-cmd
             )))
    (cd (f-dirname conan-file))
    (shell-command-to-string conan-shell-cmd)
    (cd current-dir)
    conan-file))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Use the configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conan-inspect-create-outpath (output-path)
  "Inspect and create a outpath.
Argument OUTPUT-PATH is the temporary conan generation path."
  (f-join (shell-quote-argument output-path) "out"))


(defun conan-remove-version-from-libs (libs)
  "Remove the version from the list.
Argument LIBS cpp dependable libraries."
  (let ((transform-fn (lambda (lib) (car (s-split "/" lib 'omit-nulls) ))))
    (mapcar transform-fn libs)))


(defun conan-conan-get-compile-flags (conan-libs output-path)
  "Retuns the complete compile flags for the CONAN-LIBS , using OUTPUT-PATH.
Both cflags and libs are included"
  (let* (
         (cmd (format conan-pkgconfig-flags-cmd (conan-inspect-create-outpath output-path)))
         (lib-no-ver (shell-quote-argument (mapconcat #'identity (conan-remove-version-from-libs conan-libs) " "))))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))



(defun conan-conan-get-include (conan-libs output-path)
  "Gets the include directories(-I).
Argument CONAN-LIBS cpp libraries to be installed.
Argument OUTPUT-PATH where to install conan files."
  (let* ((cmd (format conan-pkgconfig-flags-cmd (conan-inspect-create-outpath output-path)))
         (lib-no-ver (shell-quote-argument (mapconcat #'identity (conan-remove-version-from-libs conan-libs) " "))))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))

(defun conan-conan-get-libs (conan-libs output-path)
  "Gets the libraries(-l) and paths (-L).
Argument CONAN-LIBS conan cpp libraries.
Argument OUTPUT-PATH where to find conan files."
  (let* ((cmd (format conan-pkgconfig-flags-cmd (conan-inspect-create-outpath output-path)))
         (lib-no-ver (shell-quote-argument (mapconcat #'identity (conan-remove-version-from-libs conan-libs) " "))))
    (s-chomp (shell-command-to-string (concat cmd lib-no-ver)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check options and install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun conan-check-options (opts)
  "Match option flag with corresponding function.
Argument OPTS Option include,all,libs,both."
  (cl-case opts
    (include #'conan-conan-get-include)
    (libs #'conan-conan-get-libs )
    (both #'conan-conan-get-compile-flags)
    (all #'conan-conan-get-compile-flags)))


(defun conan-install (conan-libs-list flags &optional
 pre-flags post-flags profile)
  "Install Conan packages for `CONAN-LIBS-LIST', with `FLAGS'.
- `conan-libs-list' should be in the format \"fmt/8.1.1 zlib/1.2.13\"
- `flags' could be either \\='include, \\='libs, \\='all, or \\='both
  (the last two mean the same thing)
- `PRE-FLAGS' are optional flags that will be included before the conan
compile flags.
- `POST-FLAGS' are optional flags that will be included after the conan
compile flags.
- `PROFILE' are optional flag to provide a conan profile.
"
  (let* (
         (current-dir default-directory)
         (compile-fn (conan-check-options flags))
         (conan-libs (s-split " " conan-libs-list 'omit-nulls))
         (temp-conan-file (conan-conan-install conan-libs '("PkgConfigDeps") profile))
         (conan-dir (f-dirname temp-conan-file))
         ;; Concatenating pre-flags, result of compile-fn, and post-flags
         (result (concat (or (concat pre-flags " ") "")
                         (funcall compile-fn conan-libs conan-dir)
                         (or (concat " " post-flags) ""))))
    (cd current-dir)
    result))

(provide 'conan)

;;; conan.el ends here
