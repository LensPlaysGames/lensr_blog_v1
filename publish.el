;;; publish.el --- Set up org-publish variables for Lensor Radii site -*- lexical-binding: t -*-

;; Author: Rylan Lens Kellogg
;; Maintainer: Rylan Lens Kellogg
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: TODO
;; Keywords: org, publish, html, web


;; This file is not part of GNU Emacs


;;; Commentary:

;; M-x eval-buffer RET
;; (eval-buffer)

;; See =org/mainblog.org=.

;;; Code:

(defun file-string (file)
  "Return a string containing the contents of FILE."
  (interactive "fFile: ")
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun directory-files-nodot (directory &optional full nosort count)
  "Like 'directory-files' but don't return dotfiles like \".\" or \"..\".

If DIRECTORY is nil, 'default-directory' is used.

FULL, NOSORT, and COUNT are passed to 'directory-files' unchanged."
  (interactive "DDirectory: ")
  (unless directory (setq directory default-directory))
  (let ((files (directory-files directory full
                                directory-files-no-dot-files-regexp
                                nosort count)))
    (if (called-interactively-p)
        (message "%S" files)
      files)))

(defun concatenate-directory-files (directory filepath &optional append)
  "Concatenate all files in DIRECTORY and save the result into FILEPATH.

If APPEND is non-nil, concatenate to the file at FILEPATH."
  (interactive "DDirectory: \nFFile: ")
  (unless directory (setq directory default-directory))
  (with-temp-buffer
    (when (and append (file-exists-p filepath))
      (insert-file-contents filepath))
    (dolist (file (directory-files directory t))
      (when (file-regular-p file) (insert-file-contents file)))
    (write-file filepath)))

;; Load minimum amount of packages.
(require 'package)
;; Fontify source code blocks with htmlize.
(when (package-installed-p 'htmlize)
  (let ((package-load-list '((htmlize t))))
    (package-initialize)))

;;; Create pages/links for each blog post.
(let ((blog-links nil))
  (with-current-buffer (find-file-noselect "org/mainblog.org")
    ;; Ensure "org/posts/" is a valid, empty directory.
    (delete-directory "posts" t)
    (make-directory "posts" t)
    (org-map-entries
     ;; FUNC: Function that will be called when cursor is at beginning of each
     ;; entry's headline. No need to preserve point, but if you want to set a
     ;; custom continuation point set the variable 'org-map-continue-from' to
     ;; the desired point.
     (lambda ()
       (let* ((heading-components (org-heading-components))
              (level (nth 0 heading-components)) ;; fallback: (org-current-level)
              (headline (nth 4 heading-components)) ;; fallback: (org-get-heading)
              (tags-string (nth 5 heading-components)) ;; I have no clue how else to get tags.
              (custom-id    (org-entry-get (point) "CUSTOM_ID"))
              (publish-date (org-entry-get (point) "PUBLISHED"))
              (post-file-name (format "posts/%s.org" custom-id)))
         (when (and (= 1 level)
                    (when (stringp tags-string) (string-match "blogpost" tags-string)))
           ;; Copy the post into it's own file, that way readers won't get confused
           ;; as to which post is which; one web page per post.


           ;; The next thing we yank will be the contents of this subtree (the post).
           (org-copy-subtree)
           (with-current-buffer (find-file-noselect post-file-name t)
             (erase-buffer)
             (insert (format "#+title: Lens_r | %s\n" headline) ;; this shows in browser tab
                     (format "#+created: %s\n" publish-date)
                     "#+options: title:nil\n\n")
             ;; This inserts the actual blog post, copied with 'org-copy-subtree' above.
             (yank)
             (save-buffer))

           ;; Push a link to this newly-created post file, along with it's publish
           ;; date, to the list of blog links.
           (push `(,(format "[[file:%s][%s]]" post-file-name headline)
                   ,publish-date)
                 blog-links))))))

  ;; Sort links by publish date
  (sort blog-links (lambda (a b)
                     (not (time-less-p
                           (org-timestamp-to-time (org-timestamp-from-string (cadr a)))
                           (org-timestamp-to-time (org-timestamp-from-string (cadr b)))))))

  ;; Save list of links to blog posts in an org file.
  (with-current-buffer (find-file-noselect "org/index.org")
    (erase-buffer)
    (insert-file-contents "index.org.in")
    (end-of-buffer)
    (newline)
    (insert "* Blog Posts")
    (newline)
    (mapc (lambda (link)
            (insert "** " (car link))
            (newline)
            (when (cadr link)
              (insert "** Published on " (cadr link))
              (newline)
              (insert ":PROPERTIES:")
              (newline)
              (insert ":HTML_HEADLINE_CLASS: small")
              (newline)
              (insert ":END:")
              (newline)
              ))
          blog-links)
    (save-buffer))
  ;; return nil simply because this bit of code is freestanding, so placing
  ;; point after the last paren and doing `C-x C-e` will evaluate the whole
  ;; thing, generating `index.org` and all the blog post files in `org/posts/`.
  nil)

;;; Generate JavaScript and CSS with org-babel.

(require 'ob-tangle)
;; This populates org/js and org/css subdirectories.
(org-babel-tangle-file "org/resources.org")

;;; JS and CSS Minification

;; Ensure resources subdirectory existence
(make-directory "res" t)
;; Consolidate separate JS files into a single one.
(concatenate-directory-files "org/js" "res/all.js")
;; Consolidate separate CSS files into a single one.
(concatenate-directory-files "org/css" "res/all.css")

;; https://www.npmjs.com/package/uglify-js
;; install with "npm i uglify-js --location=global"
(let ((minjs (executable-find "uglifyjs"))
      (outjs "res/all.min.js"))
  (when minjs
    (when (file-exists-p outjs)
      (delete-file outjs))
    (call-process minjs nil
                  nil ;; '(:file "minjs.txt")
                  nil
                  "res/all.js"
                  "--compress"
                  "--output" (expand-file-name outjs))))

;; https://www.npmjs.com/package/clean-css-cli
;; install with "npm i clean-css-cli --location=global"
(let ((mincss (executable-find "cleancss"))
      (outcss "res/all.min.css"))
  (when mincss
    (when (file-exists-p outcss)
      (delete-file outcss))
    (call-process mincss nil
                  nil ;; '(:file "mincss.txt")
                  nil
                  "-o" (expand-file-name outcss)
                  "res/all.css")))

;; Use css classes, not inline hex values, for syntax highlighting.
(setq org-html-htmlize-output-type 'css)

;; Evaluate babel source blocks without asking.
(setq org-confirm-babel-evaluate nil)

;; Regenerate files whether they are newer or not.
(setq org-publish-use-timestamps-flag nil)

(setq org-export-with-tags nil)

;; all.js inlined into header (minified if available).
(setq org-html-scripts
      (concat "<script>"
              (if (file-exists-p "res/all.min.js")
                  (file-string "res/all.min.js")
                (file-string "res/all.js"))
              "</script>"))

;; style.css inlined into header (minified if available).
(setq org-html-head
      (concat "<style>"
              (if (file-exists-p "res/all.min.css")
                  (file-string "res/all.min.css")
                (file-string "res/all.css"))
              "</style>"))

(setq org-publish-project-alist
      '(("export-org"
         :publishing-function
         org-html-publish-to-html
         :publishing-directory "docs"
         :recursive t
         :base-directory "org"
         :base-extension "org"
         :exclude "~.*~\\|backup\\|backup/\\|tmp\\|tmp/\\|resources\\.org\\|html.org"
         :section-numbers nil
         :with-toc nil
         :auto-sitemap t
         :sitemap-title "Lensor Blog Sitemap"
         :sitemap-sort-files chronologically
         :sitemap-style list
         :html-head-include-default-style nil
         :html-head-include-scripts t
         :html-html5-fancy t
         :html-postamble t
         :html-postamble-format (("en" "<p><small>Made with %c</small></p>")))
        ("copy"
         :publishing-function
         org-publish-attachment
         :publishing-directory "docs"
         :recursive t
         :base-directory "org"
         :base-extension "png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
         :exclude "backup\\|backup/\\|tmp\\|tmp/"
         ;; :include ("emacs-init.org")
         )
        ("lensr-blog-site" :components ("export-org" "copy"))
        ))

(org-publish "lensr-blog-site")
(message "Lens_r blog website has been published.")

;;; publish.el ends here
