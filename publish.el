;;; publish.el --- Set up org-publish variables for Lens_r blog website -*- lexical-binding: t -*-

;; Author: Rylan Lens Kellogg
;; Maintainer: Rylan Lens Kellogg
;; Version: 0.1.2
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://lensplaysgames.github.io/lensr_blog_v1/
;; Keywords: org, publish, html, web

;; This file is not part of GNU Emacs


;;; Commentary:

;; M-x eval-buffer RET
;; (eval-buffer)

;; See =org/mainblog.org=.

;; TODO: Put a link to RSS feed somewhere accessible but not in the way
;; (maybe a good-looking footer would be perfect for this).

;;; Code:

;; Load minimum amount of packages.
(require 'package)
;; Fontify source code blocks with htmlize.
(when (package-installed-p 'htmlize)
  (let ((package-load-list '((htmlize t))))
    (package-initialize)))

;; Tangling source blocks from literate js and css files requires org-tangle.
(require 'ob-tangle)

(defun blub-file-string (filepath)
  "Return a string containing the contents of file at FILEPATH."
  (interactive "fFile: ")
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun blub-file-size-in-bytes (filepath)
  "Return the file size, in bytes, of the file located at FILEPATH.
If no file exists at FILEPATH, return nil."
  (let ((attributes (file-attributes filepath)))
    (when attributes (file-attribute-size attributes))))

(defun blub-directory-files-nodot (directory &optional full nosort count)
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

(defun blub-concatenate-directory-files (directory filepath &optional append)
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

(defun blub-time-to-rss2-pubdate-string (passed-time)
  "Convert PASSED-TIME object to an rss2 specification (RFC 822) date.
Passing nil will give the current time (as with any time object)."
  (concat
   (substring (format-time-string "%A" passed-time) 0 3)
   (format-time-string ", %d " passed-time)
   (substring (format-time-string "%B" passed-time) 0 3)
   (format-time-string " %Y %T PDT" passed-time) ;; PST may be PDT, sometimes :Þ
   ))

;;; Create pages/links for each blog post.
(let ((blog-links nil)
      (rss2-items nil))
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
           ;; Save the entire contents of each blogpost subtree into a variable.
           ;; Convert org timestamp string into Emacs time object.
           (let ((contents (progn
                             ;; Push the contents of the subtree onto the 'kill-ring'.
                             (org-copy-subtree)
                             ;; Return the last kill, popping it from the kill ring.
                             (current-kill 0))))
             ;; Copy the post into it's own file, that way readers won't get confused
             ;; as to which post is which; one web page per post.
             (with-current-buffer (find-file-noselect post-file-name t)
               (erase-buffer)
               (insert (format "#+title: Lens_r | %s\n" headline) ;; this shows in browser tab
                       (format "#+created: %s\n" publish-date)
                       "#+options: title:nil\n\n"
                       contents)
               (save-buffer)))

           (let ((publish-time (org-timestamp-to-time (org-timestamp-from-string publish-date))))
             ;; Push a link to this newly-created post file, along with it's publish
             ;; date, to a list of all blog post links.
             (push `(,(format "[[file:%s][%s]]" post-file-name headline)
                     ,publish-time)
                   blog-links)

             ;; Push an item to the RSS feed, along with it's publish date.
             (push `(,(concat
                       "    <item>\n"
                       (format "      <title>%s</title>\n" headline)
                       (format "      <link>https://lensplaysgames.github.io/lensr_blog_v1/posts/%s.html</link>\n" custom-id)
                       ;; Date translation...
                       (format "      <pubDate>%s</pubDate>\n" (blub-time-to-rss2-pubdate-string publish-time))
                       "    </item>\n")
                     ,publish-time)
                   rss2-items)))))))

  ;; Sort links by publish date
  (sort blog-links
        (lambda (a b)
          (not (time-less-p (cadr a) (cadr b)))))

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
              (insert "** Published on " (format-time-string "%F" (cadr link)))
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

  ;; Sort rss items by publish date...
  (sort rss2-items
        (lambda (a b)
          (not (time-less-p (cadr a) (cadr b)))))

  ;; Save RSS in an XML file.
  (with-current-buffer (find-file-noselect "extras/rss.xml")
    (erase-buffer)
    (insert "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
            "  <channel>\n"
            "    <title>Lens_r's Blog</title>\n"
            "    <link>https://lensplaysgames.github.io/lensr_blog_v1/</link>\n"
            "    <description>Lens_r sometimes writes things down.</description>\n"
            "    <language>en-us</language>\n"
            ;; pubDate of channel == when it was created.
            "    <pubDate>Mon, 29 May 2023 20:08:15 PDT</pubDate>\n"
            ;; lastBuildDate of channel == when it was last modified (now).
            "    <lastBuildDate>"
            (blub-time-to-rss2-pubdate-string nil)
            "</lastBuildDate>\n"
            "    <docs>https://validator.w3.org/feed/docs/rss2.html</docs>\n"
            "    <generator>Lens_r's Pile of Elisp</generator>\n"
            "    <atom:link href=\"https://lensplaysgames.github.io/lensr_blog_v1/rss.xml\" rel=\"self\" type=\"application/rss+xml\"/>\n")
    (mapc (lambda (rss-item)
            (insert (car rss-item)))
          rss2-items)
    (insert "  </channel>\n"
            "</rss>\n")
    (save-buffer))

  ;; return nil simply because this bit of code is freestanding, so placing
  ;; point after the last paren and doing `C-x C-e` will evaluate the whole
  ;; thing, generating `index.org` and all the blog post files in `org/posts/`.
  nil)

;;; Generate JavaScript and CSS with org-babel.

;; This populates org/js and org/css subdirectories.
(org-babel-tangle-file "org/resources.org")

;;; JS and CSS Minification

;; Ensure resources subdirectory existence
(make-directory "res" t)
;; Consolidate separate JS files into a single one.
(blub-concatenate-directory-files "org/js" "res/all.js")
;; Consolidate separate CSS files into a single one.
(blub-concatenate-directory-files "org/css" "res/all.css")

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

;; Don't include :tags: in org headers in the exported headline.
(setq org-export-with-tags nil)

;; Ensure extras subdirectory existence
(make-directory "extras" t)

;; all.js inlined into header (minified if available).
;; TODO: It would be better to have a function that produces the <script>
;; tag string and then only set 'org-html-scripts' once.
(let ((js-filepath (if (file-exists-p "res/all.min.js")
                       "res/all.min.js"
                     "res/all.js")))
  ;; If JS is larger than 4kb, it is more efficient to link to
  ;; them vs inline them. To accomplish this, we copy any extra files that
  ;; need included in the final published directory (i.e. the linked
  ;; javascript) into the ~extras~ directory.
  (if (>= (blub-file-size-in-bytes js-filepath) 4096)
      (let ((js-basename (file-name-nondirectory js-filepath)))
        (setq org-html-scripts (format "<script type=\"application/javascript\" src=\"/lensr_blog_v1/%s\"/>\n" js-basename))
        ;; Copy "res/all[.min].js" to "extras/all[.min].js"
        (copy-file js-filepath (expand-file-name js-basename "extras" t)))
    (setq org-html-scripts (format "<script>%s</script>\n" (blub-file-string js-filepath)))))

;; NOTE: THIS IS IMPORTANT. Without this, it would just keep growing longer and longer.
(setq org-html-head nil)

;; style.css inlined into header (minified if available).
;; TODO: It would be better to have a function that produces a string and
;; then only set 'org-html-head' once. Or maybe just keep setting
;; org-html-head to a concatenation of itself (append to it).
(let ((css-filepath (if (file-exists-p "res/all.min.css")
                        "res/all.min.css"
                      "res/all.css")))
  ;; If CSS is larger than 4kb, it is more efficient to link to
  ;; them vs inline them. To accomplish this, we copy any extra files that
  ;; need included in the final published directory (i.e. the linked
  ;; stylesheet) into the ~extras~ directory.
  (if (>= (blub-file-size-in-bytes css-filepath) 4096)
      (let ((css-basename (file-name-nondirectory css-filepath)))
        (setq org-html-head (format "<link rel=\"stylesheet\" href=\"/lensr_blog_v1/%s\"/>\n" css-basename))
        ;; Copy "res/all[.min].css" to "extras/all[.min].css"
        (copy-file css-filepath (expand-file-name css-basename "extras") t))
    (setq org-html-head (format "<style>%s</style>\n" (blub-file-string css-filepath)))))


(setq org-html-head (concat org-html-head
                            "<link rel=\"icon\" type=\"image/x-icon\" href=\"favicon.ico\">\n"))

(setq org-publish-project-alist
      `(("export-org"
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
         :html-postamble-format (("en" ,(blub-file-string "org/footer.html.in"))))
        ("copy"
         :publishing-function
         org-publish-attachment
         :publishing-directory "docs"
         :recursive t
         :base-directory "org"
         :base-extension "png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg\\|ico\\|xml"
         :exclude "backup\\|backup/\\|tmp\\|tmp/"
         )
        ("extra"
         :publishing-function
         org-publish-attachment
         :publishing-directory "docs"
         :recursive t
         :base-directory "extras"
         :base-extension "css\\|js")
        ("lensr-blog-site" :components ("export-org" "copy" "extra"))
        ))

(org-publish "lensr-blog-site")
(message "Lens_r blog website has been published.")

;; For local builds, we encapsulate the entire published output
;; directory/site in a `lensr_blog_v1` directory. This allows URLs for
;; local testing/building to match the GitHub way of doing things.
(make-directory "localbuild/lensr_blog_v1" t)
(copy-directory "docs" "localbuild/lensr_blog_v1/" nil t t)

;;; publish.el ends here
