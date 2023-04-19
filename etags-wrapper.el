;; -*- lexical-binding: t -*-

;; TODO check that all the exported variables here are buffer local.

;; Spesial settings that is not in etags by default
;; Example of how it can be defined
;;  '("--language=none"
;;    "--regex=\"/^[ \\t]*\\(static\\|local\\|virtual\\|protected\\|interface\\)*[ \\t]*class[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\2/\""
;;    "--regex=\"/^[ \\t]*\\(static\\|local\\|virtual\\|protected\\)*[ \\t]*task[ \\t]*\\(static\\|automatic\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+::\\)*?\\([0-9a-zA-Z\\$_]+\\)?[ \\t]*[0-9a-zA-Z\\$_]+/\\4/\""
;;    "--regex=\"/^[ \\t]*\\(static\\|local\\|virtual\\|protected\\)*[ \\t]*function[ \\t]*\\([0-9a-zA-Z\\$_]+\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+::\\)*?\\([0-9a-zA-Z\\$_]+\\)?[ \\t]*[0-9a-zA-Z\\$_]+/\\4/\""
;;    "--regex=\"/^[ \\t]*module[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
;;    "--regex=\"/^[ \\t]*package[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
;;    "--regex=\"/^[ \\t]*program[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
;;    "--regex=\"/^[ \\t]*interface[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
;;    "--regex=\"/^[ \\t]*typedef[ \\t]+.*[ \\t]+\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
;;    "--regex=\"/^[ \\t]*\\`define[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\`\\1/\""
;;    "--regex=\"/^.*}[ \\t]*\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
;;    "--regex=\"/^.*)[ \\t]*\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
;;    "--regex=\"/^[ \\t]*\\(parameter\\|localparam\\).*[ \\t]+\\([0-9a-zA-Z\\$_]+\\)[ \\t]*=.*;/\\2/\"")
(defvar etags-wrapper-switche-def nil
  "define how ctags should find system verilog tags")

(defvar etags-wrapper-path-to-repos nil
  "list of cons: repos to search for systemverilog files . exclude list")

;; Example of use
;;  '("v" "sv" "vh" "svh")
(defvar etags-wrapper-file-extention nil
  "file extentions to use in the search for files to tag")

(defvar etags-wrapper-tag-path (concat (getenv "HOME") "/") ;nil
  "path to puth the TAGS file")

(defvar etags-wrapper-tag-file-post-fix "etags_TAGS"
  "name of the TAGS file")

(defvar etags-wrapper-use-vc-root-for-tags t
  "if not etags-wrapper-path-to-repos is set use current repo of the file you are in")

(defvar etags-wrapper-print-cmd nil
  "print all run commands to the *Messeges* buffer")

;; Generate commandline to run etags
(defun etags-wrapper--run-etags (repo exclutions ctags-switches extentions tag-file)
  "Generate the find/etags commandline and run it"
  (let ((cmd "find")
        (first t))
    (setq cmd (concat cmd " " repo "/"))  ; add "/" in case you have directory ending in .something
                                        ; iterate over paths in the repo to ignore
    (let ((first_it t))
      (dolist (elem exclutions cmd)
        (if (null first)
            (setq cmd (concat cmd " -or")))
        (when first_it
          (setq cmd (concat cmd " \\("))
          (setq first_it nil))
        (setq cmd (concat cmd " -path \"*" elem "*\""))
        (setq first nil))
      (when (null first_it)
        (setq cmd (concat cmd " \\) -prune"))))
                                        ; iterate over file extentions to search in
    (let ((first_it t))
      (dolist (elem extentions cmd)
        (if (null first)
            (setq cmd (concat cmd " -or")))
        (when first_it
          (setq cmd (concat cmd " \\("))
          (setq first_it nil))
        (setq cmd (concat cmd " -name \"*\\." elem "\""))
        (setq first nil))
      (when (null first_it)
        (setq cmd (concat cmd " \\) -print"))))
                                        ; add ctags command
    (let ((etags-run (car (directory-files (invocation-directory) t ".*etags"))))
      (unless etags-run
          (setq etags-run "etags"))
      (setq cmd (concat cmd " | xargs " etags-run " -a"))) ; a hack for now
    (dolist (elem ctags-switches cmd)
      (setq cmd (concat cmd " " elem)))
    (setq cmd (concat cmd " -o " tag-file))
    (if etags-wrapper-print-cmd
        (message cmd))
    (if (tramp-tramp-file-p (buffer-file-name (current-buffer)))
        (tramp-handle-shell-command cmd)
      (shell-command cmd))))

;; generate tag-file-name
(defun etags-wrapper--generate-tag-file-name (tag-repo)
  "generate the tag file name for a given path"
  (concat etags-wrapper-tag-path "/" (md5 tag-repo) etags-wrapper-tag-file-post-fix))

(defun etags-wrapper-regen-tags(regen_all)
  "regenerate the tags file using ctags. so you need to have ctags in your path for this to work"
  (interactive "p")
  (let ((repos etags-wrapper-path-to-repos))
    ; delete excisting TAGS file
    ; iterate over repos
    (dolist (rep repos)
      (let ((exclutions (car (cdr rep)))
            (static (cdr (cdr rep)))
            (repo (car rep))
            (ctags-switches etags-wrapper-switche-def)
            (extentions etags-wrapper-file-extention)
            (tag-file (etags-wrapper--generate-tag-file-name (car rep))))
        (when (or (not static) regen_all)
          (if (file-exists-p tag-file)
              (progn
                (message "deleting file: %s" tag-file)
                (delete-file tag-file)))
          (etags-wrapper--run-etags repo exclutions ctags-switches extentions tag-file))))))

(defadvice xref-find-definitions (around refresh-etags activate)
   "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
   (condition-case err
       ad-do-it
     (error (and (buffer-modified-p)
                 (not (ding))
                 (y-or-n-p "Buffer is modified, save it? ")
                 (save-buffer))
            (etags-wrapper-regen-tags nil)
            ad-do-it)))

(defun etags-wrapper-generate-tags-list (repo-list)
  (let ((res '()))
    (dolist (rep repo-list res)
      (let* ((exclutions (car (cdr rep)))
             (static (cdr (cdr rep)))
             (repo (car rep))
             (tag-name (etags-wrapper--generate-tag-file-name repo)))
        (add-to-list 'res tag-name)))))

(defun etags-wrapper-check-for-tags-table ()
  "check if the tags are loaded and if not check if it can be regenerated"
  ;This needs update to check if vc-root fails
    (if (null (get-buffer etags-wrapper-tag-file-name))
        (cond
         ((not (null etags-wrapper-path-to-repos))
          (etags-wrapper-regen-tags nil)
          (setq tags-table-list (etags-wrapper-generate-tags-list etags-wrapper-path-to-repos))
          t)
         ((not (null etags-wrapper-use-vc-root-for-tags))
          (add-to-list 'etags-wrapper-path-to-repos (cons (vc-root-dir) nil))
          (etags-wrapper-regen-tags nil)
          (setq tags-table-list (etags-wrapper-generate-tags-list etags-wrapper-path-to-repos))
          t)
         (t nil))
      t))

(provide 'etags-wrapper)
