;;; text-draw.el --- Draw text diagram with keybord.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Installation:
;; Manual:
;; Download the source code and put it wherever you like, e.g. into
;; ~/.emacs.d/text-draw/
;; ```
;; git clone git@github.com:ginqi7/text-draw.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/text-draw/")
;; (require 'text-draw)
;; ```
;;

;;; Code:

(require 'artist)
(require 'array)

(defcustom text-draw-ditaa-cmd "ditaa --svg -r %s -o %s"
  "Ditaa cmd template.")

(defvar text-draw-ditaa-preview-temp-file nil)

(defvar text-draw--margin-left 1)
(defvar text-draw--margin-top 1)

(defun text-draw-line-with-arrow ()
  "Draw line with arrow."
  (interactive)
  (text-draw--run-function-with-mode-change #'text-draw--line-with-arrow))


(defun text-draw--line-with-arrow ()
  "Draw line with arrow."
  (artist-select-op-line)
  (let ((col (artist-current-column))
        (row (artist-current-line))
        col-end)
    (save-excursion
      (picture-forward-column 1)
      (while (or
              (not (thing-at-point 'char))
              (string-blank-p (thing-at-point 'char)))
        (picture-forward-column 1))

      (text-draw--insert-text ">")
      (setq col-end (- (artist-current-column) 2))
      (print (format "%s : %s : %s: %s" col row col-end row))
      (artist-draw-line  col row col-end row))))

(defun text-draw-square-at-point-with-text ()
  "Draw square at point with text."
  (interactive)
  (text-draw--run-function-with-mode-change #'text-draw--square-at-point-with-text))

(defun text-draw--run-function-with-mode-change (fun)
  "Run function with mode change.
FUN is function symbol."

  (when (not (bound-and-true-p artist-mode))
    ;; artist-mode not on.
    (artist-mode))
  (funcall fun)
  (when (bound-and-true-p artist-mode)
    ;; artist-mode on, off it.
    (artist-mode-off))
  (when (not (bound-and-true-p picture-mode))
    ;; picture-mode not on.
    (picture-mode))
  (text-draw-align))


(defun text-draw--square-at-point-with-text()
  "Draw square at point with text."
  (interactive)
  (save-excursion
    (artist-select-op-rectangle)
    (let ((col(artist-current-column))
          (row (artist-current-line))
          (text (read-string "Input text: "))
          text-split
          text-width
          text-height)
      (setq text-split (split-string text "\n" t " +"))
      (picture-newline (+ text-draw--margin-top 1))
      (dolist (line text-split)
        (picture-forward-column (+ col 3))
        (text-draw--insert-text line)
        (picture-newline 1))
      (setq text-height (length text-split))

      (setq text-width (seq-max (mapcar #'text-length text-split)))
      (artist-draw-rect  col row
                         (+ col text-width text-draw--margin-left text-draw--margin-left 1)
                         (+ row text-height text-draw--margin-top text-draw--margin-top 1)))))

(defun text-length (text)
  "Compute TEXT lenght."
  (with-temp-buffer (insert text) (current-column)))

(defun text-draw--insert-text (str)
  "Insert STR text."
  (dolist (char (append str nil))
    (delete-char -1)
    (insert char)
    (call-interactively 'picture-forward-column)))

(defun text-draw-mark-rectangle ()
  "Mark rectangle."
  (interactive)
  (text-draw--rectangle-goto-start)
  (rectangle-mark-mode)
  (rectangle-forward-char 1)
  (rectangle-next-line 1)
  (text-draw--rectangle-goto-end)
  (rectangle-forward-char 1))

(defun text-draw--rectangle-goto-start ()
  "Goto rectangle start."
  (while (not
          (or
           (text-draw--left-top-p)
           (text-draw--left-down-p)
           (text-draw--left-edge-p)))
    (rectangle-backward-char 1))
  (while (not (text-draw--left-top-p)) (rectangle-previous-line 1)))

(defun text-draw--rectangle-goto-end ()
  "Goto rectangle end."
  (while (not
          (or
           (text-draw--right-top-p)
           (text-draw--right-down-p)
           (text-draw--right-edge-p)))
    (rectangle-forward-char 1))
  (while (not (text-draw--right-down-p)) (rectangle-next-line 1)))

(defun text-draw--pixel-x (point)
  "Return the x pixel position of POINT."
  (-
   (car (window-text-pixel-size nil (line-beginning-position) point))
   (line-number-display-width 'pixel)))

(defsubst text-draw--space (xpos)
  "Return a display property that aligns to XPOS."
  `(space :align-to (,xpos)))

(defun text-draw--space-to-pixel (xpos)
  "Make point space aligin to XPOS."
  (put-text-property
   (1- (point))
   (point)

   'display
   (text-draw--space xpos)))

(defun text-draw-align ()
  "Align edge."
  (interactive)
  (save-excursion
    (let ((right-pixel 0))
      (text-draw--rectangle-goto-end)
      (setq right-pixel
            (max right-pixel (text-draw--pixel-x (point))))
      (artist-previous-line 1)
      (setq right-pixel
            (max right-pixel (text-draw--pixel-x (point))))
      (while (not (string= (thing-at-point 'char) "+"))
        (artist-previous-line 1)
        (setq right-pixel
              (max right-pixel (text-draw--pixel-x (point)))))

      (text-draw--space-to-pixel right-pixel)
      (artist-next-line 1)
      (text-draw--space-to-pixel right-pixel)
      (while (not (string= (thing-at-point 'char) "+"))
        (artist-next-line 1)
        (text-draw--space-to-pixel right-pixel))
      (text-draw--space-to-pixel right-pixel))

    ))

(defun text-draw--position-p (pos-preds)
  "Check point if match POS-PREDS."
  (let ((pos
         (list
          (cons "left" (string (preceding-char)))
          (cons "current" (string (following-char)))
          (cons "right"
                (string (char-after (1+ (point)))))
          (cons "up" (string (text-draw--up-pos-char)))
          (cons "down" (string (text-draw--down-pos-char))))))
    (cl-every
     #'identity
     (mapcar
      (lambda (pos-pred)
        (string=
         (cdr (assoc (car pos-pred) pos))
         (cdr pos-pred)))
      pos-preds))))

(defun text-draw--edge-p (pos)
  "Check current point if a POS edge."
  (and
   (string= (string (following-char)) "|")
   (save-excursion
     (while (not (string= (string (following-char)) "+"))
       (artist-previous-line 1))
     (funcall (intern (format "text-draw--%s-top-p" pos))))))

(defun text-draw--left-edge-p ()
  "Check current point if a left edge."
  (text-draw--edge-p "left"))

(defun text-draw--right-edge-p ()
  "Check current point if a right edge."
  (text-draw--edge-p "right"))

(defun text-draw--left-top-p ()
  "Check point if left top."
  (text-draw--position-p
   '(("current" . "+")
     ("right" . "-")
     ("down" . "|"))))

(defun text-draw--right-top-p ()
  "Check point if right top."
  (text-draw--position-p
   '(("current" . "+")
     ("left" . "-")
     ("down" . "|"))))

(defun text-draw--right-down-p ()
  "Check point if right down."
  (text-draw--position-p
   '(("current" . "+")
     ("left" . "-")
     ("up" . "|"))))

(defun text-draw--left-down-p ()
  "Check point if left down."
  (text-draw--position-p
   '(("current" . "+")
     ("right" . "-")
     ("up" . "|"))))

(defun text-draw--up-pos-char ()
  "Get up position char."
  (save-excursion (artist-previous-line 1) (following-char)))

(defun text-draw--down-pos-char ()
  "Get up position char."
  (save-excursion (artist-next-line 1) (following-char)))

(defun text-draw--extract-content ()
  "Extract content in diagram.
Return value is content lines list."
  (save-excursion
    (text-draw-mark-rectangle)
    (call-interactively 'kill-ring-save)
    (with-temp-buffer
      (yank)
      (goto-char (point-min))
      (while (re-search-forward "[+|-]" nil t) (replace-match ""))
      (string-split
       (substring-no-properties (buffer-string))
       "\n" t " +"))))

(defun text-draw--min-size ()
  "Get diagram min size."
  (let ((content (text-draw--extract-content))
        height
        width)
    (setq height
          (+
           (length content)
           text-draw--margin-top text-draw--margin-top 2))
    (setq width
          (+
           (seq-max (mapcar #'length content))
           text-draw--margin-left text-draw--margin-left 2))
    (cons height width))

  )

(defun text-draw-ditaa-chinese-align ()
  "Add align for ditaa chinese parse."
  (let ((regex-block "|.*?|")
        (regex-chinese "\\cc+")
        match-block
        match-chinese
        chinese-lenght
        words
        old-space-count
        new-space-count
        half-space-count
        replace-block-string)
    (while (search-forward-regexp regex-block nil t)
      (setq match-block (substring-no-properties (match-string 0)))
      (setq match-chinese
            (save-excursion
              (with-temp-buffer
                (insert match-block)
                (goto-char (point-min))
                (when (search-forward-regexp regex-chinese nil t)
                  (substring-no-properties (match-string 0))))))
      (setq words
            (string-trim (substring-no-properties match-block 1 -1)))
      (setq old-space-count
            (- (length match-block) (length words) 2))
      (setq new-space-count
            (+ old-space-count (length match-chinese)))
      (setq chinese-lenght (length match-chinese))
      (setq half-space-count (/ new-space-count 2))
      (if (eq 0 (% new-space-count 2))
          (setq replace-block-string
                (concat "|"
                        (make-string half-space-count
                                     (string-to-char " "))
                        words
                        (make-string half-space-count
                                     (string-to-char " "))
                        "|"))
        (setq replace-block-string
              (concat "|"
                      (make-string half-space-count
                                   (string-to-char " "))
                      words
                      (make-string
                       (+ half-space-count 1)
                       (string-to-char " "))
                      "|")))
      (search-backward-regexp regex-block)
      (replace-match replace-block-string))))

(defun text-draw-ditaa ()
  (interactive)
  (let ((buffer-string (buffer-string))
        )
    (when (not text-draw-ditaa-preview-temp-file)
      (setq text-draw-ditaa-preview-temp-file
            (make-temp-file "ditaa-preview--")))
    (with-temp-file text-draw-ditaa-preview-temp-file
      (erase-buffer)
      (insert buffer-string)
      (goto-char (point-min))
      (text-draw-ditaa-chinese-align))
    (start-process-shell-command
     "ditaa"
     nil
     (format text-draw-ditaa-cmd text-draw-ditaa-preview-temp-file
             (concat text-draw-ditaa-preview-temp-file ".svg")))))

(provide 'text-draw)
;;; text-draw.el ends here
