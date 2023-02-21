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
    (picture-mode)))


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
        (picture-forward-column (+ col 2))
        (text-draw--insert-text line)
        (picture-newline 1))
      (setq text-height (length text-split))

      (setq text-width (seq-max (mapcar #'length text-split)))
      (artist-draw-rect  col row
                         (+ col text-width text-draw--margin-left text-draw--margin-left 1)
                         (+ row text-height text-draw--margin-top text-draw--margin-top 1)))))

(defun text-draw--insert-text (str)
  "Insert STR text."
  (dolist (char (append str nil)) (insert char)
          ;; (call-interactively 'picture-self-insert)
          ))

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
  (while (not (string= (thing-at-point 'char) "|"))
    (rectangle-backward-char 1))
  (while (not (string= (thing-at-point 'char) "+"))
    (rectangle-previous-line 1)))

(defun text-draw--rectangle-goto-end ()
  "Goto rectangle end."
  (while (not (string= (thing-at-point 'char) "|"))
    (rectangle-forward-char 1))
  (while (not (string= (thing-at-point 'char) "+"))
    (rectangle-next-line 1)))

(provide 'text-draw)
;;; text-draw.el ends here
