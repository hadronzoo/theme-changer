;;; theme-changer.el --- Sunrise/Sunset Theme Changer for Emacs

;; Copyright (C) 2011 Joshua B. Griffith

;; Author: Joshua B. Griffith <josh.griffith@gmail.com>
;; Keywords: color-theme

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Given a location and day/night color themes, this file provides a
;; `change-theme` function that selects the appropriate theme based on
;; whether it is day or night. It will continue to change themes at
;; sunrise and sunset. To install:

;; Set the location:
;;     (setq calendar-location-name "Dallas, TX") 
;;     (setq calendar-latitude 32.85)
;;     (setq calendar-longitude -96.85)

;; Specify the day and night themes:
;;     (require 'theme-changer)
;;     (change-theme 'color-theme-solarized-light 'color-theme-solarized-dark)

;; You may need to add this file path to your loadpath. For example:
;;     (add-to-list 'load-path "~/.emacs.d/elisp/theme-changer")

;;; Code:

(require 'cl)
(require 'solar)

(defun hour-fraction-to-time (date hour-fraction)
  (let*
      ((now (decode-time (current-time)))
       
       (month (first   date))
       (day   (second  date))
       (year  (third   date))
       (zone  (ninth   now))
       
       (frac-hour (cl-truncate hour-fraction))
       (hour (first frac-hour))

       (frac-minutes (cl-truncate (* (second frac-hour) 60)))
       (minute (first frac-minutes))

       (frac-seconds (cl-truncate (* (second frac-minutes) 60)))
       (sec (first frac-seconds)))
    (encode-time sec minute hour day month year zone)))


(defun sunrise-sunset-times (date)
  (let*
      ((l (solar-sunrise-sunset date))
       (sunrise-time (hour-fraction-to-time date (caar l)))
       (sunset-time (hour-fraction-to-time date (caadr l))))
    (list sunrise-time sunset-time)))

(defun daytime-p (sunrise-time sunset-time)
  (let* ((now (current-time)))
    (and (time-less-p sunrise-time now)
	 (time-less-p now sunset-time))))

(defun today () (calendar-current-date))

(defun tomorrow ()
  (calendar-gregorian-from-absolute
   (+ 1 (calendar-absolute-from-gregorian (today)))))

(defun +second (time)
  (time-add time (seconds-to-time 1)))

(defun change-theme (day-theme night-theme)
  (let*
      ((now (current-time))
       
       (today-times    (sunrise-sunset-times (today)))
       (tomorrow-times (sunrise-sunset-times (tomorrow)))
       
       (sunrise-today (first today-times))
       (sunset-today (second today-times))
       (sunrise-tomorrow (first tomorrow-times)))
    
    (if (daytime-p sunrise-today sunset-today)
	(progn
	  (apply (symbol-function day-theme) '())
	  (run-at-time (+second sunset-today) nil
		       'change-theme day-theme night-theme))

      (apply (symbol-function night-theme) '())
      (if (time-less-p now sunrise-today)
	  (run-at-time (+second sunrise-today) nil
		       'change-theme day-theme night-theme)
	(run-at-time (+second sunrise-tomorrow) nil
		     'change-theme day-theme night-theme)))))

(provide 'theme-changer)
