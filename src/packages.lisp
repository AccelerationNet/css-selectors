;;;; Copyright (C) 2009 Acceleration.net Russ Tyndall, email: bobbysmith007@gmail.com
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, under version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; Copyright (C) 2009 Acceleration.net Russ Tyndall, email: bobbysmith007@gmail.com
;;;; This program comes with ABSOLUTELY NO WARRANTY; for details see COPYING.
;;;; This is free software, and you are welcome to redistribute it
;;;; under certain conditions; for details see COPYING.

(in-package :cl-user)

(defpackage :css-selectors
    (:nicknames :css)
  (:use :cl :iter)
  (:export :query
	   :node-matches?
	   :parse-results
	   :compile-css-node-matcher))


(defpackage :css-selectors.pseudo
    (:nicknames :pseudo)
  (:shadow :not)
  (:use :css :iter :cl)
  (:export #:not #:has #:is #:root #:first-child #:last-child
	   #:only-child #:nth-child #:nth-last-child))