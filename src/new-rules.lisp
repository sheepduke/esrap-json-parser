(defpackage json
  (:use #:cl
        #:esrap
        #:models)
  (:export #:json))

(in-package json)

(defrule json (and ws value ws)
  (:function second))

(defrule value
    (or "false"
        "true"
        "null"
        object-or-array
        number
        string)
  (:use-cache nil))

(defrule object-or-array
    (and ws
         (or (and (& #\{) object)
             (and (& #\[) array))
         ws)
  (:function second)
  (:function second)
  (:use-cache nil))

(defrule object
    (and (and #\{ ws) ; begin-object
         (? (and member (* additional-member)))
         (and ws #\})) ; end-object
  (:function second)
  (:destructure (&optional first-member additional-members)
    (make-instance 'json-object
                   :members (when first-member
                              (list* first-member additional-members))))
  (:use-cache nil))

(defrule additional-member
    (and ws #\, ws member)
  (:function fourth)
  (:use-cache nil))

(defrule member
    (and string ws #\: ws value)
  (:destructure (key ws1 name-separator ws2 value)
    (declare (ignore ws1 name-separator ws2))
    (make-instance 'json-kv-pair :key key :value value))
  (:use-cache nil))

(defrule array
    (and (and #\[ ws) ; begin-array
         (? (and value (* additional-element)))
         (and ws #\])) ; end-array
  (:function second)
  (:destructure (&optional first-element additional-elements)
    (make-instance 'json-array
                   :members (when first-element
                              (list* first-element additional-elements))))
  (:use-cache nil))

(defrule additional-element
    (and ws #\, ws value)
  (:function fourth)
  (:use-cache nil))

(defrule number
    (and (? #\-)
         int
         (? frac)
         (? exp))
  (:text t)
  (:lambda (text)
    (let ((*read-default-float-format* config:*float-precision*))
      (parse-number:parse-number text)))
  (:use-cache nil))

(defrule int
    (or #\0
        (and positive-digit (* digit)))
  (:text t)
  (:use-cache nil))

(defrule frac
    (and #\. (+ digit))
  (:text t)
  (:use-cache nil))

(defrule exp
    (and (or #\e #\E)
         (? (or #\- #\+))
         (+ digit))
  (:text t)
  (:use-cache nil))

(defrule string
    (and #\" (* char) #\")
  (:function second)
  (:lambda (chars)
    (coerce chars 'string))
  (:use-cache nil)) ; TEST

(defun unescaped-p (char)
  (let ((code (char-code char)))
    (or (<= #x20 code #x21)
        (<= #x23 code #x5B)
        (<= #x5D code #x10FFFF))))

(defrule char
    (or (and (& #\\) escaped)
        (and (and) (unescaped-p character)))
  (:function second)
  (:use-cache nil))

(defrule escaped
    (or escaped-quotation-mark
        escaped-backslash
        escaped-slash
        escaped-backspace
        escaped-form-feed
        escaped-line-feed
        escaped-carriage-return
        escaped-tab
        escaped-utf8
        escaped-utf16)
  (:use-cache nil))

(defrule escaped-quotation-mark "\\\""
  (:constant #\")
  (:use-cache nil))

(defrule escaped-backslash "\\\\"
  (:constant #\\)
  (:use-cache nil))

(defrule escaped-slash "\\/"
  (:constant #\/)
  (:use-cache nil))

(defrule escaped-backspace "\\b"
  (:constant #\backspace)
  (:use-cache nil))

(defrule escaped-form-feed "\\f"
  (:constant #\page)
  (:use-cache nil))

(defrule escaped-line-feed "\\n"
  (:constant #\newline)
  (:use-cache nil))

(defrule escaped-carriage-return "\\r"
  (:constant #\return)
  (:use-cache nil))

(defrule escaped-tab "\\t"
  (:constant #\tab)
  (:use-cache nil))

(defun basic-multilingual-plane-p (text)
  (let ((code (parse-integer text :start 2 :radix 16)))
    (or (<= #x0000 code #x007F)
        (<= #x0080 code #x07FF)
        (<= #x0800 code #x0FFF)
        (<= #x1000 code #xCFFF)
        (<= #xD000 code #xD7FF)
        (<= #xE000 code #xFFFF))))

(defrule escaped-utf8 (basic-multilingual-plane-p escaped-unicode)
  (:text t)
  (:lambda (text)
    (let ((code (parse-integer text :start 2 :radix 16)))
      (code-char code))))

(defun extended-multilingual-plane-p (text)
  (let ((code (parse-integer text :start 2 :end 6 :radix 16)))
    (print code)
    (<= #xD800 code #xDBFF)))

(defrule escaped-utf16 (extended-multilingual-plane-p escaped-two-unicodes)
  (:lambda (text)
    (let* ((code1 (parse-integer text :start 2 :end 6 :radix 16))
           (code2 (parse-integer text :start 8 :end 12 :radix 16)))
      (code-char (logior #x10000
                         (ash (logand code1 #x3FF) 10)
                         (logand code2 #x3FF))))))

(defrule escaped-two-unicodes (and escaped-unicode escaped-unicode)
  (:text t))

(defrule escaped-unicode (and "\\u" hex hex hex hex)
  (:text t))

(defrule unescaped
    (unescaped-p character)
  (:use-cache nil))

(defrule ws
    (* (character-ranges #\space #\tab #\newline #\return))
  (:constant nil)
  (:use-cache nil))

(defrule positive-digit
    (character-ranges (#\1 #\9))
  (:use-cache nil))

(defrule hex (or (character-ranges (#\a #\f))
                 (character-ranges (#\A #\F))
                 digit)
  (:use-cache nil))

(defrule digit (character-ranges (#\0 #\9))
  (:use-cache nil))
