(defpackage json
  (:use #:cl
        #:esrap
        #:models)
  (:export #:json))

(in-package json)

(defrule json (and ws value ws)
  (:destructure (ws1 value ws2)
    (declare (ignore ws1 ws2))
    value))

(defrule value
    (or "false"
        "true"
        "null"
        object
        array
        number
        string))

(defrule object
    (and begin-object
         (? (and member (* (and value-separator member))))
         end-object)
  (:destructure (begin-object members end-object)
    (declare (ignore begin-object end-object))
    (make-instance 'json-object
                   :members (construct-members members))))

(defrule member (and string name-separator value)
  (:destructure (key name-separator value)
    (declare (ignore name-separator))
    (make-instance 'json-kv-pair :key key :value value)))

(defrule array
    (and begin-array
         (? (and value (* (and value-separator value))))
         end-array)
  (:destructure (begin-object members end-object)
    (declare (ignore begin-object end-object))
    (make-instance 'json-array
                   :members (construct-members members))))

(defun construct-members (members)
  (if members
      (cons (first members)
            (mapcar (lambda (list) (second list))
                    (second members)))
      nil))

(defrule number
    (and (? #\-)
         int
         (? frac)
         (? exp))
  (:text t)
  (:lambda (text)
    (let ((*read-default-float-format* config:*float-precision*))
      (parse-number:parse-number text))))

(defrule int
    (or #\0
        (and positive-digit (* digit)))
  (:text t))

(defrule frac
    (and #\. (+ digit))
  (:text t))

(defrule exp
    (and (or #\e #\E)
         (? (or #\- #\+))
         (+ digit))
  (:text t))

(defrule string
    (and #\"
         (* char)
         #\")
  (:destructure (begin-quote chars end-quote)
    (declare (ignore begin-quote end-quote))
    (coerce chars 'string)))

(defrule char (or escaped unescaped))

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
        escaped-utf16))

(defrule escaped-quotation-mark "\\\""
  (:constant #\"))

(defrule escaped-backslash "\\\\"
  (:constant #\\))

(defrule escaped-slash "\\/"
  (:constant #\/))

(defrule escaped-backspace "\\b"
  (:constant #\backspace))

(defrule escaped-form-feed "\\f"
  (:constant #\page))

(defrule escaped-line-feed "\\n"
  (:constant #\newline))

(defrule escaped-carriage-return "\\r"
  (:constant #\return))

(defrule escaped-tab "\\t"
  (:constant #\tab))

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
    (let* ((code (parse-integer text :start 2 :radix 16)))
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

(defrule unescaped (unescaped-p character))

(defun unescaped-p (char)
  (let ((code (char-code char)))
    (or (<= #x20 code #x21)
        (<= #x23 code #x5B)
        (<= #x5D code #x10FFFF))))

(defrule begin-array (and ws #\[ ws)
  (:constant nil))

(defrule begin-object (and ws #\{ ws)
  (:constant nil))

(defrule end-array (and ws #\] ws)
  (:constant nil))

(defrule end-object (and ws #\} ws)
  (:constant nil))

(defrule name-separator (and ws #\: ws)
  (:constant nil))

(defrule value-separator (and ws #\, ws)
  (:constant nil))

(defrule ws
    (* (or #\space
           #\tab
           #\newline
           #\return))
  (:constant nil))

(defrule positive-digit (character-ranges (#\1 #\9)))

(defrule hex (or (character-ranges (#\a #\f))
                 (character-ranges (#\A #\F))
                 digit))

(defrule digit (character-ranges (#\0 #\9)))
