;; ichiran dictionary module
;; based on JMDict

(in-package #:ichiran/dict)

(defvar *jmdict-path* #p"foobar")

(defvar *jmdict-data* #p"foobar")

(defvar *connection* '("jmdict" "postgres" "" "localhost"))

(load (asdf:system-relative-pathname :ichiran "settings.lisp") :if-does-not-exist nil)

(defgeneric get-kana (obj)
  (:documentation "most popular kana representation"))

(defgeneric get-kanji (obj)
  (:documentation "most popular kanji representation"))

(defgeneric get-text (obj)
  (:documentation "most popular text representation (kanji or kana)")
  (:method (obj) (text obj)))

(defgeneric word-type (obj)
  (:documentation "returns :kanji or :kana or :gap")
  (:method (obj) :gap))

(defclass entry ()
  ((seq :reader seq :col-type integer :initarg :seq)
   (content :reader content :col-type string :initarg :content)
   (root-p :reader root-p :col-type boolean :initform nil :initarg :root-p)
   (n-kanji :accessor n-kanji :col-type integer :initform 0 :initarg :n-kanji)
   (n-kana :accessor n-kana :col-type integer :initform 0 :initarg :n-kana)
   (primary-nokanji :reader primary-nokanji :col-type boolean :initform nil)
   )
  (:metaclass dao-class)
  (:keys seq))

(deftable entry
  (!dao-def))

(defmethod print-object ((obj entry) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a:~a" (seq obj) (n-kanji obj) (n-kana obj))))

(defmethod get-kana ((obj entry))
  (text (car (select-dao 'kana-text (:and (:= 'seq (seq obj)) (:= 'ord 0))))))

(defmethod get-text ((obj entry))
  (text (car (select-dao (if (> (n-kanji obj) 0) 'kanji-text 'kana-text)
                         (:and (:= 'seq (seq obj)) (:= 'ord 0))))))

(defmethod get-kanji ((obj entry))
  (when (> (n-kanji obj) 0)
    (text (car (select-dao 'kanji-text (:and (:= 'seq (seq obj)) (:= 'ord 0)))))))

(defun recalc-entry-stats ()
  (query (:update 'entry :set
                  'n-kanji (:select (:count 'id) :from 'kanji-text :where (:= 'kanji-text.seq 'entry.seq))
                  'n-kana (:select (:count 'id) :from 'kana-text :where (:= 'kana-text.seq 'entry.seq)))))

(defun entry-digest (entry)
  (list (seq entry) (get-text entry) (get-kana entry)))

(defclass simple-text () 
  ((conjugations :accessor word-conjugations :initform nil)
   ))

(defmethod print-object ((obj simple-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (seq obj) (text obj))))

(defclass kanji-text (simple-text)
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (common :reader common :col-type (or db-null integer) :initarg :common)
   (common-tags :reader common-tags :col-type string :initform "" :initarg :common-tags)
   (conjugate-p :reader conjugate-p :col-type boolean :initform t :initarg :conjugate-p)
   (nokanji :reader nokanji :col-type boolean :initform nil :initarg :nokanji)
   (best-kana :accessor best-kana :col-type (or db-null string) :initform :null :initarg :best-kana)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable kanji-text
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!index 'text)
  (!index 'common)
  (!foreign 'entry 'seq :on-delete :cascade))

(defmethod get-kanji ((obj kanji-text))
  (text obj))

(defmethod get-kana :around ((obj kanji-text))
  (let ((bk (best-kana-conj obj)))
    (if (eql bk :null)
        (call-next-method)
        bk)))

(defmethod get-kana ((obj kanji-text))
  "old get-kana, used when everything else fails"
  (loop with regex = (kanji-regex (text obj))
     and kts = (select-dao 'kana-text (:= 'seq (seq obj)) 'ord)
     for kt in kts
     for tkt = (text kt)
     if (ppcre:scan regex tkt) do (return tkt)
     finally (return (text (car kts)))))

(defmethod word-type ((obj kanji-text)) :kanji)

(defclass kana-text (simple-text)
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (common :reader common :col-type (or db-null integer) :initarg :common)
   (common-tags :reader common-tags :col-type string :initform "" :initarg :common-tags)
   (conjugate-p :reader conjugate-p :col-type boolean :initform t :initarg :conjugate-p)
   (nokanji :reader nokanji :col-type boolean :initform nil :initarg :nokanji)
   (best-kanji :accessor best-kanji :col-type (or db-null string) :initform :null :initarg :best-kanji)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable kana-text
  (!dao-def)
  (!index 'seq)
  (!index 'ord)
  (!index 'text)
  (!index 'common)
  (!foreign 'entry 'seq :on-delete :cascade))

(defmethod get-kana ((obj kana-text))
  (text obj))

(defmethod get-kanji ((obj kana-text))
  (let ((bk (best-kanji-conj obj)))
    (unless (eql bk :null) bk)))

(defmethod word-type ((obj kana-text)) :kana)


(defmethod common ((obj entry) &aux (seq (seq obj)))
  (query (:select (:max 'common) :from (:as (:union
          (:select 'common :from 'kanji-text :where (:= 'seq seq))
          (:select 'common :from 'kana-text :where (:= 'seq seq))) 'tmp))
         :single))

(defclass sense ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (ord :reader ord :col-type integer :initarg :ord))
  (:metaclass dao-class)
  (:keys id))

(deftable sense
  (!dao-def)
  (!index 'seq)
  (!foreign 'entry 'seq :on-delete :cascade))

(defclass gloss ()
  ((id :reader id :col-type serial)
   (sense-id :reader sense-id :col-type integer :initarg :sense-id)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   )
  (:documentation "English meaning")
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj gloss) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "(~a) ~a" (ord obj) (text obj))))

(deftable gloss
  (!dao-def)
  (!index 'sense-id)
  (!foreign 'sense 'sense-id 'id :on-delete :cascade))

(defclass sense-prop ()
  ((id :reader id :col-type serial)
   (tag :reader tag :col-type string :initarg :tag)
   (sense-id :reader sense-id :col-type integer :initarg :sense-id)
   (text :reader text :col-type string :initarg :text)
   (ord :reader ord :col-type integer :initarg :ord)
   (seq :reader seq :col-type integer :initarg :seq)
   )
  (:documentation "sense properties")
  (:metaclass dao-class)
  (:keys id))
  
(defmethod print-object ((obj sense-prop) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a:~a" (tag obj) (text obj))))

(deftable sense-prop
  (!dao-def)
  (!index 'sense-id 'tag)
  (!index 'tag 'text)
  (!index 'seq 'tag 'text)
  (!foreign 'entry 'seq :on-delete :cascade)
  (!foreign 'sense 'sense-id 'id :on-delete :cascade))

(defclass restricted-readings ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (reading :reader reading :col-type string :initarg :reading)
   (text :reader text :col-type string :initarg :text))
  (:metaclass dao-class)
  (:keys id))

(defmethod print-object ((obj restricted-readings) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a -> ~a" (reading obj) (text obj))))

(deftable restricted-readings
  (!dao-def)
  (!index 'seq 'reading)
  (!foreign 'entry 'seq :on-delete :cascade))

(defclass conjugation ()
  ((id :reader id :col-type serial)
   (seq :reader seq :col-type integer :initarg :seq)
   (from :reader seq-from :col-type integer :initarg :from)
   (via :reader seq-via :col-type (or integer db-null) :initform :null :initarg :via)
   )
  (:documentation "conjugation link")
  (:metaclass dao-class)
  (:keys id))

(deftable conjugation
  (!dao-def)
  (!index 'seq)
  (!index 'from)
  (!foreign 'entry 'seq :on-delete :cascade)
  (!foreign 'entry 'from 'seq :on-delete :cascade))

(defmethod print-object ((obj conjugation) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (let ((via (seq-via obj)))
      (if (eql via :null)
          (format stream "~a -> ~a" (seq-from obj) (seq obj))
          (format stream "~a -> ~a -> ~a" (seq-from obj) (seq-via obj) (seq obj))))))

(defclass conj-prop ()
  ((id :reader id :col-type serial)
   (conj-id :reader conj-id :col-type integer :initarg :conj-id)
   (conj-type :reader conj-type :col-type integer :initarg :conj-type)
   (pos :reader pos :col-type string :initarg :pos)
   (neg :reader conj-neg :col-type (or db-null boolean) :initarg :neg)
   (fml :reader conj-fml :col-type (or db-null boolean) :initarg :fml))
  (:metaclass dao-class)
  (:keys id))

(deftable conj-prop
  (!dao-def)
  (!index 'conj-id)
  (!foreign 'conjugation 'conj-id 'id :on-delete :cascade))

(defun conj-info-short (obj)
  (format nil "[~a] ~a~[ Affirmative~; Negative~]~[ Plain~; Formal~]"
          (pos obj) 
          (get-conj-description (conj-type obj))
          (case (conj-neg obj) ((nil) 0) ((t) 1))
          (case (conj-fml obj) ((nil) 0) ((t) 1))
          ))

(defun conj-prop-json (obj)
  (let ((js (jsown:new-js
              ("pos" (pos obj))
              ("type" (get-conj-description (conj-type obj)))))
        (neg (conj-neg obj))
        (fml (conj-fml obj)))
    (unless (or (not neg) (eql neg :null))
      (jsown:extend-js js ("neg" neg)))
    (unless (or (not fml) (eql fml :null))
      (jsown:extend-js js ("fml" fml)))
    js))

(defun delete-duplicate-props ()
  (query "DELETE FROM conj_prop cp1 USING conj_prop cp2
          WHERE cp1.id < cp2.id AND cp1.conj_id = cp2.conj_id
            AND cp1.conj_type = cp2.conj_type
            AND cp1.pos = cp2.pos
            AND cp1.neg IS NOT DISTINCT FROM cp2.neg
            AND cp1.fml IS NOT DISTINCT FROM cp2.fml"))

(defmethod print-object ((obj conj-prop) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (princ (conj-info-short obj) stream)))

(defclass conj-source-reading ()
  ((id :reader id :col-type serial)
   (conj-id :reader conj-id :col-type integer :initarg :conj-id)
   (text :reader text :col-type string :initarg :text)
   (source-text :reader source-text :col-type string :initarg :source-text)
   )
  (:metaclass dao-class)
  (:keys id))

(deftable conj-source-reading
  (!dao-def)
  (!index 'conj-id 'text)
  (!foreign 'conjugation 'conj-id 'id :on-delete :cascade))

(defmethod print-object ((obj conj-source-reading) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a -> ~a" (source-text obj) (text obj))))

(defstruct conj-data seq from via prop src-map)

(defun get-conj-data (seq &optional from/conj-ids)
  "from/conj-ids can be either from which word to find conjugations or a list of conj-ids"
  (loop for conj in (cond
                      ((eql from/conj-ids :root) nil)
                      ((null from/conj-ids)
                       (select-dao 'conjugation (:= 'seq seq)))
                      ((listp from/conj-ids)
                       (select-dao 'conjugation (:and (:= 'seq seq) (:in 'id (:set from/conj-ids)))))
                      (t (select-dao 'conjugation (:and (:= 'seq seq) (:= 'from from/conj-ids)))))
       for src-map = (query (:select 'text 'source-text :from 'conj-source-reading
                                     :where (:= 'conj-id (id conj))))
       nconcing (loop for prop in (select-dao 'conj-prop (:= 'conj-id (id conj)))
                     collect (make-conj-data :seq (seq conj) :from (seq-from conj)
                                             :via (let ((via (seq-via conj)))
                                                    (if (eql via :null) nil via))
                                             :prop prop
                                             :src-map src-map
                                             ))))

(defun get-original-text-once (conj-datas texts)
  (unless (listp texts)
    (setf texts (list texts)))
  (unless (listp conj-datas)
    (setf conj-datas (list conj-datas)))
  (loop for conj-data in conj-datas
       nconc (loop for (txt src-txt) in (conj-data-src-map conj-data)
                if (find txt texts :test 'equal) collect src-txt)))
  
(defun get-original-text* (conj-datas texts)
  (unless (listp texts)
    (setf texts (list texts)))
  (unless (listp conj-datas)
    (setf conj-datas (list conj-datas)))
  (loop for conj-data in conj-datas
       nconc
       (let ((src-text (loop for (txt src-txt) in (conj-data-src-map conj-data)
                          if (find txt texts :test 'equal) collect src-txt)))
         (if (not (conj-data-via conj-data))
             (mapcar (lambda (txt) (list txt (conj-data-from conj-data))) src-text)
             (let ((new-cd (get-conj-data (conj-data-via conj-data) (conj-data-from conj-data))))
                     (get-original-text* new-cd src-text))))))

(defgeneric get-original-text (reading)
  (:documentation "Returns unconjugated text(s) for reading")
  (:method ((reading simple-text))
    (let ((orig-texts (get-original-text* (word-conj-data reading) (text reading)))
          (table (case (word-type reading) (:kanji 'kanji-text) (:kana 'kana-text))))
      (loop for (txt seq) in orig-texts
           nconc (select-dao table (:and (:= 'seq seq) (:= 'text txt)))))))

;;;;

(defun best-kana-conj (obj)
  (cond ((not (eql (best-kana obj) :null)) (best-kana obj))
        (t (let* ((parents (query (:select 'kt.id 'conj.id
                                           :from (:as 'kanji-text 'kt)
                                           (:as 'conj-source-reading 'csr)
                                           (:as 'conjugation 'conj)
                                           :where (:and
                                                   (:= 'conj.seq (seq obj))
                                                   (:= 'conj.id 'csr.conj-id)
                                                   (:= 'csr.text (text obj))
                                                   (:= 'kt.seq (:case ((:not-null 'conj.via) 'conj.via)
                                                                 (:else 'conj.from)))
                                                   (:= 'kt.text 'csr.source-text))))))
             (loop for (pid cid) in parents
                  for parent-kt = (get-dao 'kanji-text pid)
                  for parent-bk = (best-kana-conj parent-kt)
                  unless (eql parent-bk :null)
                  do (let ((readings (query (:select 'text :from 'conj-source-reading
                                                     :where (:and (:= 'conj-id cid)
                                                                  (:= 'source-text parent-bk)))
                                            :column)))
                       (when readings
                         (return
                           (if (= (length readings) 1)
                               (car readings)
                               (let ((km (kanji-cross-match (text parent-kt) parent-bk (text obj))))
                                 (or (car (member km readings :test 'equal))
                                     (loop with regex = (kanji-regex (text obj))
                                        for rd in readings
                                        if (ppcre:scan regex rd) do (return rd)
                                        finally (return (car readings)))))))))
                  finally (return :null))))))


(defun best-kanji-conj (obj)
  (cond ((not (eql (best-kanji obj) :null)) (best-kanji obj))
        ((or (nokanji obj) (= (n-kanji (get-dao 'entry (seq obj))) 0))
         :null)
        (t (let* ((parents (query (:select 'kt.id 'conj.id
                                           :from (:as 'kana-text 'kt)
                                           (:as 'conj-source-reading 'csr)
                                           (:as 'conjugation 'conj)
                                           :where (:and
                                                   (:= 'conj.seq (seq obj))
                                                   (:= 'conj.id 'csr.conj-id)
                                                   (:= 'csr.text (text obj))
                                                   (:= 'kt.seq (:case ((:not-null 'conj.via) 'conj.via)
                                                                 (:else 'conj.from)))
                                                   (:= 'kt.text 'csr.source-text))))))
             (loop for (pid cid) in parents
                  for parent-bk = (best-kanji-conj (get-dao 'kana-text pid))
                  unless (eql parent-bk :null)
                  do (let* ((readings (query (:select 'text :from 'conj-source-reading
                                                     :where (:and (:= 'conj-id cid)
                                                                  (:= 'source-text parent-bk)))
                                            :column))
                            (matching-readings
                             (some (lambda (reading) (and (kanji-match reading (text obj)) reading))
                                   readings)))
                       (when matching-readings
                         (return matching-readings)))
                  finally (return :null))))))
                    

;;;

(defun find-word (word &key root-only)
  (let ((table (if (test-word word :kana) 'kana-text 'kanji-text)))
    (if root-only
        (query-dao table (:select 'wt.* :from (:as table 'wt) :inner-join 'entry :on (:= 'wt.seq 'entry.seq)
                                  :where (:and (:= 'text word)
                                               'root-p)))
        (select-dao table (:= 'text word)))))

(defun find-words-seqs (words seqs)
  "generalized version of find-word-seq from dict-grammar"
  (unless (listp words)
    (setf words (list words)))
  (unless (listp seqs)
    (setf seqs (list seqs)))
  (loop for word in words
     if (test-word word :kana)
     collect word into kana-words
     else
     collect word into kanji-words
     finally
       (let ((kw (when kanji-words (select-dao 'kanji-text (:and (:in 'text (:set kanji-words)) (:in 'seq (:set seqs))))))
             (rw (when kana-words (select-dao 'kana-text (:and (:in 'text (:set kana-words)) (:in 'seq (:set seqs)))))))
         (return (nconc kw rw)))))

(defun word-readings (word)
  (let* ((kana-seq (query (:select 'seq :from 'kana-text :where (:= 'text word)) :column))
         (readings
          (if kana-seq (list word)
              (let* ((kanji-seq (query (:select 'seq :from 'kanji-text
                                                :where (:= 'text word)) :column)))
                (query (:order-by 
                        (:select 'text :from 'kana-text :where
                                 (:in 'seq (:set kanji-seq)))
                        'id) :column)))))
    (values readings (mapcar #'ichiran:romanize-word readings))))

;; Proxy text (kanji-text or kana-text with changed spelling)

(defclass proxy-text (simple-text)
  ((text :reader text :initarg :text)
   (kana :reader get-kana :initarg :kana)
   (source :reader source :initarg :source)))

(defgeneric true-text (obj)
  (:documentation "Returns true text for reading")
  (:method (obj) (text obj))
  (:method ((obj proxy-text)) (true-text (source obj))))

(defmethod word-conjugations ((obj proxy-text))
  (word-conjugations (source obj)))

(defmethod (setf word-conjugations) (value (obj proxy-text))
  (setf (word-conjugations (source obj)) value))

(defmethod seq ((obj proxy-text))
  (seq (source obj)))

(defmethod common ((obj proxy-text))
  (common (source obj)))

(defmethod ord ((obj proxy-text))
  (ord (source obj)))

(defmethod nokanji ((obj proxy-text))
  (nokanji (source obj)))

(defmethod word-type ((obj proxy-text))
  (word-type (source obj)))

(defmethod get-original-text ((reading proxy-text))
  (get-original-text (source reading)))

;; Compound words (2 or more words squished together)

(defclass compound-text ()
  ((text :reader text :initarg :text)
   (kana :reader get-kana :initarg :kana)
   (primary :reader primary :initarg :primary)
   (words :reader words :initarg :words)
   (score-mod :reader score-mod :initarg :score-mod)
   ))

(defmethod seq ((obj compound-text))
  (mapcar #'seq (words obj)))

(defmethod print-object ((obj compound-text) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a" (seq obj) (text obj))))

(defmethod common ((obj compound-text))
  (common (primary obj)))

(defmethod ord ((obj compound-text))
  (ord (primary obj)))

(defmethod word-type ((obj compound-text)) (word-type (primary obj)))

(defgeneric adjoin-word (word1 word2 &key text kana score-mod)
  (:documentation "make compound word from 2 words"))

(defmethod adjoin-word :around (word1 word2 &key text kana score-mod)
  (call-next-method word1 word2
                    :text (or text (concatenate 'string (get-text word1) (get-text word2)))
                    :kana (or kana (concatenate 'string (get-kana word1) (get-kana word2)))
                    :score-mod (or score-mod 0)))

(defmethod adjoin-word ((word1 simple-text) (word2 simple-text) &key text kana score-mod)
  (make-instance 'compound-text
                 :text text :kana kana :primary word1 :words (list word1 word2) :score-mod score-mod))

(defmethod adjoin-word ((word1 compound-text) (word2 simple-text) &key text kana score-mod)
  (with-slots ((s-text text) (s-kana kana) (s-words words) (s-score-mod score-mod)) word1
    (setf s-text text s-kana kana
          s-words (append s-words (list word2))
          s-score-mod (+ s-score-mod score-mod)))
  word1)

(defgeneric word-conj-data (word)
  (:documentation "conjugation data for word"))

(defmethod word-conj-data ((word simple-text))
  (get-conj-data (seq word) (word-conjugations word)))

(defmethod word-conj-data ((word compound-text))
  (word-conj-data (car (last (words word)))))

(defmethod word-conjugations ((word compound-text))
  (word-conjugations (car (last (words word)))))

(defmethod (setf word-conjugations) (value (word compound-text))
  (setf (word-conjugations (car (last (words word)))) value))

(defstruct segment
  start end word (score nil) (info nil) (top nil)) ;; (accum 0) (path nil)

(defun length-multiplier (length power len-lim)
  "len^power until len-lim, goes linear after"
  (cond ((<= length len-lim) (expt length power))
        (t (* length (expt len-lim (1- power))))))

(defparameter *length-coeff-sequences*
  '((:strong 1 8 24 40 60)
    (:weak 1 4 9 16 25 36)
    (:tail 4 9 16 24)))

(defun length-multiplier-coeff (length class)
  (let ((coeffs (assoc class *length-coeff-sequences*)))
    (if (< 0 length (length coeffs))
        (elt coeffs length)
        (* length (/ (car (last coeffs)) (1- (length coeffs)))))))

(defun kanji-break-penalty (kanji-break score &key info)
  (let ((end (cond ((cdr kanji-break) :both)
                   ((eql (car kanji-break) 0) :beg)
                   (t :end)))
        (bonus 0))
    (when info
      (cond ((and (eql end :beg) (member "num" (getf info :posi) :test 'equal))
             (incf bonus 5))
            ))
    (if (>= score *score-cutoff*)
        (max *score-cutoff* (+ (ceiling score 2) bonus))
        score)))

;; *skip-words* *(semi-/non-)final-prt* *weak-conj-types* *skip-conj-forms* are defined in dict-errata.lisp

(defun calc-score (reading &key final use-length (score-mod 0) kanji-break)
  (when (typep reading 'compound-text)
    (multiple-value-bind (score info) (calc-score (primary reading)
                                                  :use-length (mora-length (text reading))
                                                  :score-mod (score-mod reading))
      (setf (getf info :conj) (word-conj-data reading))
      (when kanji-break (setf score (kanji-break-penalty kanji-break score :info info)))
      (return-from calc-score
        (values score info))))

  (let* ((score 1) prop-score
         (kanji-p (eql (word-type reading) :kanji))
         (katakana-p (and (not kanji-p) (> (count-char-class (text reading) :katakana-uniq) 0)))
         (text (text reading))
         (n-kanji (count-char-class text :kanji))
         ;(kanji-prefix (kanji-prefix text))
         (len (max 1 (mora-length text)))
         (seq (seq reading))
         (ord (ord reading))
         (entry (get-dao 'entry seq))
         (conj-only (let ((wc (word-conjugations reading))) (and wc (not (eql wc :root)))))
         (root-p (and (not conj-only) (root-p entry)))
         (conj-data (word-conj-data reading))
         (conj-of (mapcar #'conj-data-from conj-data))
         (secondary-conj-p (and conj-data (every #'conj-data-via conj-data)))
         (conj-types (unless root-p (mapcar (lambda (cd) (conj-type (conj-data-prop cd))) conj-data)))
         (conj-types-p (or root-p (set-difference conj-types *weak-conj-types*)))
         (seq-set (cons seq conj-of)) ;;(if root-p (list seq) (cons seq conj-of)))
         (prefer-kana
          (select-dao 'sense-prop (:and (:in 'seq (:set (if (and root-p (not use-length)) (list seq) seq-set)))
                                        (:= 'tag "misc") (:= 'text "uk"))))
         (posi (query (:select 'text :distinct :from 'sense-prop
                               :where (:and (:in 'seq (:set seq-set)) (:= 'tag "pos"))) :column))
         (common (if conj-only :null (common reading)))
         (common-of common)
         (common-p (not (eql common :null)))
         (particle-p (member "prt" posi :test 'equal))
         (semi-final-particle-p (member seq *semi-final-prt*))
         (non-final-particle-p (member seq *non-final-prt*))
         (pronoun-p (member "pn" posi :test 'equal))
         (cop-da-p (member "cop-da" posi :test 'equal))
         (long-p (> len
                    (if (or (and kanji-p (not prefer-kana)
                                 (or root-p (and use-length (member 13 conj-types))))
                            (and common-p (< 0 common 10)))
                        2 3)))
         (no-common-bonus (or particle-p
                              (and (not use-length) (not conj-types-p))
                              (and (not long-p) (equal posi '("int")))))
         (primary-p nil))
    (when (or (intersection seq-set *skip-words*)
              (and (not final) (member seq *final-prt*))
              (and (not root-p) (skip-by-conj-data conj-data)))
      (return-from calc-score 0))
    (when (and conj-data (not (and (= ord 0) common-p)))
      (let ((conj-of-data (loop for ot in (get-original-text reading) collect (list (common ot) (ord ot)))))
        (when conj-of-data
          (unless common-p
            (let ((conj-of-common (mapcan (lambda (row) (unless (eql (car row) :null) (list (car row)))) conj-of-data)))
              (when conj-of-common
                (setf common 0 common-p t common-of (car (sort conj-of-common #'compare-common))))))
          (let ((conj-of-ord (reduce 'min conj-of-data :key 'second)))
            (when (< conj-of-ord ord) (setf ord conj-of-ord))))))

    (setf primary-p 
          (or (and prefer-kana conj-types-p
                   (not kanji-p)
                   (or (not (primary-nokanji entry))
                       (nokanji reading)))
              (and (or (= ord 0) cop-da-p)
                   (or kanji-p conj-types-p)
                   (or (and kanji-p (not prefer-kana))
                       (and common-p pronoun-p)
                       (= (n-kanji entry) 0)))
              (and prefer-kana kanji-p (= ord 0)
                   (not (query (:select 'id :from 'sense
                                        :where (:and (:in 'id (:set (mapcar 'sense-id prefer-kana)))
                                                     (:= 'ord 0))))))
              ))

    (when primary-p
      (incf score (cond (long-p 10)
                        (common-p 5)
                        ((or prefer-kana (= (n-kanji entry) 0)) 3)
                        (t 2))))
    (when (and particle-p (or final (not semi-final-particle-p)))
      (incf score 2)
      (when common-p
        (incf score (+ 2 len)))
      (when (and final (not non-final-particle-p))
        (cond (primary-p (incf score 5))
              (semi-final-particle-p (incf score 2)))))
    (when (and common-p (not no-common-bonus))
      (let ((common-bonus
             (cond
               (secondary-conj-p (if primary-p 4 2))
               ((or long-p cop-da-p (and root-p (or kanji-p (and primary-p (> len 2)))))
                (cond ((= common 0) 10)
                      ((not primary-p) (max (- 15 common) 10))
                      (t (max (- 20 common) 10))))
               (kanji-p 8)
               (primary-p 4)
               ((or (> len 2) (< 0 common 10)) 3)
               (t 2))))
        (incf score common-bonus)))
    (when long-p
      (setf score (max len score)))
    (when kanji-p
      (setf score (max 5 score))
      (when (and long-p kanji-p (or (> n-kanji 1) (> len 4)))
        (incf score 2)))
    (setf prop-score score)
    (setf score (* score (+ (length-multiplier-coeff len (if (or kanji-p katakana-p) :strong :weak))
                            (if (> n-kanji 1) (* (1- n-kanji) 5) 0)
                            (if use-length
                                (+ (length-multiplier-coeff (- use-length len) :tail)
                                   (* score-mod (- use-length len)))
                                0))))

    (multiple-value-bind (split score-mod-split) (get-split reading conj-of)
      (when split
        (setf score
              (+ score-mod-split
                 (loop with nparts = (length split)
                    for part in split
                    for cnt from 1
                    for last = (= cnt nparts)
                    summing (calc-score part
                                        :final (and final last)
                                        :use-length (when (and last use-length)
                                                      (+ (mora-length (text part))
                                                         (- use-length len)))
                                        :score-mod (if last score-mod 0)
                                        ))))))

    (let ((info (list :posi posi :seq-set (cons seq conj-of)
                      :conj conj-data
                      :common (and common-p common-of)
                      :score-info (list prop-score kanji-break)
                      :kpcl (list kanji-p primary-p common-p long-p))))
      (when kanji-break (setf score (kanji-break-penalty kanji-break score :info info)))
      (values score info))))

(defun gen-score (segment &key final kanji-break)
  (setf (values (segment-score segment) (segment-info segment))
        (calc-score (segment-word segment) :final final :kanji-break kanji-break))
  segment)

(defun find-sticky-positions (str)
  "words cannot start or end after sokuon and before yoon characters"
  (loop with modifiers = (append *modifier-characters* *iteration-characters*)
       and str-len = (length str)
     for pos from 0 below str-len
     for char = (char str pos)
     for char-class = (gethash char *char-class-hash* char)
     if (and (eql char-class :sokuon) 
             (not (= pos (1- str-len)))
             (let ((char (char str (1+ pos))))
               (member (gethash char *char-class-hash* char) *kana-characters*))) collect (1+ pos)
     else if (and (member char-class modifiers)
                  (not (and (= pos (1- str-len)) (eql char-class :long-vowel))))
                  collect pos))

(defun make-slice ()
  (make-array 0 :element-type 'character
              :displaced-to ""))

(defun subseq-slice (slice str start &optional (end (length str)))
  (assert (>= end start))
  (unless slice (setf slice (make-slice)))
  (adjust-array slice (- end start)
                :displaced-to str
                :displaced-index-offset start))

(defparameter *identical-word-score-cutoff* 1/2)

(defun compare-common (c1 c2)
  (cond ((not c2) c1)
        ((= c2 0) (and c1 (> c1 0)))
        ((and c1 (> c1 0)) (< c1 c2))))

(defun cull-segments (segments)
  (when segments
    (let* ((segments (stable-sort segments #'compare-common
                                  :key (lambda (s) (getf (segment-info s) :common))))
           (segments (stable-sort segments #'> :key #'segment-score))
           (max-score (segment-score (car segments)))
           (cutoff (* max-score *identical-word-score-cutoff*)))
      (loop for seg in segments
           while (>= (segment-score seg) cutoff)
           collect seg))))

(defstruct segment-list segments start end (top nil))

(defgeneric get-segment-score (seg)
  (:documentation "Like segment-score but also works for segment-list and synergies")
  (:method ((seg segment))
    (segment-score seg))
  (:method ((seg-list segment-list))
    (let ((seg (car (segment-list-segments seg-list))))
      (if seg (segment-score seg) 0))))

;;; Those are only bound during join-substring-words calls
(defvar *suffix-map-temp* nil)
(defvar *suffix-next-end* nil)

(defun find-word-full (word)
  (let ((simple-words (find-word word)))
    (nconc simple-words
           (find-word-suffix word :unique (not simple-words)))))

(defparameter *score-cutoff* 5) ;; this must filter out ONLY bad kana spellings, and NOT filter out any kanji spellings

(defun join-substring-words* (str)
  (loop with sticky = (find-sticky-positions str)
        and kanji-break
        and slice = (make-slice)
       with suffix-map = (get-suffix-map str)
       for start from 0 below (length str)
       unless (member start sticky)
       nconcing 
       (loop for end from (1+ start) upto (length str)
            unless (member end sticky)
            nconcing
            (let* ((part (subseq-slice slice str start end))
                   (segments (mapcar
                              (lambda (word)
                                (make-segment :start start :end end :word word))
                              (let ((*suffix-map-temp* suffix-map)
                                    (*suffix-next-end* end))
                                (find-word-full part)))))
              (when segments
                (setf kanji-break (nconc (sequential-kanji-positions part start) kanji-break))
                (list (list start end segments)))))
       into result
     finally (return (values result kanji-break))))

(defun join-substring-words (str)
  (multiple-value-bind (result kanji-break) (join-substring-words* str)
    (loop
       with ends-with-lw = (alexandria:ends-with #\ー str)
       for (start end segments) in result
       for kb = (mapcar (lambda (n) (- n start)) (intersection (list start end) kanji-break))
       for sl = (loop for segment in segments
                   do (gen-score segment
                                 :final (or (= (segment-end segment) (length str))
                                            (and ends-with-lw
                                                 (= (segment-end segment) (1- (length str)))))
                                 :kanji-break kb)
                   if (>= (segment-score segment) *score-cutoff*)
                   collect segment)
       when sl
         collect (make-segment-list :segments (cull-segments sl) :start start :end end))))

(defstruct (top-array-item (:conc-name tai-)) score payload)

(defclass top-array ()
  ((array :reader top-array)
   (count :reader item-count :initform 0)
   ))

(defmethod initialize-instance :after ((obj top-array) &key (limit 5))
  (setf (slot-value obj 'array) (make-array limit :initial-element nil)))

(defgeneric register-item (collection score payload)
  (:method ((obj top-array) score payload)
    (with-slots (array count) obj
      (let ((item (make-top-array-item :score score :payload payload))
            (len (length array)))
        (loop for idx from (min count len) downto 0
           for prev-item = (when (> idx 0) (aref array (1- idx)))
           for done = (or (not prev-item) (>= (tai-score prev-item) score))
           when (< idx len) do (setf (aref array idx) (if done item prev-item))
           until done)
        (incf count)))))

(defgeneric get-array (collection)
  (:method ((obj top-array))
    (with-slots (array count) obj
      (if (>= count (length array)) array (subseq array 0 count)))))

(defparameter *gap-penalty* -500)

(declaim (inline gap-penalty))
(defun gap-penalty (start end)
  (* (- end start) *gap-penalty*))

(defun get-seg-initial (seg)
  (loop for split in (apply-segfilters nil seg)
     collect (cadr split)))

(defun get-seg-splits (seg-left seg-right)
  (let ((splits (apply-segfilters seg-left seg-right)))
    (loop for (seg-left seg-right) in splits
         nconcing (cons (get-penalties seg-left seg-right) (get-synergies seg-left seg-right)))))

(defun find-best-path (segment-lists str-length &key (limit 5))
  "generalized version of old find-best-path that operates on segment-lists and uses synergies"
  (let ((top (make-instance 'top-array :limit limit)))
    (register-item top (gap-penalty 0 str-length) nil)

    (dolist (segment-list segment-lists)
      (setf (segment-list-top segment-list) (make-instance 'top-array :limit limit)))

    ;;assume segments are sorted by (start, end) (as is the result of join-substring-words)
    (loop for (seg1 . rest) on segment-lists
       do
         (let ((gap-left (gap-penalty 0 (segment-list-start seg1)))
               (gap-right (gap-penalty (segment-list-end seg1) str-length)))
           (let ((initial-segs (get-seg-initial seg1)))
             (loop for seg in initial-segs
                for score1 = (get-segment-score seg)
                do
                  (register-item (segment-list-top seg1) (+ gap-left score1) (list seg))
                  (register-item top (+ gap-left score1 gap-right) (list seg)))))
         (loop for seg2 in rest
            for score2 = (get-segment-score seg2)
            when (>= (segment-list-start seg2) (segment-list-end seg1)) do
              (loop with gap-left = (gap-penalty (segment-list-end seg1) (segment-list-start seg2))
                   and gap-right = (gap-penalty (segment-list-end seg2) str-length)
                   for tai across (get-array (segment-list-top seg1))
                   for (seg-left . tail) = (tai-payload tai)
                   for score3 = (get-segment-score seg-left)
                   for score-tail = (- (tai-score tai) score3)
                   do (loop for split in (get-seg-splits seg-left seg2)
                           for accum = (+ gap-left
                                          (max (reduce #'+ split :key #'get-segment-score)
                                               (1+ score3)
                                               (1+ score2))
                                          score-tail)
                           for path = (nconc split tail)
                           do (register-item (segment-list-top seg2) accum path)
                              (register-item top (+ accum gap-right) path)))))

    (dolist (segment segment-lists)
      (setf (segment-list-top segment) nil))

    (loop for tai across (get-array top)
         collect (cons (reverse (tai-payload tai)) (tai-score tai)))))

;; (defun find-best-path* (segment-lists &key (limit 5))
;;   "convert find-best-path results to single word format"
;;   (let ((result (find-best-path segment-lists :limit limit)))
;;     (dolist (item result result)
;;       (setf (car item)
;;             (mapcan (lambda (obj)
;;                       (typecase obj
;;                         (segment-list (list (car (segment-list-segments obj))))))
;;                     (car item))))))

(defclass word-info ()
  ((type :initarg :type :accessor word-info-type)
   (text :initarg :text :accessor word-info-text)
   (true-text :initarg :true-text :initform nil :accessor word-info-true-text)
   (kana :initarg :kana :accessor word-info-kana)
   (seq :initarg :seq :initform nil :accessor word-info-seq)
   (conjugations :initarg :conjugations :initform nil :accessor word-info-conjugations)
   (score :initarg :score :initform 0 :accessor word-info-score)
   (components :initarg :components :initform nil :accessor word-info-components)
   (alternative :initarg :alternative :initform nil :accessor word-info-alternative)
   (primary :initarg :primary :initform t :accessor word-info-primary)
   (start :initarg :start :initform nil :accessor word-info-start)
   (end :initarg :end :initform nil :accessor word-info-end)
   ))

(defun word-info-json (word-info)
  (with-slots (type text true-text kana seq conjugations score components alternative primary start end)
      word-info
    (jsown:new-js
      ("type" (symbol-name type))
      ("text" text)
      ("truetext" true-text)
      ("kana" kana)
      ("seq" seq)
      ("conjugations" (if (eql conjugations :root) "ROOT" conjugations))
      ("score" score)
      ("components" (mapcar #'word-info-json components))
      ("alternative" alternative)
      ("primary" primary)
      ("start" start)
      ("end" end))))

(defun simple-word-info (seq text reading type &key (as :object))
  (let ((obj (make-instance 'word-info :type type :text text :true-text text :seq seq :kana reading)))
    (cond ((eql as :object)
           obj)
          ((eql as :json)
           (word-info-json obj)))))

;; define appropriate defmethods so that word-info-str and
;; word-info-gloss-json work both on CLOS objects and jsown objects

(defmacro def-reader-for-json (name slot)
  (alexandria:with-gensyms (obj)
    `(defmethod ,name ((,obj cons))
       (jsown:val ,obj ,slot))))

;;(def-reader-for-json word-info-type "type")
(defmethod word-info-type ((obj cons))
  (let ((val (jsown:val obj "type")))
    (cond ((equal val "KANJI") :kanji)
          ((equal val "KANA") :kana)
          (t :gap))))

(defmethod word-info-conjugations ((obj cons))
  (let ((val (jsown:val obj "conjugations")))
    (cond ((equal val "ROOT") :root)
          (t val))))

(def-reader-for-json word-info-text "text")
(def-reader-for-json word-info-true-text "truetext")
(def-reader-for-json word-info-kana "kana")
(def-reader-for-json word-info-seq "seq")
(def-reader-for-json word-info-score "score")
(def-reader-for-json word-info-components "components")
(def-reader-for-json word-info-alternative "alternative")
(def-reader-for-json word-info-primary "primary")
(def-reader-for-json word-info-start "start")
(def-reader-for-json word-info-end "end")

(defmethod print-object ((obj word-info) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "~a ~a[~a] score=~a"
            (word-info-seq obj) (word-info-text obj) (word-info-kana obj) (word-info-score obj))))

(defun word-info-from-segment (segment &aux (word (segment-word segment)))
  (make-instance 'word-info
                 :type (word-type word)
                 :text (get-text word)
                 :kana (get-kana word)
                 :seq (seq word)
                 :conjugations (when (typep word 'simple-text) (word-conjugations word))
                 :true-text (when (typep word 'simple-text) (true-text word))
                 :components (when (typep word 'compound-text)
                               (loop with primary-id = (id (primary word)) 
                                  for wrd in (words word)
                                  collect (make-instance 'word-info 
                                                         :type (word-type wrd)
                                                         :text (get-text wrd)
                                                         :true-text (true-text wrd)
                                                         :kana (get-kana wrd)
                                                         :seq (seq wrd)
                                                         :conjugations (word-conjugations wrd)
                                                         :primary (= (id wrd) primary-id))))
                 :score (segment-score segment)
                 :start (segment-start segment)
                 :end (segment-end segment)))

(defparameter *segment-score-cutoff* 2/3)

(defun word-info-from-segment-list (segment-list)
  (let* ((segments (segment-list-segments segment-list))
         (wi-list* (mapcar #'word-info-from-segment segments))
         (wi1 (car wi-list*))
         (max-score (word-info-score wi1))
         (wi-list (remove-if (lambda (wi)
                               (< (word-info-score wi)
                                  (* *segment-score-cutoff* max-score)))
                             wi-list*)))
    (if (= (length wi-list) 1)
        wi1
        (loop for wi in wi-list
           collect (word-info-kana wi) into kana-list
           collect (word-info-seq wi) into seq-list
           finally (return (make-instance 'word-info
                                          :type (word-info-type wi1)
                                          :text (word-info-text wi1)
                                          :kana (remove-duplicates kana-list :test 'equal)
                                          :seq seq-list
                                          :components wi-list
                                          :alternative t
                                          :score (word-info-score wi1)
                                          :start (segment-list-start segment-list)
                                          :end (segment-list-end segment-list)
                                          ))))))

(defun word-info-from-text (text)
  (with-connection *connection*
    (let* ((readings (find-word-full text))
           (segments (loop for r in readings collect (gen-score (make-segment :start 0 :end (length text) :word r))))
           (segment-list (make-segment-list :segments segments :start 0 :end (length text))))
      (word-info-from-segment-list segment-list))))

(defun fill-segment-path (str path)
  (flet ((make-substr-gap (start end)
           (let ((substr (subseq str start end)))
             (make-instance 'word-info
                            :type :gap :text substr :kana substr
                            :start start :end end))))
    (loop with idx = 0 and result
       for segment-list in path
       when (typep segment-list 'segment-list)
       if (> (segment-list-start segment-list) idx)
         do (push (make-substr-gap idx (segment-list-start segment-list)) result)
         end
       and do (push (word-info-from-segment-list segment-list) result)
              (setf idx (segment-list-end segment-list))
       finally
         (when (< idx (length str))
           (push (make-substr-gap idx (length str)) result))
         (return (process-word-info (nreverse result))))))

(defun word-info-rec-find (wi-list test-fn)
  "Find a word satisfying test-fn and the one after it"
  (loop for (wi wi-next) on wi-list
     for components = (word-info-components wi)
     if (funcall test-fn wi) nconc (list (cons wi wi-next))
     nconc (loop for (wf . wf-next) in (word-info-rec-find components test-fn)
              collect (cons wf (or wf-next wi-next)))))
       
(defun process-word-info (wi-list)
  "Process readings such as nani/nan (hardcoded so far)"
  (loop for (wi wi-next) on wi-list
       when (and wi-next (equal (word-info-text wi) "何")) do
         (let ((kn (word-info-kana wi-next)))
            (unless (listp kn) (setf kn (list kn)))
            (loop with nani = nil and nan = nil
               for kana in kn
               for first-char = (when (> (length kana) 0) (char kana 0))
               for fc-class = (gethash first-char *char-class-hash* first-char)
               when first-char
               if (member fc-class '(:ba :bi :bu :be :bo
                                     :pa :pi :pu :pe :po
                                     :da :dji :dzu :de :do
                                     :za :ji :zu :ze :zo
                                     :ta :chi :tsu :te :to
                                     :na :nu :ne :no
                                     :ra :ri :ru :re :ro))
                 do (setf nan t)
               else
               do (setf nani t)
               finally (let ((nani-kana (cond ((and nan nani) "なに")
                                              (nan "なん")
                                              (nani "なに"))))
                         (when nani-kana (setf (word-info-kana wi) nani-kana))))))
  wi-list)

(defun word-info-reading (word-info)
  (let ((table (case (word-info-type word-info) (:kanji 'kanji-text) (:kana 'kana-text)))
        (true-text (word-info-true-text word-info)))
    (when (and table true-text)
      (car (select-dao table (:= 'text true-text))))))

(defun dict-segment (str &key (limit 5))
  (with-connection *connection*
    (loop for (path . score) in (find-best-path (join-substring-words str) (length str) :limit limit)
         collect (cons (fill-segment-path str path) score))))

(defun simple-segment (str &key (limit 5))
  (caar (dict-segment str :limit limit)))

(defun get-senses-raw (seq &aux (tags '("pos" "s_inf" "stagk" "stagr")))
  (let* ((glosses
          (query (:order-by
                  (:select 'sense.ord (:raw "string_agg(gloss.text, '; ' ORDER BY gloss.ord)")
                           :from 'sense :left-join 'gloss :on (:= 'gloss.sense-id 'sense.id)
                           :where (:= 'sense.seq seq)
                           :group-by 'sense.id)
                  'sense.ord)))
         (props
          (query (:order-by
                  (:select 'sense.ord 'sense-prop.tag 'sense-prop.text
                           :from 'sense 'sense-prop
                           :where (:and (:= 'sense.seq seq)
                                        (:= 'sense-prop.sense-id 'sense.id)
                                        (:in 'sense-prop.tag (:set tags))))
                  'sense.ord 'sense-prop.tag 'sense-prop.ord)))
         (sense-list (loop for (sord gloss) in glosses
                          collect (list :ord sord :gloss (if (eql gloss :null) "" gloss) :props nil))))
    (loop with cursord and curtag and curprop and bag
       for (sord tag text) in props
       if (or (not (eql sord cursord)) (not (equal tag curtag)))
       do (when curprop (push (cons curtag (reverse bag)) (getf curprop :props)))
         (setf cursord sord curtag tag bag nil
               curprop (find sord sense-list :key 'cadr))
       do (push text bag)
       finally (when curprop (push (cons curtag bag) (getf curprop :props))))
    sense-list))

(defun get-senses (seq)
  (loop for sense in (get-senses-raw seq)
       for props = (getf sense :props)
       for gloss = (getf sense :gloss)
       for pos = (cdr (assoc "pos" props :test 'equal))
       for pos-str = (format nil "[~{~a~^,~}]" pos)
       collect (list pos-str gloss props)))

(defun get-senses-str (seq)
  (with-output-to-string (s)
    (loop for (pos gloss props) in (get-senses seq)
          for i from 1
          for rpos = pos then (if (equal pos "[]") rpos pos)
          for inf = (cdr (assoc "s_inf" props :test 'equal))
          for rinf = (when inf (format nil "~{~a~^; ~}" inf))
          when (> i 1) do (terpri s)
          do (format s "~a. ~a ~@[《~a》 ~]~a" i rpos rinf gloss))))


(defun match-kana-kanji (kana-reading kanji-reading restricted)
  (cond ((nokanji kana-reading) nil)
        (t (let* ((kana-text (text kana-reading))
                  (restr (loop for (rt kt) in restricted when (equal kana-text rt) collect kt)))
             (if restr
                 (find (text kanji-reading) restr :test 'equal)
                 t)))))

(defun match-sense-restrictions (seq props reading)
  (let ((stagk (cdr (assoc "stagk" props :test 'equal)))
        (stagr (cdr (assoc "stagr" props :test 'equal)))
        (wtype (word-type reading)))
    (cond ((and (not stagk) (not stagr)) t)
          ((or (member (text reading) stagk :test 'equal)
               (member (text reading) stagr :test 'equal)) t)
          ((and (not stagr) (eql wtype :kanji)) nil)
          ((and (not stagk) (eql wtype :kana)) nil)
          (t (let ((restricted (query (:select 'reading 'text :from 'restricted-readings :where (:= 'seq seq)))))
               (case wtype
                 (:kanji
                  (let ((rkana (select-dao 'kana-text (:and (:= 'seq seq) (:in 'text (:set stagr))))))
                    (some (lambda (rk) (match-kana-kanji rk reading restricted)) rkana)))
                 (:kana
                  (let ((rkanji (select-dao 'kanji-text (:and (:= 'seq seq) (:in 'text (:set stagk))))))
                    (some (lambda (rk) (match-kana-kanji reading rk restricted)) rkanji)))))))))


(defun split-pos (pos-str)
  (split-sequence #\, pos-str :start 1 :end (1- (length pos-str))))

(defun get-senses-json (seq &key pos-list reading reading-getter)
  (loop with readp
     for (pos gloss props) in (get-senses seq)
     for emptypos = (equal pos "[]")
     for rpos = pos then (if emptypos rpos pos)
     for lpos = (split-pos pos) then (if emptypos lpos (split-pos pos))
     for inf = (cdr (assoc "s_inf" props :test 'equal))
     for rinf = (when inf (format nil "~{~a~^; ~}" inf))
     when (and (or (not pos-list) (intersection lpos pos-list :test 'equal))
               (or (not (or reading-getter reading))
                   (not (or (assoc "stagk" props :test 'equal)
                            (assoc "stagr" props :test 'equal)))
                   (let ((rr (or reading
                                 (and (not readp)
                                      (setf readp t
                                            reading (funcall reading-getter))))))
                     (if rr (match-sense-restrictions seq props rr) t))))
     collect (let ((js (jsown:new-js ("pos" rpos) ("gloss" gloss))))
               (if rinf (jsown:extend-js js ("info" rinf)))
               js)))

(defun short-sense-str (seq &key with-pos)
  (query 
   (sql-compile
    `(:limit 
      (:order-by
       (:select (:select (:raw "string_agg(gloss.text, '; ' ORDER BY gloss.ord)")
                         :from gloss :where (:= gloss.sense-id sense.id))
                :from sense 
                ,@(if with-pos
                      `(:inner-join (:as sense-prop pos) :on (:and (:= pos.sense-id sense.id)
                                                                   (:= pos.tag "pos")
                                                                   (:= pos.text ,with-pos))))
                :where (:= 'sense.seq ,seq)
                :group-by 'sense.id)
       'sense.ord)
      1)) :single))

(defun reading-str* (kanji kana)
  (if kanji
      (format nil "~a 【~a】" kanji kana)
      kana))

(defun reading-str-seq (seq)
  (let* ((kanji-text (car (query (:select 'text :from 'kanji-text :where (:and (:= 'seq seq) (:= 'ord 0))) :column)))
         (kana-text (car (query (:select 'text :from 'kana-text :where (:and (:= 'seq seq) (:= 'ord 0))) :column))))
    (reading-str* kanji-text kana-text)))

(defgeneric reading-str (obj)
  (:method ((obj simple-text))
    (reading-str* (get-kanji obj) (get-kana obj)))
  (:method ((obj integer))
    (reading-str-seq obj)))

(defun entry-info-short (seq &key with-pos)
  (let ((sense-str (short-sense-str seq :with-pos with-pos)))
    (with-output-to-string (s)
      (format s "~a : " (reading-str-seq seq))
      (when sense-str (princ sense-str s)))))

(defun select-conjs (seq &optional conj-ids)
  (if conj-ids
      (unless (eql conj-ids :root)
        (select-dao 'conjugation (:and (:= 'seq seq) (:in 'id (:set conj-ids)))))
      (or
       (select-dao 'conjugation (:and (:= 'seq seq) (:is-null 'via)))
       (select-dao 'conjugation (:= 'seq seq)))))

(defun print-conj-info (seq &key conjugations (out *standard-output*))
  (loop with via-used = nil
     for conj in (select-conjs seq conjugations)
     for via = (seq-via conj)
     unless (member via via-used)
     do (loop for conj-prop in (select-dao 'conj-prop (:= 'conj-id (id conj)))
             for first = t then nil
           do (format out "~%~:[ ~;[~] Conjugation: ~a" first (conj-info-short conj-prop)))
       (if (eql via :null)
           (format out "~%  ~a" (entry-info-short (seq-from conj)))
           (progn
             (format out "~% --(via)--")
             (print-conj-info via :out out)
             (push via via-used)))
       (princ " ]" out)))

(defun conj-info-json (seq &key conjugations text has-gloss)
  (loop with via-used = nil
     for conj in (select-conjs seq conjugations)
     for via = (seq-via conj)
     unless (member via via-used)
     nconc (block outer
             (let* ((conj-pos nil)
                    (orig-text (get-original-text-once (get-conj-data seq (list (id conj))) text))
                    (js (jsown:new-js 
                          ("prop" (loop for conj-prop in (select-dao 'conj-prop (:= 'conj-id (id conj)))
                                     do (push (pos conj-prop) conj-pos)
                                     collect (conj-prop-json conj-prop))))))
               (if (eql via :null)
                   (let ((orig-reading (when orig-text
                                         ;; TODO: allow multiple readings
                                         (car (find-words-seqs orig-text (seq-from conj))))))
                     (when (and has-gloss (not orig-reading))
                       (return-from outer nil))
                     (jsown:extend-js js
                       ("reading" (reading-str (or orig-reading (seq-from conj))))
                       ("gloss" (get-senses-json (seq-from conj)
                                                 :pos-list conj-pos
                                                 :reading-getter (lambda () orig-reading)))))
                   (progn
                     (jsown:extend-js js
                       ("via" (conj-info-json via :text orig-text :has-gloss has-gloss)))
                     (push via via-used)))
               (list js)))))

(defun map-word-info-kana (fn word-info &key (separator "/")
                           &aux (wkana (word-info-kana word-info)))
  (if (listp wkana)
      (with-output-to-string (s)
        (loop for wk in wkana
             for first = t then nil
             unless first do (princ separator s)
             do (princ (funcall fn wk) s)))
      (funcall fn wkana)))

(defun word-info-reading-str (word-info)
  (reading-str* (case (word-info-type word-info)
                 (:kanji (word-info-text word-info))
                 (t nil))
               (word-info-kana word-info)))

(defmethod reading-str ((word-info word-info))
  (word-info-reading-str word-info))

(defmethod reading-str ((word-info list))
  (word-info-reading-str word-info))
  
(defun word-info-str (word-info)
  (with-connection *connection*
    (with-output-to-string (s)
      (labels ((inner (word-info &optional suffix marker)
                 (when marker (princ " * " s))
                 (princ (reading-str word-info) s)
                 (if (word-info-components word-info)
                     (progn
                       (format s " Compound word: ~{~a~^ + ~}" (mapcar #'word-info-text (word-info-components word-info)))
                       (dolist (comp (word-info-components word-info))
                         (terpri s)
                         (inner comp (not (word-info-primary comp)) t)))
                     (let ((seq (word-info-seq word-info)) 
                           (conjs (word-info-conjugations word-info))
                           desc)
                       (cond ((and suffix (setf desc (get-suffix-description seq)))
                              (format s "  [suffix]: ~a " desc))
                             ((or (not conjs) (eql conjs :root))
                              (terpri s) (princ (if seq (get-senses-str seq) "???") s)))
                       (when seq
                         (print-conj-info seq :out s
                                          :conjugations conjs))))))
        (if (word-info-alternative word-info)
            (loop for wi in (word-info-components word-info)
                 for i from 1
                 when (> i 1) do (terpri s)
                 do (format s "<~a>. " i) (inner wi nil nil))
            (inner word-info))))))

(defun word-info-gloss-json (word-info &key root-only)
  (with-connection *connection*
    (labels ((inner (word-info &optional suffix)
               (let ((js (jsown:new-js ("reading" (reading-str word-info))
                                       ("text" (word-info-text word-info))
                                       ("kana" (word-info-kana word-info))
                                       )))
                 (when (word-info-score word-info)
                   (jsown:extend-js js ("score" (word-info-score word-info))))
                 (if (word-info-components word-info)
                     (jsown:extend-js js
                       ("compound" (mapcar #'word-info-text (word-info-components word-info)))
                       ("components" (loop for wi in (word-info-components word-info)
                                        collect (inner wi (not (word-info-primary wi))))))
                     (let ((seq (word-info-seq word-info))
                           (conjs (word-info-conjugations word-info))
                           (reading-getter (lambda () (word-info-reading word-info)))
                           desc has-gloss)
                       (cond (root-only
                              (return-from inner
                                (jsown:extend-js js ("gloss" (get-senses-json seq :reading-getter reading-getter)))))
                             ((and suffix (setf desc (get-suffix-description seq)))
                              (jsown:extend-js js ("suffix" desc)))
                             ((and seq (or (not conjs) (eql conjs :root)))
                              (let ((gloss (get-senses-json seq :reading-getter reading-getter)))
                                (when gloss
                                  (setf has-gloss t)
                                  (jsown:extend-js js ("gloss" gloss))))))
                       (when seq
                         (jsown:extend-js js 
                           ("conj" (conj-info-json seq :conjugations (word-info-conjugations word-info)
                                                   :text (word-info-true-text word-info)
                                                   :has-gloss has-gloss))))))
                 js)))
      (if (word-info-alternative word-info)
          (jsown:new-js ("alternative" (mapcar #'inner (word-info-components word-info))))
          (inner word-info)))))

(defun get-kanji-words (char)
  (with-connection *connection*
    (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char)))
      (query (:select 'e.seq 'k.text 'r.text 'k.common
                      :from (:as 'entry 'e) (:as 'kanji-text 'k) (:as 'kana-text 'r)
                      :where (:and (:= 'e.seq 'k.seq)
                                   (:= 'e.seq 'r.seq)
                                   (:= 'r.text 'k.best-kana)
                                   (:not-null 'k.common)
                                   'e.root-p
                                   (:like 'k.text (:|| "%" str "%"))))))))

(defun exists-reading (seq reading)
  (query (:select 'seq :from 'kana-text :where (:and (:= 'seq seq) (:= 'text reading)))))
         
(defun find-word-info (text &key reading root-only &aux (end (length text)))
  (with-connection *connection*
    (let* ((*suffix-map-temp* (get-suffix-map text))
           (*suffix-next-end* end)
           (all-words (if root-only
                          (find-word text :root-only t)
                          (find-word-full text)))
           (segments (loop for word in all-words
                        collect (gen-score (make-segment :start 0 :end end :word word))))
           (segments (sort segments #'> :key #'segment-score))
           (wis (mapcar #'word-info-from-segment segments)))
      (when reading
        (setf wis
              (loop for wi in wis
                 for seq = (word-info-seq wi)
                 if (equal (word-info-kana wi) reading)
                 collect wi
                 else if (and seq (exists-reading seq reading))
                 do (setf (word-info-kana wi) reading)
                 and collect wi)))
      wis)))

(defun find-word-info-json (text &key reading root-only)
  (mapcar (lambda (wi) (word-info-gloss-json wi :root-only root-only))
          (find-word-info text :reading reading :root-only root-only)))

