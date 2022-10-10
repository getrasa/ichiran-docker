(in-package #:ichiran/conn)

(defparameter *connection* '("jmdict0122" "ichiran" "ichiran" "localhost"))

(defparameter *connections* '((:old "jmdict_old" "postgres" "password" "localhost")
                              (:test "jmdict_test" "postgres" "password" "localhost")))

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"~/dump/JMdict_e")

(defparameter *jmdict-data* #p"/jmdictdb/jmdictdb/data/")

(in-package #:ichiran/kanji)

(defparameter *kanjidic-path* #P"~/dump/kanjidic2.xml")
