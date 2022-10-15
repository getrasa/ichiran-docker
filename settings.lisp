(in-package #:ichiran/conn)

(defparameter *connection* '("jmdict0122" "postgres" "postgres" "localhost"))

(defparameter *connections* '((:old "jmdict_old" "postgres" "password" "localhost")
                              (:test "jmdict_test" "postgres" "password" "localhost")))

(in-package #:ichiran/dict)

(defparameter *jmdict-path* #p"/home/ubuntu/dump/JMdict_e")

(defparameter *jmdict-data* #p"/home/ubuntu/jmdictdb/jmdictdb/data/")

(in-package #:ichiran/kanji)

(defparameter *kanjidic-path* #P"/home/ubuntu/dump/kanjidic2.xml")
