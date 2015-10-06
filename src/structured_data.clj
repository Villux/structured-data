(ns structured-data)


(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


(defn spiff [v]
  (+ (get v 0) (get v 2)))



(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))


(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- x1 y1) (- x2 y2))))


(defn area [rectangle]
  (let [x (width rectangle)
        y (height rectangle)]
    (* x y)
  ))



(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point ]
    (and (<= x1 xp x2) (<= y1 yp y2))))




(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1) (contains-point? outer point2) )
  ))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (cond
   (< 1 (author-count book)) true
   :else false))

(defn add-author [book new-author]
  (let [original-authors (:authors book)
        new-author-list (conj original-authors new-author)
        new (assoc book :authors new-author-list)]
    new))


(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [get-second (fn [item] (get item 1))]
    (map get-second collection)))


(defn titles [books]
  (map :title books))


;; apply changes (fn [arg1 arg2 ...]) -> (fn arg1 arg2 ...)
(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))



(defn stars [n]
  (apply str "" (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))



(defn contains-duplicates? [a-seq]
  (if (= a-seq (seq (set a-seq)))
    false
    true ))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str " (" (:birth-year author) " - " (:death-year author)")")]
    (str name (if (:birth-year author) years))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (cond
   (= (count books) 0) "No books."
   (= (count books) 1) (str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
   :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))


(defn books-by-author [author books]
  (filter #(has-author? % author) books))


(defn author-by-name [name authors]
  (first (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
