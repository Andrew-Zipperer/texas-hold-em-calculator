;; : preflop two hands --> probability hand1 wins
;;   computes all possible boards
;;   for each board:
;;     compute best hand from hand1 and board
;;     compute best hand from hand2 and board
;;     compare hands

;; compute best hand from seven cards
;; @todo need to construct best hand from seven cards

;; compare hand

;; could walk through this process with joe, hudson, will, skyler
;; improvement: based on what I learn from first draft: write types. write specifications. aim for self-documentation
;;  'suppose you have notion of card'
;;  'card has suit, card has rank'
;;  'hand type' 
;;  'hand type rank' / comparing with same hand type rank
;; it's a nice idea to try to make it clear and present to them


;; separate into files, by data type and accessors
;;   then files for different parts like -- better than within type

;; have:
;; - example hands
;; - compute-hand-type
;; - hand-type-better?
;; - hand better within hand type -- with some tests. see notes next to -equal* -lower* variants for more test coverage.



;; next: generate all boards and save to file?
;;       increase test coverage for existing functions
;;       function to filter list of all boards to list possible given the cards that we see in hand-a and hand-b
;;       given a board, find best hand from hand1 and board

;; later: simplify existing functions
;;        abstract and reuse where possible



;; once I have that, then I can
;; - generate five card boards -- can do unordered boards if assuming going all the way to showdown
;; - make best hand from seven cards
;;
;; if generating all possible boards is slow, then i can start with boards I make.
;; also, I can generate all possible boards and store them in a file.
;; then 'generating the possible boards' given two hands is:
;;  get the boards from the file
;;  remove the boards that contain cards from the two hands


;; what happens with more than two hands?
;;  - calculate overall winner for each board
;;  -- i.e. calculate each player's best hand then find best among them
;;  --  

(defparameter *ordered-list-of-hand-types*
  '(:royal-flush :straight-flush :four-of-a-kind :full-house
    :flush :straight :three-of-a-kind :two-pair :one-pair :high-card))

(defun ordered-list-of-hand-types ()
  *ordered-list-of-hand-types*)

(defun hand-type-position (hand-type)
  (position hand-type (ordered-list-of-hand-types)))

(defun hand-type-better? (hand-a hand-b)
  (< (hand-type-position hand-a)
     (hand-type-position hand-b)))

(defun hand-type-equal? (hand-a hand-b)
  (= (hand-type-position hand-a)
     (hand-type-position hand-b)))

(defun hand-type-worse? (hand-a hand-b)
  (and (not (hand-type-better? hand-a hand-b))
       (not (hand-type-equal? hand-a hand-b))))

;; need better within type too, e.g. pair of aces beats pair of kings. pair of aces with 10 high beats pair of aces 8 high.

;; @todo that is, need hand-better? where it considers hand type and the rank within type

(defun hand-better-worse-or-draw (hand-a hand-b)
  (let ((hand-a-hand-type (compute-hand-type hand-a))
	(hand-b-hand-type (compute-hand-type hand-b)))
    (cond
      ((hand-type-better? hand-a-hand-type hand-b-hand-type)
       :better)
      ((hand-type-better? hand-b-hand-type hand-a-hand-type)
       :worse)
      ((and (hand-type-equal? hand-a-hand-type hand-b-hand-type)
	    (hand-better-within-hand-type? hand-a hand-b hand-a-hand-type))
       :better)
      ((hand-better-within-hand-type? hand-b hand-a hand-a-hand-type)
       :worse)
      #+ignoreForNow
      ((hand-equal-within-hand-type? hand-a hand-b hand-a-hand-type) ;; do I even need this? or I can I conclude :draw?
       :draw)
      (t
       :draw)
      #+ignore
      (t
       (error "should we ever reach this case? right now I do not think so")))))

#+ignore
(defun hand-better-within-hand-type? (hand-a hand-b hand-type)
  (let* ((hand-a-hand-type-rank (hand-type-rank hand-a hand-type))
	 (hand-b-hand-type-rank (hand-type-rank hand-b hand-type)))
  t))

(defun hand-better-within-hand-type? (hand-a hand-b hand-type)
  (case hand-type
    (:royal-flush
     nil)
    (:straight-flush
     (hand-better-within-hand-type-straight-flush? hand-a hand-b))
    (:four-of-a-kind
     (hand-better-within-hand-type-four-of-a-kind? hand-a hand-b))
    (:full-house
     (hand-better-within-hand-type-full-house? hand-a hand-b))
    (:flush
     (hand-better-within-hand-type-flush? hand-a hand-b))
    (:straight
     (hand-better-within-hand-type-straight? hand-a hand-b))
    (:three-of-a-kind
     (hand-better-within-hand-type-three-of-a-kind? hand-a hand-b))
    (:two-pair
     (hand-better-within-hand-type-two-pair? hand-a hand-b))
    (:one-pair
     (hand-better-within-hand-type-one-pair? hand-a hand-b))
    (:high-card
     (hand-better-within-hand-type-high-card? hand-a hand-b))))

(defun hand-highest-card-rank (hand)
  (let* ((card-ranks (mapcar 'card-rank hand))
	 (card-rank-positions (mapcar 'card-rank-position card-ranks))
	 (card-rank-positions-highest-card-rank-position (apply #'min card-rank-positions))
	 (card-rank-positions-highest-card-rank (position-card-rank card-rank-positions-highest-card-rank-position)))
    card-rank-positions-highest-card-rank))

(defun hand-highest-card-rank-straight (hand)
  "get highest card in straight. when it is ace to five straight, set highest card to five"
  (let* ((hand-highest-card-rank (hand-highest-card-rank hand))
	 (hand-highest-card-rank-straight hand-highest-card-rank))
    (when (and (rank-ace? hand-highest-card-rank)
	       (hand-ace-to-five-straight? hand))
      (setf hand-highest-card-rank-straight :five))
    hand-highest-card-rank-straight))	 

(defun hand-ace-to-five-straight? (hand)
  (let* ((hand-ranks-descending (hand-ranks-descending hand))
	 (hand-ace-to-five-straight? (equal (ace-to-five-straight-ranks-descending)
					   hand-ranks-descending)))
    hand-ace-to-five-straight?))

;; @todo add test cases for these   
(defun hand-better-within-hand-type-straight-flush? (hand-a hand-b)
  (hand-better-within-hand-type-straight? hand-a hand-b))

(defun hand-better-within-hand-type-four-of-a-kind? (hand-a hand-b)
  "assumes hand-a and hand-b are four of a kinds.
   finds the card rank in which each has four of a kind. 
   compares them. when they are the same rank, then compares kickers"
  (multiple-value-bind (hand-a-four-of-a-kind-rank hand-a-four-of-a-kind-kicker)
      (hand-four-of-a-kind-rank-and-kicker hand-a)
    (multiple-value-bind (hand-b-four-of-a-kind-rank hand-b-four-of-a-kind-kicker)
	(hand-four-of-a-kind-rank-and-kicker hand-b)
      (let ((hand-a-four-of-a-kind-better? nil))
	(cond
	  ((card-rank-better? hand-a-four-of-a-kind-rank hand-b-four-of-a-kind-rank)
	   (setf hand-a-four-of-a-kind-better? t))
	  ((and (card-rank-equal? hand-a-four-of-a-kind-rank hand-b-four-of-a-kind-rank)
		(card-rank-better? hand-a-four-of-a-kind-kicker
				   hand-b-four-of-a-kind-kicker))
	   (setf hand-a-four-of-a-kind-better? t)))
        hand-a-four-of-a-kind-better?))))

(defun hand-n-of-a-kind-rank (n hand)
  (let ((rank-and-occurrence-alist '())
	(threshold-for-n-of-a-kind (- 5 n)))
    (dolist (card hand)
      (let* ((card-rank (card-rank card))
	     (occurrences-for-rank (getf rank-and-occurrence-alist card-rank)))
	(cond
	  ((and occurrences-for-rank           
		(> occurrences-for-rank        ; hands have five cards, so any rank that occurs more than 
		   threshold-for-n-of-a-kind)) ;  the threshold is the one that repeats n times
	   (return-from hand-n-of-a-kind-rank card-rank)) 
	  (occurrences-for-rank
	   (setf (getf rank-and-occurrence-alist card-rank) (incf occurrences-for-rank)))
	  (t
	   (setf (getf rank-and-occurrence-alist card-rank) 1)))))))

(defun hand-four-of-a-kind-rank-and-kicker (hand)
  (let* ((hand-four-of-a-kind-rank (hand-four-of-a-kind-rank-alt hand))
	 (hand-ranks (hand-ranks hand))
	 (hand-ranks-without-four-of-a-kind-rank (remove hand-four-of-a-kind-rank hand-ranks))
	 (sole-remaining-rank (first hand-ranks-without-four-of-a-kind-rank))
	 (hand-four-of-a-kind-kicker sole-remaining-rank))
    (values hand-four-of-a-kind-rank
	    hand-four-of-a-kind-kicker)))

(defun hand-four-of-a-kind-rank (hand)
  (hand-n-of-a-kind-rank 4 hand))

(defun hand-three-of-a-kind-rank (hand)
  (hand-n-of-a-kind-rank 3 hand))

(defun hand-one-pair-rank (hand)
  (hand-n-of-a-kind-rank 4 hand)) ;; pass in 4 to make threshold 1, because once there is more than one occurrence
                                  ;;  we found the pair. but this confuses. probably rewrite.

;; notes for rewrite of this: there aren't that many cards, and I don't think it's that big a deal
;; to make the whole alist. so, I can make the alist of ranks and occurrences. then get the rank
;; that has {four|three|two|one} occurrences.

(defun hand-full-house-rank-with-three-of-a-kind (hand)
  (hand-n-of-a-kind-rank 3 hand))

(defun hand-full-house-rank-with-three-of-a-kind-alt (hand)
  (hand-three-of-a-kind-rank-alt hand))

(defun hand-three-of-a-kind-rank-alt (hand)
  (hand-n-of-a-kind-rank-alt 3 hand))

(defun hand-four-of-a-kind-rank-alt (hand)
  (hand-n-of-a-kind-rank-alt 4 hand))

(defun hand-n-of-a-kind-rank-alt (n hand)
  (let ((hand-rank-and-occurrences-alist (hand-rank-and-occurrences-alist hand))
	(hand-ranks (hand-ranks hand)))
    (dolist (hand-rank hand-ranks)
      (let ((number-of-occurrences-of-rank (getf hand-rank-and-occurrences-alist hand-rank)))
	(when (= n number-of-occurrences-of-rank)
	  (return-from hand-n-of-a-kind-rank-alt hand-rank))))))  

;; similar to hand-four-of-a-kind-rank-and-kicker
(defun hand-full-house-rank-with-three-of-a-kind-and-rank-with-two-of-a-kind (hand)
  (let* ((hand-full-house-rank-with-three-of-a-kind
	  (hand-full-house-rank-with-three-of-a-kind-alt hand))
	 (hand-ranks (hand-ranks hand))
	 (hand-ranks-without-rank-with-three-of-a-kind (remove hand-full-house-rank-with-three-of-a-kind
							       hand-ranks))
	 (two-instances-of-other-rank hand-ranks-without-rank-with-three-of-a-kind)
	 (hand-full-house-rank-with-two-of-a-kind-representative (first two-instances-of-other-rank))
	 (hand-full-house-rank-with-two-of-a-kind hand-full-house-rank-with-two-of-a-kind-representative))
    (values hand-full-house-rank-with-three-of-a-kind
	    hand-full-house-rank-with-two-of-a-kind)))

#+NoCallers
(defun hand-four-of-a-kind-kicker-better? (hand-a hand-b)
  hand-a
  hand-b
  nil)

(defun hand-better-within-hand-type-full-house? (hand-a hand-b)
  (multiple-value-bind (hand-a-rank-with-three-of-a-kind hand-a-rank-with-two-of-a-kind)
      (hand-full-house-rank-with-three-of-a-kind-and-rank-with-two-of-a-kind hand-a)
    (multiple-value-bind (hand-b-rank-with-three-of-a-kind hand-b-rank-with-two-of-a-kind)
	(hand-full-house-rank-with-three-of-a-kind-and-rank-with-two-of-a-kind hand-b)
      (let ((hand-a-full-house-better? nil))
	(cond
	  ((card-rank-better? hand-a-rank-with-three-of-a-kind hand-b-rank-with-three-of-a-kind)
	   (setf hand-a-full-house-better? t))
	  ((and (card-rank-equal? hand-a-rank-with-three-of-a-kind hand-b-rank-with-three-of-a-kind)
		(card-rank-better? hand-a-rank-with-two-of-a-kind hand-b-rank-with-two-of-a-kind))
	   (setf hand-a-full-house-better? t)))
	hand-a-full-house-better?))))

(defun hand-better-within-hand-type-flush? (hand-a hand-b)
  (hand-ordered-ranks-higher? hand-a hand-b))

;; the idea: (:ace :ten :eight :three :two) higher than (:ace :nine :eight :three :two)
(defun hand-ordered-ranks-higher? (hand-a hand-b)
  (let* ((hand-a-ranks-descending (hand-ranks-descending hand-a))
	 (hand-b-ranks-descending (hand-ranks-descending hand-b))
	 (index 0))
    ;; make dolist-indexed
    (dolist (hand-a-card-rank hand-a-ranks-descending)
      (let ((hand-b-card-rank (nth index hand-b-ranks-descending)))
	(cond
	  ((card-rank-better? hand-a-card-rank hand-b-card-rank)
	   (return-from hand-ordered-ranks-higher? t))
	  ((card-rank-better? hand-b-card-rank hand-a-card-rank)
	   (return-from hand-ordered-ranks-higher? nil)))
	(incf index)))
    nil))
	
(defun hand-better-within-hand-type-straight? (hand-a hand-b)
  "assumes hand-a and hand-b are straights. 
   gets high card. if high card is ace, then check for ace to 5 or 10 to ace.
   if ace to 5, then if other is ace, check whether ace to 5. if it is, then draw. so nil.
                                                              if not then worse.
   get high card
   when ace, determine whether ace to five or ten to ace, when ace to five, set high card to five
   ace-to-five ten-to-ace helpers
   compare high cards"
  (let* ((hand-a-highest-card-rank (hand-highest-card-rank-straight hand-a))
	 (hand-b-highest-card-rank (hand-highest-card-rank-straight hand-b))
	 (hand-better-within-hand-type-straight? (card-rank-better? hand-a-highest-card-rank
								    hand-b-highest-card-rank)))
    hand-better-within-hand-type-straight?))

(defun hand-better-within-hand-type-three-of-a-kind? (hand-a hand-b)
  (multiple-value-bind (hand-a-three-of-a-kind-rank hand-a-three-of-a-kind-higher-kicker hand-a-three-of-a-kind-lower-kicker)
      (hand-three-of-a-kind-rank-and-ordered-kickers hand-a)
    (multiple-value-bind (hand-b-three-of-a-kind-rank hand-b-three-of-a-kind-higher-kicker hand-b-three-of-a-kind-lower-kicker)
      (hand-three-of-a-kind-rank-and-ordered-kickers hand-b)
      (let ((hand-a-three-of-a-kind-better? nil))
	(cond
	  ((card-rank-better? hand-a-three-of-a-kind-rank hand-b-three-of-a-kind-rank)
	   (setf hand-a-three-of-a-kind-better? t))
	  ((card-rank-equal? hand-a-three-of-a-kind-rank hand-b-three-of-a-kind-rank)
	   (cond
	     ((card-rank-better? hand-a-three-of-a-kind-higher-kicker
				 hand-b-three-of-a-kind-higher-kicker)
	      (setf hand-a-three-of-a-kind-better? t))
	     ((and (card-rank-equal? hand-a-three-of-a-kind-higher-kicker
				     hand-b-three-of-a-kind-higher-kicker)
		   (card-rank-better? hand-a-three-of-a-kind-lower-kicker
				      hand-b-three-of-a-kind-lower-kicker))
	      (setf hand-a-three-of-a-kind-better? t)))))
        hand-a-three-of-a-kind-better?))))

;; similar to hand-four-of-a-kind-rank-and-kicker
(defun hand-three-of-a-kind-rank-and-ordered-kickers (hand)
  (let* ((hand-three-of-a-kind-rank
	  (hand-three-of-a-kind-rank-alt hand))
	 (hand-ranks-descending (hand-ranks-descending hand))
	 (hand-ranks-descending-without-rank-with-three-of-a-kind
	  (remove hand-three-of-a-kind-rank
		  hand-ranks-descending))
	 (higher-kicker (first hand-ranks-descending-without-rank-with-three-of-a-kind))
	 (lower-kicker  (second hand-ranks-descending-without-rank-with-three-of-a-kind)))
    (values hand-three-of-a-kind-rank
	    higher-kicker
	    lower-kicker)))

;; can I just incf it, or do i have to setf the getf?
(defun hand-rank-and-occurrences-alist (hand)
  (let* ((rank-and-occurrence-alist '()))
    (dolist (card hand)
      (let* ((card-rank (card-rank card))
	     (occurrences-for-rank (getf rank-and-occurrence-alist card-rank)))
	(cond
	  (occurrences-for-rank
	   (setf (getf rank-and-occurrence-alist card-rank) (incf occurrences-for-rank)))
	  (t
	   (setf (getf rank-and-occurrence-alist card-rank) 1)))))
    rank-and-occurrence-alist))

(defun alist-keys (alist)
  ;; assert-type alist alist
  ;; e.g. even number of elements
  (when (null alist)
    (return-from alist-keys nil))
  (let ((alist-keys '()))
    (do* ((alist-remaining alist (rest (rest alist-remaining)))
	  (key (first alist-remaining) (first alist-remaining))
;;	  (value (second alist-remaining) (second alist-remaining))
	  )
	 ((null alist-remaining) (reverse alist-keys))
      (push key alist-keys))))

(defparameter *example-alist* (list :ten 1 :eight 2 :seven 4))

(defun hand-two-pair-higher-pair-rank-lower-pair-rank-and-kicker (hand)
  (let* ((hand-two-pair-rank-and-occurrences-alist (hand-rank-and-occurrences-alist hand))
	 (hand-ranks-descending (hand-ranks-descending hand))
	 (hand-rank-highest (first hand-ranks-descending))
	 (hand-number-of-occurences-of-highest-rank (getf hand-two-pair-rank-and-occurrences-alist hand-rank-highest))
	 (hand-rank-second (second hand-ranks-descending))
	 (hand-number-of-occurrences-of-second-rank (getf hand-two-pair-rank-and-occurrences-alist hand-rank-second))
	 (hand-rank-third (third hand-ranks-descending))
	 hand-two-pair-higher-pair-rank
	 hand-two-pair-lower-pair-rank
	 hand-two-pair-kicker)
    (if (= 2 hand-number-of-occurences-of-highest-rank)
	(setf hand-two-pair-higher-pair-rank hand-rank-highest)
	(setf hand-two-pair-kicker hand-rank-highest))
    (cond 
      ((and hand-two-pair-higher-pair-rank
	    (= 2 hand-number-of-occurrences-of-second-rank))
	(setf hand-two-pair-lower-pair-rank hand-rank-second))
      (hand-two-pair-higher-pair-rank
       (setf hand-two-pair-kicker hand-rank-second))
      (t
       (setf hand-two-pair-higher-pair-rank hand-rank-second)))
    (if hand-two-pair-lower-pair-rank
	(setf hand-two-pair-kicker hand-rank-third)
	(setf hand-two-pair-lower-pair-rank hand-rank-third))
  (values hand-two-pair-higher-pair-rank
	  hand-two-pair-lower-pair-rank
	  hand-two-pair-kicker)))

(defun hand-better-within-hand-type-two-pair? (hand-a hand-b)
  (multiple-value-bind (hand-a-two-pair-higher-pair-rank hand-a-two-pair-lower-pair-rank hand-a-two-pair-kicker)
      (hand-two-pair-higher-pair-rank-lower-pair-rank-and-kicker hand-a)
    (multiple-value-bind (hand-b-two-pair-higher-pair-rank hand-b-two-pair-lower-pair-rank hand-b-two-pair-kicker)
	(hand-two-pair-higher-pair-rank-lower-pair-rank-and-kicker hand-b)
      (let ((hand-a-two-pair-better? nil))
	(cond
	  ((card-rank-better? hand-a-two-pair-higher-pair-rank hand-b-two-pair-higher-pair-rank)
	   (setf hand-a-two-pair-better? t))
	  ((and (card-rank-equal? hand-a-two-pair-higher-pair-rank hand-b-two-pair-higher-pair-rank)
		(card-rank-better? hand-a-two-pair-lower-pair-rank hand-b-two-pair-lower-pair-rank))
	   (setf hand-a-two-pair-better? t))
	  ((and (card-rank-equal? hand-a-two-pair-lower-pair-rank hand-b-two-pair-lower-pair-rank)
		(card-rank-better? hand-a-two-pair-kicker hand-b-two-pair-kicker))
	   (setf hand-a-two-pair-better? t)))
	hand-a-two-pair-better?))))
	
(defun hand-one-pair-and-ordered-kickers (hand)
  (let* ((hand-rank-and-occurrences-alist (hand-two-pair-rank-and-occurrences-alist hand))
	 (hand-ranks-descending (hand-ranks-descending hand))
	 hand-one-pair-rank
	 hand-one-pair-high-kicker
	 hand-one-pair-middle-kicker
	 hand-one-pair-low-kicker)
    (dolist (hand-rank hand-ranks-descending)
      (let ((number-occurrences-of-rank (getf hand-rank-and-occurrences-alist hand-rank)))
	(when (= 2 number-occurrences-of-rank)
	  (setf hand-one-pair-rank hand-rank))))
    (let ((hand-ranks-descending-without-rank-with-pair (remove hand-one-pair-rank hand-ranks-descending)))
      (setf hand-one-pair-high-kicker (first hand-ranks-descending-without-rank-with-pair))
      (setf hand-one-pair-middle-kicker (second hand-ranks-descending-without-rank-with-pair))
      (setf hand-one-pair-low-kicker (third hand-ranks-descending-without-rank-with-pair)))
    (values hand-one-pair-rank
	    hand-one-pair-high-kicker
	    hand-one-pair-middle-kicker
	    hand-one-pair-low-kicker)))
	
(defun hand-better-within-hand-type-one-pair? (hand-a hand-b)
  (multiple-value-bind (hand-a-one-pair-rank hand-a-one-pair-higher-kicker hand-a-one-pair-middle-kicker hand-a-one-pair-low-kicker)
      (hand-one-pair-and-ordered-kickers hand-a)
    (multiple-value-bind (hand-b-one-pair-rank hand-b-one-pair-higher-kicker hand-b-one-pair-middle-kicker hand-b-one-pair-low-kicker)
	(hand-one-pair-and-ordered-kickers hand-b)
      (let* ((hand-a-two-pair-better? nil))
	(cond
	  ((card-rank-better? hand-a-one-pair-rank hand-b-one-pair-rank)
	   (setf hand-a-two-pair-better? t))
	  ((and (card-rank-equal? hand-a-one-pair-rank hand-b-one-pair-rank)
		(card-rank-better? hand-a-one-pair-higher-kicker hand-b-one-pair-higher-kicker))
	   (setf hand-a-two-pair-better? t))
	  ((and (card-rank-equal? hand-a-one-pair-higher-kicker hand-b-one-pair-higher-kicker)
		(card-rank-better? hand-a-one-pair-middle-kicker hand-b-one-pair-middle-kicker))
	   (setf hand-a-two-pair-better? t))
	  ((and (card-rank-equal? hand-a-one-pair-middle-kicker hand-b-one-pair-middle-kicker)
		(card-rank-better? hand-a-one-pair-low-kicker hand-b-one-pair-low-kicker))
	   (setf hand-a-two-pair-better? t)))
	hand-a-two-pair-better?))))

(defun hand-better-within-hand-type-high-card? (hand-a hand-b)
  (hand-ordered-ranks-higher? hand-a hand-b))

#+NoCallers--I-removed-from-the-pcond
(defun hand-equal-within-hand-type? (hand-a hand-b hand-type)
  hand-a
  hand-b
  hand-type
  t)

;; @todo how to deal with kickers, other pairs etc -- thinking about dealing with this in hand-better-within-hand-type?
(defun hand-type-rank (hand hand-type)
  hand
  hand-type
  nil)

(defparameter *ordered-list-of-card-ranks*
  '(:ace :king :queen :jack :ten :nine :eight :seven :six :five :four :three :two))

(defun ordered-list-of-card-ranks ()
  *ordered-list-of-card-ranks*)

(defun card-rank-position (card-rank)
  (position card-rank (ordered-list-of-card-ranks)))

(defun card-rank-better? (card-rank-a card-rank-b)
  (< (card-rank-position card-rank-a)
     (card-rank-position card-rank-b)))

(defun card-rank-equal? (card-rank-a card-rank-b)
  (= (card-rank-position card-rank-a)
     (card-rank-position card-rank-b)))

(defun card-rank-worse? (card-rank-a card-rank-b)
  (and (not (card-rank-better? card-rank-a card-rank-b))
       (not (card-rank-equal? card-rank-a card-rank-b))))  

(defun position-card-rank (position)
  (nth position (ordered-list-of-card-ranks)))

(defun rank-ace? (card-rank)
  (eq :ace card-rank))

(defparameter *ace-to-five-straight-ranks-descending*
  (list :ace :five :four :three :two))

(defun ace-to-five-straight-ranks-descending ()
  *ace-to-five-straight-ranks-descending*)

(defparameter *list-of-card-suits*
  '(:spade :heart :diamond :club))

(defun list-of-card-suits ()
  *list-of-card-suits*)

(defstruct card
  rank
  suit)

;; maybe i should flip order so ace-club becomes club-ace

(defparameter ace-club   (make-card :rank :ace
				    :suit :club))
(defparameter king-club  (make-card :rank :king
				    :suit :club))
(defparameter queen-club (make-card :rank :queen
				    :suit :club))
(defparameter jack-club  (make-card :rank :jack
				    :suit :club))
(defparameter ten-club   (make-card :rank :ten
				    :suit :club))
(defparameter nine-club  (make-card :rank :nine
				    :suit :club))
(defparameter eight-club (make-card :rank :eight
				    :suit :club))
(defparameter seven-club (make-card :rank :seven
				    :suit :club))
(defparameter six-club   (make-card :rank :six
				    :suit :club))
(defparameter five-club  (make-card :rank :five
				    :suit :club))
(defparameter four-club  (make-card :rank :four
				    :suit :club))
(defparameter three-club (make-card :rank :three
				    :suit :club))
(defparameter two-club   (make-card :rank :two
				    :suit :club))

(defparameter ace-spade   (make-card :rank :ace
				     :suit :spade))
(defparameter king-spade  (make-card :rank :king
				     :suit :spade))
(defparameter queen-spade (make-card :rank :queen
				     :suit :spade))
(defparameter jack-spade  (make-card :rank :jack
				     :suit :spade))
(defparameter ten-spade   (make-card :rank :ten
				     :suit :spade))
(defparameter nine-spade  (make-card :rank :nine
				     :suit :spade))
(defparameter eight-spade (make-card :rank :eight
				     :suit :spade))
(defparameter seven-spade (make-card :rank :seven
				     :suit :spade))
(defparameter six-spade   (make-card :rank :six
				     :suit :spade))
(defparameter five-spade  (make-card :rank :five
				     :suit :spade))
(defparameter four-spade  (make-card :rank :four
				     :suit :spade))
(defparameter three-spade (make-card :rank :three
				     :suit :spade))
(defparameter two-spade   (make-card :rank :two
				     :suit :spade))

(defparameter ace-diamond   (make-card :rank :ace
				       :suit :diamond))
(defparameter king-diamond  (make-card :rank :king
				       :suit :diamond))
(defparameter queen-diamond (make-card :rank :queen
				       :suit :diamond))
(defparameter jack-diamond  (make-card :rank :jack
				       :suit :diamond))
(defparameter ten-diamond   (make-card :rank :ten
				       :suit :diamond))
(defparameter nine-diamond  (make-card :rank :nine
				       :suit :diamond))
(defparameter eight-diamond (make-card :rank :eight
				       :suit :diamond))
(defparameter seven-diamond (make-card :rank :seven
				       :suit :diamond))
(defparameter six-diamond   (make-card :rank :six
				       :suit :diamond))
(defparameter five-diamond  (make-card :rank :five
				       :suit :diamond))
(defparameter four-diamond  (make-card :rank :four
				       :suit :diamond))
(defparameter three-diamond (make-card :rank :three
				       :suit :diamond))
(defparameter two-diamond   (make-card :rank :two
				       :suit :diamond))

(defparameter ace-heart   (make-card :rank :ace
				     :suit :heart))
(defparameter king-heart  (make-card :rank :king
				     :suit :heart))
(defparameter queen-heart (make-card :rank :queen
				     :suit :heart))
(defparameter jack-heart  (make-card :rank :jack
				     :suit :heart))
(defparameter ten-heart   (make-card :rank :ten
				     :suit :heart))
(defparameter nine-heart  (make-card :rank :nine
				     :suit :heart))
(defparameter eight-heart (make-card :rank :eight
				     :suit :heart))
(defparameter seven-heart (make-card :rank :seven
				     :suit :heart))
(defparameter six-heart   (make-card :rank :six
				     :suit :heart))
(defparameter five-heart  (make-card :rank :five
				     :suit :heart))
(defparameter four-heart  (make-card :rank :four
				     :suit :heart))
(defparameter three-heart (make-card :rank :three
				     :suit :heart))
(defparameter two-heart   (make-card :rank :two
				     :suit :heart))

(defparameter *list-of-cards*
  (list

   ace-club
   king-club
   queen-club
   jack-club
   ten-club
   nine-club
   eight-club
   seven-club
   six-club
   five-club
   four-club
   three-club
   two-club

   ace-spade
   king-spade
   queen-spade
   jack-spade
   ten-spade
   nine-spade
   eight-spade
   seven-spade
   six-spade
   five-spade
   four-spade
   three-spade
   two-spade

   ace-diamond
   king-diamond
   queen-diamond
   jack-diamond
   ten-diamond
   nine-diamond
   eight-diamond
   seven-diamond
   six-diamond
   five-diamond
   four-diamond
   three-diamond
   two-diamond

   ace-heart
   king-heart
   queen-heart
   jack-heart
   ten-heart
   nine-heart
   eight-heart
   seven-heart
   six-heart
   five-heart
   four-heart
   three-heart
   two-heart))

(defun list-of-cards ()
  *list-of-cards*)

(defun print-list-of-cards ()
  (dolist (card (list-of-cards))
    (format t "~S~%" card)))

(defun card-has-suit? (card suit)
  (eq suit (card-suit card)))

(defun card-spade? (card)
  (card-has-suit? card :spade))

(defun card-heart? (card)
  (card-has-suit? card :heart))

(defun card-club? (card)
  (card-has-suit? card :club))

(defun card-diamond? (card)
  (card-has-suit? card :diamond))

;; is this what i want? 
(defun make-hand (card1 card2 card3 card4 card5)
  (list card1 card2 card3 card4 card5))

(defun hand-ranks (hand)
  (mapcar 'card-rank hand))

(defun hand-cards-descending-by-rank (hand)
  (let* ((hand-copy (copy-list hand))
	 (hand-ranks-descending (sort hand-copy #'card-rank-better? :key #'card-rank)))
    hand-ranks-descending))

(defun hand-ranks-descending (hand)
  (mapcar #'card-rank (hand-cards-descending-by-rank hand)))

(defparameter *example-hand* (make-hand ace-spade ace-diamond ace-heart ace-club two-club))

(defparameter *example-royal-flush*
  (make-hand ace-spade king-spade queen-spade jack-spade ten-spade))

(defparameter *example-straight-flush*
  (make-hand ten-club nine-club eight-club seven-club six-club))

(defparameter *example-four-of-a-kind*
  (make-hand two-spade two-club two-heart two-diamond four-diamond))

(defparameter *example-full-house*
  (make-hand five-heart five-diamond five-club seven-club seven-heart))

(defparameter *example-flush*
  (make-hand jack-heart seven-heart four-heart king-heart two-heart))

(defparameter *example-straight*
  (make-hand six-diamond seven-club eight-spade nine-heart ten-club))

(defparameter *example-three-of-a-kind*
  (make-hand queen-club queen-diamond queen-heart two-club four-heart))

(defparameter *example-two-pair*
  (make-hand seven-club seven-diamond nine-heart nine-spade king-heart))

(defparameter *example-one-pair*
  (make-hand jack-diamond jack-heart ten-heart three-club two-spade))

(defparameter *example-high-card*
  (make-hand ten-spade eight-heart six-club four-diamond two-club))

(defparameter *example-royal-flush-equal*
  (make-hand ace-club king-club queen-club jack-club ten-club))

(defparameter *example-straight-flush-equal*
  (make-hand ten-diamond nine-diamond eight-diamond seven-diamond six-diamond))

(defparameter *example-straight-flush-lower*
  (make-hand nine-club eight-club seven-club six-club five-club))

(defparameter *example-four-of-a-kind-equal*
  (make-hand two-spade two-club two-heart two-diamond four-heart))

(defparameter *example-four-of-a-kind-lower*
  (make-hand two-spade two-club two-heart two-diamond three-diamond))

(defparameter *example-full-house-equal*
  (make-hand five-heart five-diamond five-spade seven-club seven-heart))

(defparameter *example-full-house-lower*
  (make-hand four-heart four-diamond four-club seven-club seven-heart))

(defparameter *example-flush-equal*
  (make-hand jack-diamond seven-diamond four-diamond king-diamond two-diamond))

(defparameter *example-flush-lower*
  (make-hand ten-heart seven-heart four-heart king-heart two-heart))

(defparameter *example-straight-equal*
  (make-hand six-diamond seven-club eight-spade nine-heart ten-spade))

(defparameter *example-straight-lower*
  (make-hand five-heart six-diamond seven-club eight-spade nine-heart))

(defparameter *example-three-of-a-kind-equal*
  (make-hand queen-club queen-diamond queen-heart two-spade four-diamond))

(defparameter *example-three-of-a-kind-lower* ;; could have lower b/c rank of three of a kind lower or b/c kicker
  (make-hand jack-club jack-diamond jack-heart two-club four-heart)) ;; full coverage requires each version for all types
                                                                     ;; @todo example hands that cover each version
(defparameter *example-two-pair-equal*
  (make-hand seven-club seven-heart nine-heart nine-spade king-heart))

(defparameter *example-two-pair-lower*
  (make-hand seven-club seven-diamond eight-heart eight-spade king-heart))

(defparameter *example-one-pair-equal*
  (make-hand jack-diamond jack-spade ten-heart three-club two-spade))

(defparameter *example-one-pair-lower*
  (make-hand ten-diamond ten-heart nine-heart three-club two-spade))

(defparameter *example-high-card-equal*
  (make-hand ten-spade eight-diamond six-club four-diamond two-club))

(defparameter *example-high-card-lower*
  (make-hand ten-spade seven-heart six-club four-diamond two-club))

(defun print-hand (hand)
  (dolist (card hand)
    (format t "~S~&" card)))

(defun print-hand-human (hand)
  (dolist (card hand)
    (format t "~a of ~a~&" (card-rank card) (card-suit card))))

(defun p1 (input-list)
  (dolist (element input-list)
    (format t "~S~%" element)))

#+ignore
(defmacro do-hand (hand &body body)
  (let (card)
    `(dolist (,card ,hand)
       ,@body)))

;; maybe better to do compute-hand-type-and-rank
;;  because can get all that info at once
(defun compute-hand-type (hand)
  (cond
    ((hand-royal-flush? hand)
     :royal-flush)
    ((hand-straight-flush? hand)
     :straight-flush)
    ((hand-four-of-a-kind? hand)
     :four-of-a-kind)
    ((hand-full-house? hand)
     :full-house)
    ((hand-flush? hand)
     :flush)
    ((hand-straight? hand)
     :straight)
    ((hand-three-of-a-kind? hand)
     :three-of-a-kind)
    ((hand-two-pair? hand)
     :two-pair)
    ((hand-one-pair? hand)
     :one-pair)
    (t
     :high-card)))

;; the functions below do not check whether higher
;;   hand type applies. e.g. one-pair does not check
;;   whether three of a kind applies.

(defun hand-royal-flush? (hand)
  (and (hand-contains-ace-through-ten? hand)
       (hand-flush? hand)
       (hand-straight? hand)))

(defun hand-straight-flush? (hand)
  (and (hand-flush? hand)
       (hand-straight? hand)))

(defun hand-four-of-a-kind? (hand)
 (hand-n-of-a-kind? 4 hand))

;; aside: would like to have hand-three-of-a-kind
;;        and hand two of a kind with different
(defun hand-full-house? (hand)
  (hand-three-of-a-kind-with-one-rank-and-two-of-a-kind-with-different-rank? hand))

(defun hand-flush? (hand)
  (dolist (suit (list-of-card-suits))
    (when (hand-contains-only-cards-with-suit? hand suit)
      (return-from hand-flush? t))))

(defun hand-straight? (hand)
  (hand-cards-ordered-by-rank-continuous? hand))

(defun hand-three-of-a-kind? (hand)
  (hand-n-of-a-kind? 3 hand))

(defun hand-two-pair? (hand)
  (hand-pair-with-one-rank-and-pair-with-different-rank? hand))

(defun hand-one-pair? (hand)
  (hand-n-of-a-kind? 2 hand))

(defun card-ranks-ace-through-ten ()
  (card-ranks-sequence-of-length-five-starting-at-rank :ace))

(defun hand-contains-ace-through-ten? (hand)
  (let* ((hand-cards-ordered-by-rank-descending
	  (hand-cards-ordered-by-rank-descending hand))
	 (hard-cards-ranks-ordered-by-rank-descending
	  (mapcar 'card-rank hand-cards-ordered-by-rank-descending))
	 (card-ranks-ace-through-ten
	  (card-ranks-ace-through-ten)))
    (equal card-ranks-ace-through-ten
	   hard-cards-ranks-ordered-by-rank-descending)))

(defun hand-three-of-a-kind-with-one-rank-and-two-of-a-kind-with-different-rank? (hand)
  (let ((rank-with-three-of-a-kind nil)
	(rank-with-two-of-a-kind nil))
    (dolist (rank (ordered-list-of-card-ranks))
      (when (hand-three-of-a-kind-with-rank? hand rank)
	(setf rank-with-three-of-a-kind rank)))

    (when rank-with-three-of-a-kind
      (dolist (rank (ordered-list-of-card-ranks))
	(when (hand-one-pair-with-rank? hand rank)
	  (when (not (eq rank rank-with-three-of-a-kind))
	    (setf rank-with-two-of-a-kind rank)))))

    (values (and rank-with-three-of-a-kind
		 rank-with-two-of-a-kind)
	    rank-with-three-of-a-kind
	    rank-with-two-of-a-kind)))

(defun hand-n-of-a-kind? (n hand)
  (dolist (rank (ordered-list-of-card-ranks))
    (when (hand-n-of-a-kind-with-rank? n hand rank)
      (return-from hand-n-of-a-kind? t))))

(defun hand-n-of-a-kind-with-rank? (n hand rank)
  (= n (hand-number-of-cards-with-rank hand rank)))

(defun hand-four-of-a-kind-with-rank? (hand rank)
  (hand-n-of-a-kind-with-rank? 4 hand rank))

(defun hand-three-of-a-kind-with-rank? (hand rank)
  (hand-n-of-a-kind-with-rank? 3 hand rank))

(defun hand-one-pair-with-rank? (hand rank)
  (hand-n-of-a-kind-with-rank? 2 hand rank))

;; assumes hand is list. better if make macro.
;; easier for audience if not macro? 
(defun hand-number-of-cards-with-rank (hand rank)
  (let ((number-of-cards-with-rank 0))
    (dolist (card hand)
      (let ((card-rank (card-rank card)))
	(when (eq rank card-rank)
	  (incf number-of-cards-with-rank))))
    number-of-cards-with-rank))

(defun hand-contains-only-cards-with-suit? (hand suit)
  (every #'(lambda (card)
	     (eq suit (card-suit card)))
	 hand))

(defun hand-cards-ordered-by-rank-continuous? (hand)
  (let* ((cards-ordered-by-rank-descending 
	  (hand-cards-ordered-by-rank-descending hand))
	 (cards-ranks-continuous-descending?
	  (cards-ranks-continuous-descending? 
	   cards-ordered-by-rank-descending)))
    cards-ranks-continuous-descending?))

;; better: copy-hand
(defun hand-cards-ordered-by-rank-descending (hand)
  (let* ((hand-cards (copy-list hand))
	 (hand-cards-ordered-by-rank-descending
	  (sort hand-cards #'card-rank-better? :key #'card-rank)))
    hand-cards-ordered-by-rank-descending))

#+NoCallers
(defun hand-cards-ordered-by-rank-ascending (hand)
  (reverse (hand-cards-ordered-by-rank-descending hand)))

;; could do subseq equals or something
(defun cards-ranks-continuous-descending? (cards-ordered-by-rank-descending)
  (let* ((card-ranks (mapcar 'card-rank cards-ordered-by-rank-descending))
	 (highest-card-rank (first card-ranks)))
    (equal card-ranks
	   (card-ranks-sequence-of-length-five-starting-at-rank highest-card-rank))))

(defun card-ranks-sequence-of-length-starting-at-rank (sequence-length card-rank)
  (let* ((rank-position (card-rank-position card-rank))
	 (rank-position-end (+ rank-position sequence-length))
	 (number-of-ranks (length (ordered-list-of-card-ranks))))
    (when (< rank-position-end number-of-ranks)
      (subseq (ordered-list-of-card-ranks) rank-position rank-position-end))))

(defun card-ranks-sequence-of-length-five-starting-at-rank (card-rank)
  (card-ranks-sequence-of-length-starting-at-rank 5 card-rank))

;; similar to hand-three-of-a-kind-with-one-rank-and-two-of-a-kind-with-different-rank?
(defun hand-pair-with-one-rank-and-pair-with-different-rank? (hand)
  (let ((rank-with-first-pair nil)
	(rank-with-second-pair nil))
    (dolist (rank (ordered-list-of-card-ranks))
      (when (hand-one-pair-with-rank? hand rank)
	(setf rank-with-first-pair rank)))

    (when rank-with-first-pair
      (dolist (rank (ordered-list-of-card-ranks))
	(when (hand-one-pair-with-rank? hand rank)
	  (when (not (eq rank rank-with-first-pair))
	    (setf rank-with-second-pair rank)))))

    (values (and rank-with-first-pair
		 rank-with-second-pair)
	    rank-with-first-pair
	    rank-with-second-pair)))
  
#+ignore
  (let ((hand-contains-only-cards-with-suit? t))
    (dolist (card hand)
      (let ((card-suit (card-suit card)))
	(unless))))
;; could try to do this faster by asking whether there are
;;   three distinct ranks of card in hand
#+ignore
(defun hand-four-of-a-kind? (hand)
  (dolist (rank (ordered-list-of-card-ranks))
    (when (hand-four-of-a-kind-with-rank? hand rank)
      (return-from hand-four-of-a-kind? t))))


;; I am thinking that I should make a struct that contains
;;  hand type, highest rank, second highest rank,
;;  third highest rank
;; e.g. for a full house, it has full house type
;;          but it also has rank-with-three ace
;;                          rank-with-two jack


;; (defun hand-better? (hand-a hand-b)
;;   (let ((hand-a-hand-type (compute-hand-type hand-a))
;; 	(hand-b-hand-type (compute-hand-type hand-b)))
;;   (cond
;;     ((hand-type-better? hand-a-hand-type hand-b-hand-type)
;;      t)
;;     ((hand-type-better? hand-b-hand-type hand-a-hand-type)
;;      nil)
;;     ((and (hand-type-equal? hand-a-hand-type hand-b-hand-type)
;; 	  (hand-better-within-hand-type? hand-a hand-b hand-a-hand-type))
;;      t)
;;     ((hand-better-within-hand-type? hand-b hand-a hand-a-hand-type)
;;      nil)
;;     ((hand-equal-within-hand-type? hand-a hand-b hand-a-hand-type)
;;      nil)
;;     (t
;;      (error "should we ever reach this case?")))))

;; ;; todo -- determine how to compute less
;; (defun compute-hand-that-wins-or-draw (hand-a hand-b)
;;   (cond
;;     ((hand-better? hand-a hand-b)
;;      hand-a)
;;     ((hand-better? hand-b hand-a)
;;      hand-b)
;;     (t
;;      :draw)))


;; (defun example-incf-alist (hand)
;;   (let ((rank-and-occurrence-alist '()))
;;     (dolist (card hand)
      
;; (defun hand-four-of-a-kind-rank (hand)
;;   (let ((rank-and-occurrence-alist '()))
;;     (dolist (card hand)
;;       (let ((card-rank (card-rank card))
;; 	    (occurrences-for-rank (getf rank-and-occurrence-alist :card-rank)))
;; 	(cond
;; 	  ((and occurrences-for-rank           ; hands have five cards, so any rank that occurs more than 
;; 		(> occurrences-for-rank 1))    ;  once is the one that repeats four times
;; 	   (return-from hand-four-of-a-kind-rank card-rank)) 
;; 	  (occurrences-for-rank
;; 	   (setf (getf rank-and-occurrence-alist card-rank) (incf occurrences-for-rank)))
;; 	  (t
;; 	   (setf (getf rank-and-occurrence-alist card-rank) 1)))))))


;; (defun hand-better-within-hand-type-four-of-a-kind? (hand-a hand-b)
;;   "assumes hand-a and hand-b are four of a kinds.
;;    finds the card rank in which each has four of a kind. 
;;    compares them. when they are the same rank, then compares kickers"
;;   (multiple-value-bind (hand-a-four-of-a-kind-rank hand-a-four-of-a-kind-kicker)
;;       (hand-four-of-a-kind-rank-and-kicker hand-a)
;;     (multiple-value-bind 
;;   (let* ((hand-a-four-of-a-kind-rank (hand-four-of-a-kind-rank hand-a))
;; 	 (hand-b-four-of-a-kind-rank (hand-four-of-a-kind-rank hand-b))
;; 	 (hand-a-four-of-a-kind-rank-better? nil))
;;     (cond
;;       ((card-rank-better? hand-a-four-of-a-kind-rank hand-b-four-of-a-kind-rank)
;;        (setf hand-a-four-of-a-kind-rank-better? t))
;;       ((card-rank-equal? hand-a-four-of-a-kind-rank hand-b-four-of-a-kind-rank)
;;        (setf hand-a-four-of-a-kind-rank-better?
;; 	     (hand-four-of-a-kind-kicker-better? hand-a hand-b))))
;;     hand-a-four-of-a-kind-rank-better?))
