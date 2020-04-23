(defparameter *test-cases-compute-card-rank*
  (list

   '(card-rank-better? :ace :king)
   '(card-rank-better? :ace :queen)
   '(card-rank-better? :ace :jack)
   '(card-rank-better? :ace :ten)
   '(card-rank-better? :ace :nine)
   '(card-rank-better? :ace :eight)
   '(card-rank-better? :ace :seven)
   '(card-rank-better? :ace :six)
   '(card-rank-better? :ace :five)
   '(card-rank-better? :ace :four)
   '(card-rank-better? :ace :three)
   '(card-rank-better? :ace :two)

   '(not (card-rank-better? :king :ace))
   '(card-rank-better? :king :queen)
   '(card-rank-better? :king :jack)
   '(card-rank-better? :king :ten)
   '(card-rank-better? :king :nine)
   '(card-rank-better? :king :eight)
   '(card-rank-better? :king :seven)
   '(card-rank-better? :king :six)
   '(card-rank-better? :king :five)
   '(card-rank-better? :king :four)
   '(card-rank-better? :king :three)
   '(card-rank-better? :king :two)

   ;; and so on


   ))

(defun test-cases-compute-card-rank ()
  *test-cases-compute-card-rank*)

(defparameter *test-cases-hand-highest-card-rank*
  (list

   '(eq :ace    (hand-highest-card-rank *example-royal-flush*))
   '(eq :ten    (hand-highest-card-rank *example-straight-flush*))
   '(eq :four  (hand-highest-card-rank *example-four-of-a-kind*))
   '(eq :seven  (hand-highest-card-rank *example-full-house*))
   '(eq :king   (hand-highest-card-rank *example-flush*))
   '(eq :ten    (hand-highest-card-rank *example-straight*))
   '(eq :queen  (hand-highest-card-rank *example-three-of-a-kind*))
   '(eq :king   (hand-highest-card-rank *example-two-pair*))
   '(eq :jack   (hand-highest-card-rank *example-one-pair*))
   '(eq :ten    (hand-highest-card-rank *example-high-card*))))

(defun test-cases-hand-highest-card-rank ()
  *test-cases-hand-highest-card-rank*)

(defparameter *test-cases-compute-hand-type*
  (list
   
   '(hand-royal-flush?     *example-royal-flush*)
   '(hand-straight-flush?  *example-straight-flush*)
   '(hand-four-of-a-kind?  *example-four-of-a-kind*)
   '(hand-full-house?      *example-full-house*)
   '(hand-flush?           *example-flush*)
   '(hand-straight?        *example-straight*)
   '(hand-three-of-a-kind? *example-three-of-a-kind*)
   '(hand-two-pair?        *example-two-pair*)
   '(hand-one-pair?        *example-one-pair*)

   '(hand-straight-flush? *example-royal-flush*)
   '(not (hand-four-of-a-kind? *example-royal-flush*))
   '(not (hand-full-house? *example-royal-flush*))
   '(hand-flush? *example-royal-flush*)
   '(hand-flush? *example-royal-flush*)
   '(not (hand-three-of-a-kind? *example-royal-flush*))
   '(not (hand-two-pair? *example-royal-flush*))
   '(not (hand-one-pair? *example-royal-flush*))

   '(not (hand-royal-flush? *example-straight-flush*))
   '(not (hand-four-of-a-kind? *example-straight-flush*))
   '(not (hand-full-house? *example-straight-flush*))
   '(not (hand-three-of-a-kind? *example-straight-flush*))
   '(not (hand-two-pair? *example-straight-flush*))
   '(not (hand-one-pair? *example-straight-flush*))

   '(not (hand-royal-flush? *example-four-of-a-kind*))
   '(not (hand-straight-flush?  *example-four-of-a-kind*))
   '(not (hand-full-house? *example-four-of-a-kind*))
   '(not (hand-straight? *example-four-of-a-kind*))
   '(not (hand-flush? *example-four-of-a-kind*)) 
   '(not (hand-two-pair? *example-four-of-a-kind*))

   '(not (hand-royal-flush? *example-full-house*))
   '(not (hand-four-of-a-kind? *example-full-house*))
   '(not (hand-straight? *example-full-house*))
   '(not (hand-flush? *example-full-house*)) 

   '(not (hand-royal-flush?     *example-flush*))
   '(not (hand-straight-flush?  *example-flush*))
   '(not (hand-four-of-a-kind?  *example-flush*))
   '(not (hand-full-house?      *example-flush*))
   '(hand-flush?           *example-flush*)
   '(not (hand-straight?        *example-flush*))
   '(not (hand-three-of-a-kind? *example-flush*))
   '(not (hand-two-pair?        *example-flush*))
   '(not (hand-one-pair?        *example-flush*))

   '(not (hand-royal-flush?     *example-straight*))
   '(not (hand-straight-flush?  *example-straight*))
   '(not (hand-four-of-a-kind?  *example-straight*))
   '(not (hand-full-house?      *example-straight*))
   '(not (hand-flush?           *example-straight*))
   '(hand-straight?             *example-straight*)
   '(not (hand-three-of-a-kind? *example-straight*))
   '(not (hand-two-pair?        *example-straight*))
   '(not (hand-one-pair?        *example-straight*))

   '(not (hand-royal-flush?     *example-three-of-a-kind*))
   '(not (hand-straight-flush?  *example-three-of-a-kind*))
   '(not (hand-four-of-a-kind?  *example-three-of-a-kind*))
   '(not (hand-full-house?      *example-three-of-a-kind*))
   '(not (hand-flush?           *example-three-of-a-kind*))
   '(not (hand-straight?        *example-three-of-a-kind*))
   '(hand-three-of-a-kind?      *example-three-of-a-kind*)   
   '(not (hand-two-pair?        *example-three-of-a-kind*))

   '(not (hand-royal-flush?     *example-two-pair*))
   '(not (hand-straight-flush?  *example-two-pair*))
   '(not (hand-four-of-a-kind?  *example-two-pair*))
   '(not (hand-full-house?      *example-two-pair*))
   '(not (hand-flush?           *example-two-pair*))
   '(not (hand-straight?        *example-two-pair*))
   '(not (hand-three-of-a-kind? *example-two-pair*))
   '(hand-two-pair?             *example-two-pair*)

   '(not (hand-royal-flush?     *example-one-pair*))
   '(not (hand-straight-flush?  *example-one-pair*))
   '(not (hand-four-of-a-kind?  *example-one-pair*))
   '(not (hand-full-house?      *example-one-pair*))
   '(not (hand-flush?           *example-one-pair*))
   '(not (hand-straight?        *example-one-pair*))
   '(not (hand-three-of-a-kind? *example-one-pair*))
   '(not (hand-two-pair?        *example-one-pair*))
   '(hand-one-pair?        *example-one-pair*)

   '(not (hand-royal-flush?     *example-high-card*))
   '(not (hand-straight-flush?  *example-high-card*))
   '(not (hand-four-of-a-kind?  *example-high-card*))
   '(not (hand-full-house?      *example-high-card*))
   '(not (hand-flush?           *example-high-card*))
   '(not (hand-straight?        *example-high-card*))
   '(not (hand-three-of-a-kind? *example-high-card*))
   '(not (hand-two-pair?        *example-high-card*))
   '(not (hand-one-pair?        *example-high-card*))

   '(eq (compute-hand-type *example-royal-flush*)    :royal-flush)
   '(eq (compute-hand-type *example-straight-flush*)  :straight-flush)
   '(eq (compute-hand-type *example-four-of-a-kind*)   :four-of-a-kind)
   '(eq (compute-hand-type *example-full-house*)      :full-house)
   '(eq (compute-hand-type *example-flush*)           :flush)
   '(eq (compute-hand-type *example-straight*)        :straight)
   '(eq (compute-hand-type *example-three-of-a-kind*) :three-of-a-kind)
   '(eq (compute-hand-type *example-two-pair*)        :two-pair)
   '(eq (compute-hand-type *example-one-pair*)        :one-pair)
   '(eq (compute-hand-type *example-high-card*)       :high-card)))

(defun test-cases-compute-hand-type ()
  *test-cases-compute-hand-type*)

(defparameter *test-cases-number-of-cards*
  (list
   
   '(eql 52 (length (list-of-cards)))
   '(eql 13 (length (remove-if-not 'card-diamond? (list-of-cards))))
   '(eql 13 (length (remove-if-not 'card-heart? (list-of-cards))))
   '(eql 13 (length (remove-if-not 'card-club? (list-of-cards))))
   '(eql 13 (length (remove-if-not 'card-spade? (list-of-cards))))))

(defun test-cases-number-of-cards ()
  *test-cases-number-of-cards*)

;; increase number of examples of each type of hand

(defparameter *test-cases-hand-type-better*
  (list

   '(hand-type-better? 
     (compute-hand-type *example-royal-flush*)
     (compute-hand-type *example-straight-flush*))
   '(hand-type-better? 
     (compute-hand-type *example-royal-flush*)
     (compute-hand-type *example-four-of-a-kind*))
   '(hand-type-better? 
     (compute-hand-type *example-royal-flush*)
     (compute-hand-type *example-full-house*))
   '(hand-type-better? 
     (compute-hand-type *example-royal-flush*)
     (compute-hand-type *example-flush*))
   '(hand-type-better? 
     (compute-hand-type *example-royal-flush*)
     (compute-hand-type *example-straight*))
   '(hand-type-better? 
     (compute-hand-type *example-royal-flush*)
     (compute-hand-type *example-three-of-a-kind*))
   '(hand-type-better? 
     (compute-hand-type *example-royal-flush*)
     (compute-hand-type *example-two-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-royal-flush*)
     (compute-hand-type *example-one-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-royal-flush*)
     (compute-hand-type *example-high-card*))


   '(not (hand-type-better? 
	  (compute-hand-type *example-straight-flush*)
	  (compute-hand-type *example-royal-flush*)))
   '(hand-type-better? 
     (compute-hand-type *example-straight-flush*)
     (compute-hand-type *example-four-of-a-kind*))
   '(hand-type-better? 
     (compute-hand-type *example-straight-flush*)
     (compute-hand-type *example-full-house*))
   '(hand-type-better? 
     (compute-hand-type *example-straight-flush*)
     (compute-hand-type *example-flush*))
   '(hand-type-better? 
     (compute-hand-type *example-straight-flush*)
     (compute-hand-type *example-straight*))
   '(hand-type-better? 
     (compute-hand-type *example-straight-flush*)
     (compute-hand-type *example-three-of-a-kind*))
   '(hand-type-better? 
     (compute-hand-type *example-straight-flush*)
     (compute-hand-type *example-two-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-straight-flush*)
     (compute-hand-type *example-one-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-straight-flush*)
     (compute-hand-type *example-high-card*))


   '(not (hand-type-better? 
	  (compute-hand-type *example-four-of-a-kind*)
	  (compute-hand-type *example-royal-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-four-of-a-kind*)
	  (compute-hand-type *example-straight-flush*)))
   '(hand-type-better? 
     (compute-hand-type *example-four-of-a-kind*)
     (compute-hand-type *example-full-house*))
   '(hand-type-better? 
     (compute-hand-type *example-four-of-a-kind*)
     (compute-hand-type *example-flush*))
   '(hand-type-better? 
     (compute-hand-type *example-four-of-a-kind*)
     (compute-hand-type *example-straight*))
   '(hand-type-better? 
     (compute-hand-type *example-four-of-a-kind*)
     (compute-hand-type *example-three-of-a-kind*))
   '(hand-type-better? 
     (compute-hand-type *example-four-of-a-kind*)
     (compute-hand-type *example-two-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-four-of-a-kind*)
     (compute-hand-type *example-one-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-four-of-a-kind*)
     (compute-hand-type *example-high-card*))

     
   '(not (hand-type-better? 
	  (compute-hand-type *example-full-house*)
	  (compute-hand-type *example-royal-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-full-house*)
	  (compute-hand-type *example-straight-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-full-house*)
	  (compute-hand-type *example-four-of-a-kind*)))
   '(hand-type-better? 
     (compute-hand-type *example-full-house*)
     (compute-hand-type *example-flush*))
   '(hand-type-better? 
     (compute-hand-type *example-full-house*)
     (compute-hand-type *example-straight*))
   '(hand-type-better? 
     (compute-hand-type *example-full-house*)
     (compute-hand-type *example-three-of-a-kind*))
   '(hand-type-better? 
     (compute-hand-type *example-full-house*)
     (compute-hand-type *example-two-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-full-house*)
     (compute-hand-type *example-one-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-full-house*)
     (compute-hand-type *example-high-card*))

   '(not (hand-type-better? 
	  (compute-hand-type *example-flush*)
	  (compute-hand-type *example-royal-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-flush*)
	  (compute-hand-type *example-straight-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-flush*)
	  (compute-hand-type *example-four-of-a-kind*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-flush*)
	  (compute-hand-type *example-full-house*)))
   '(hand-type-better? 
     (compute-hand-type *example-flush*)
     (compute-hand-type *example-straight*))
   '(hand-type-better? 
     (compute-hand-type *example-flush*)
     (compute-hand-type *example-three-of-a-kind*))
   '(hand-type-better? 
     (compute-hand-type *example-flush*)
     (compute-hand-type *example-two-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-flush*)
     (compute-hand-type *example-one-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-flush*)
     (compute-hand-type *example-high-card*))
   
   '(not (hand-type-better? 
	  (compute-hand-type *example-straight*)
	  (compute-hand-type *example-royal-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-straight*)
	  (compute-hand-type *example-straight-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-straight*)
	  (compute-hand-type *example-four-of-a-kind*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-straight*)
	  (compute-hand-type *example-full-house*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-straight*)
	  (compute-hand-type *example-flush*)))
   '(hand-type-better? 
     (compute-hand-type *example-straight*)
     (compute-hand-type *example-three-of-a-kind*))
   '(hand-type-better? 
     (compute-hand-type *example-straight*)
     (compute-hand-type *example-two-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-straight*)
     (compute-hand-type *example-one-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-straight*)
     (compute-hand-type *example-high-card*))


   '(not (hand-type-better? 
	  (compute-hand-type *example-three-of-a-kind*)
	  (compute-hand-type *example-royal-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-three-of-a-kind*)
	  (compute-hand-type *example-straight-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-three-of-a-kind*)
	  (compute-hand-type *example-four-of-a-kind*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-three-of-a-kind*)
	  (compute-hand-type *example-full-house*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-three-of-a-kind*)
	  (compute-hand-type *example-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-three-of-a-kind*)
	  (compute-hand-type *example-straight*)))
   '(hand-type-better? 
     (compute-hand-type *example-three-of-a-kind*)
     (compute-hand-type *example-two-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-three-of-a-kind*)
     (compute-hand-type *example-one-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-three-of-a-kind*)
     (compute-hand-type *example-high-card*))


   '(not (hand-type-better? 
	  (compute-hand-type *example-two-pair*)
	  (compute-hand-type *example-royal-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-two-pair*)
	  (compute-hand-type *example-straight-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-two-pair*)
	  (compute-hand-type *example-four-of-a-kind*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-two-pair*)
	  (compute-hand-type *example-full-house*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-two-pair*)
	  (compute-hand-type *example-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-two-pair*)
	  (compute-hand-type *example-straight*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-two-pair*)
	  (compute-hand-type *example-three-of-a-kind*)))
   '(hand-type-better? 
     (compute-hand-type *example-two-pair*)
     (compute-hand-type *example-one-pair*))
   '(hand-type-better? 
     (compute-hand-type *example-two-pair*)
     (compute-hand-type *example-high-card*))

   '(not (hand-type-better? 
	  (compute-hand-type *example-one-pair*)
	  (compute-hand-type *example-royal-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-one-pair*)
	  (compute-hand-type *example-straight-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-one-pair*)
	  (compute-hand-type *example-four-of-a-kind*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-one-pair*)
	  (compute-hand-type *example-full-house*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-one-pair*)
	  (compute-hand-type *example-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-one-pair*)
	  (compute-hand-type *example-straight*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-one-pair*)
	  (compute-hand-type *example-three-of-a-kind*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-one-pair*)
	  (compute-hand-type *example-two-pair*)))
   '(hand-type-better? 
     (compute-hand-type *example-one-pair*)
     (compute-hand-type *example-high-card*))
         

   '(not (hand-type-better? 
	  (compute-hand-type *example-high-card*)
	  (compute-hand-type *example-royal-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-high-card*)
	  (compute-hand-type *example-straight-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-high-card*)
	  (compute-hand-type *example-four-of-a-kind*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-high-card*)
	  (compute-hand-type *example-full-house*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-high-card*)
	  (compute-hand-type *example-flush*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-high-card*)
	  (compute-hand-type *example-straight*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-high-card*)
	  (compute-hand-type *example-three-of-a-kind*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-high-card*)
	  (compute-hand-type *example-two-pair*)))
   '(not (hand-type-better? 
	  (compute-hand-type *example-high-card*)
	  (compute-hand-type *example-one-pair*)))

   ))

(defun test-cases-hand-type-better ()
  *test-cases-hand-type-better*)

(defparameter *test-cases-hand-better-worse-or-draw*
  (list

   '(eq :better (hand-better-worse-or-draw *example-straight-flush* *example-straight-flush-lower*))
   '(eq :better (hand-better-worse-or-draw *example-four-of-a-kind* *example-four-of-a-kind-lower*))
   '(eq :better (hand-better-worse-or-draw *example-full-house* *example-full-house-lower*))
   '(eq :better (hand-better-worse-or-draw *example-flush* *example-flush-lower*))
   '(eq :better (hand-better-worse-or-draw *example-straight* *example-straight-lower*))
   '(eq :better (hand-better-worse-or-draw *example-three-of-a-kind* *example-three-of-a-kind-lower*))
   '(eq :better (hand-better-worse-or-draw *example-two-pair* *example-two-pair-lower*))
   '(eq :better (hand-better-worse-or-draw *example-one-pair* *example-one-pair-lower*))
   '(eq :better (hand-better-worse-or-draw *example-high-card* *example-high-card-lower*))

   '(eq :worse (hand-better-worse-or-draw *example-straight-flush-lower* *example-straight-flush*))
   '(eq :worse (hand-better-worse-or-draw *example-four-of-a-kind-lower* *example-four-of-a-kind*))
   '(eq :worse (hand-better-worse-or-draw *example-full-house-lower* *example-full-house*))
   '(eq :worse (hand-better-worse-or-draw *example-flush-lower* *example-flush*))
   '(eq :worse (hand-better-worse-or-draw *example-straight-lower* *example-straight*))
   '(eq :worse (hand-better-worse-or-draw *example-three-of-a-kind-lower* *example-three-of-a-kind*))
   '(eq :worse (hand-better-worse-or-draw *example-two-pair-lower* *example-two-pair*))
   '(eq :worse (hand-better-worse-or-draw *example-one-pair-lower* *example-one-pair*))
   '(eq :worse (hand-better-worse-or-draw *example-high-card-lower* *example-high-card*))

   '(eq :better (hand-better-worse-or-draw *example-straight-flush-equal* *example-straight-flush-lower*))
   '(eq :better (hand-better-worse-or-draw *example-four-of-a-kind-equal* *example-four-of-a-kind-lower*))
   '(eq :better (hand-better-worse-or-draw *example-full-house-equal* *example-full-house-lower*))
   '(eq :better (hand-better-worse-or-draw *example-flush-equal* *example-flush-lower*))
   '(eq :better (hand-better-worse-or-draw *example-straight-equal* *example-straight-lower*))
   '(eq :better (hand-better-worse-or-draw *example-three-of-a-kind-equal* *example-three-of-a-kind-lower*))
   '(eq :better (hand-better-worse-or-draw *example-two-pair-equal* *example-two-pair-lower*))
   '(eq :better (hand-better-worse-or-draw *example-one-pair-equal* *example-one-pair-lower*))
   '(eq :better (hand-better-worse-or-draw *example-high-card-equal* *example-high-card-lower*))

   '(eq :worse (hand-better-worse-or-draw *example-straight-flush-lower* *example-straight-flush-equal*))
   '(eq :worse (hand-better-worse-or-draw *example-four-of-a-kind-lower* *example-four-of-a-kind-equal*))
   '(eq :worse (hand-better-worse-or-draw *example-full-house-lower* *example-full-house-equal*))
   '(eq :worse (hand-better-worse-or-draw *example-flush-lower* *example-flush-equal*))
   '(eq :worse (hand-better-worse-or-draw *example-straight-lower* *example-straight-equal*))
   '(eq :worse (hand-better-worse-or-draw *example-three-of-a-kind-lower* *example-three-of-a-kind-equal*))
   '(eq :worse (hand-better-worse-or-draw *example-two-pair-lower* *example-two-pair-equal*))
   '(eq :worse (hand-better-worse-or-draw *example-one-pair-lower* *example-one-pair-equal*))
   '(eq :worse (hand-better-worse-or-draw *example-high-card-lower* *example-high-card-equal*))
   
   '(eq :draw (hand-better-worse-or-draw *example-royal-flush* *example-royal-flush-equal*))
   '(eq :draw (hand-better-worse-or-draw *example-straight-flush* *example-straight-flush-equal*))
   '(eq :draw (hand-better-worse-or-draw *example-four-of-a-kind* *example-four-of-a-kind-equal*))
   '(eq :draw (hand-better-worse-or-draw *example-full-house* *example-full-house-equal*))
   '(eq :draw (hand-better-worse-or-draw *example-flush* *example-flush-equal*))
   '(eq :draw (hand-better-worse-or-draw *example-straight* *example-straight-equal*))
   '(eq :draw (hand-better-worse-or-draw *example-three-of-a-kind* *example-three-of-a-kind-equal*))
   '(eq :draw (hand-better-worse-or-draw *example-two-pair* *example-two-pair-equal*))
   '(eq :draw (hand-better-worse-or-draw *example-one-pair* *example-one-pair-equal*))
   '(eq :draw (hand-better-worse-or-draw *example-high-card* *example-high-card-equal*))

   '(eq :draw (hand-better-worse-or-draw *example-royal-flush-equal* *example-royal-flush*))
   '(eq :draw (hand-better-worse-or-draw *example-straight-flush-equal* *example-straight-flush*))
   '(eq :draw (hand-better-worse-or-draw *example-four-of-a-kind-equal* *example-four-of-a-kind*))
   '(eq :draw (hand-better-worse-or-draw *example-full-house-equal* *example-full-house*))
   '(eq :draw (hand-better-worse-or-draw *example-flush-equal* *example-flush*))
   '(eq :draw (hand-better-worse-or-draw *example-straight-equal* *example-straight*))
   '(eq :draw (hand-better-worse-or-draw *example-three-of-a-kind-equal* *example-three-of-a-kind*))
   '(eq :draw (hand-better-worse-or-draw *example-two-pair-equal* *example-two-pair*))
   '(eq :draw (hand-better-worse-or-draw *example-one-pair-equal* *example-one-pair*))
   '(eq :draw (hand-better-worse-or-draw *example-high-card-equal* *example-high-card*))

   ))

(defun test-cases-hand-better-worse-or-draw () 
  *test-cases-hand-better-worse-or-draw*)

(defparameter *test-cases*
  (append (test-cases-compute-card-rank)
	  (test-cases-hand-highest-card-rank)
	  (test-cases-compute-hand-type)
	  (test-cases-number-of-cards)
	  (test-cases-hand-type-better)
	  (test-cases-hand-better-worse-or-draw)))

(defun test-cases ()
  *test-cases*)

(defun run-test-cases (&key (verbose? nil))
  (let ((all-test-cases-pass? t))
    (dolist (test-case (test-cases))
      (let ((test-case-result (eval test-case)))
	(when verbose?
	  (format t "~&test-case expression: ~S" test-case)
	  (format t "~&test-case value: ~S" test-case-result))
	(setf all-test-cases-pass?
	      (and all-test-cases-pass?
		   test-case-result))
	(unless test-case-result
	  (format t "~&failing test case: ~S" test-case))))
    all-test-cases-pass?))
