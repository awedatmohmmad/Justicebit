
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ESCROW_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ESCROW_ALREADY_EXISTS (err u103))
(define-constant ERR_ESCROW_EXPIRED (err u104))
(define-constant ERR_ESCROW_NOT_EXPIRED (err u105))
(define-constant ERR_DISPUTE_ALREADY_RAISED (err u106))
(define-constant ERR_NO_DISPUTE (err u107))
(define-constant ERR_VOTING_PERIOD_ACTIVE (err u108))
(define-constant ERR_VOTING_PERIOD_ENDED (err u109))
(define-constant ERR_ALREADY_VOTED (err u110))
(define-constant ERR_INSUFFICIENT_STAKE (err u111))

(define-constant DISPUTE_DURATION u144)
(define-constant MIN_JUDGE_STAKE u1000)
(define-constant JUDGE_REWARD_PERCENTAGE u10)

(define-data-var next-escrow-id uint u1)
(define-data-var total-judges uint u0)

(define-map escrows
  uint
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 20),
    dispute-raised: bool,
    dispute-started-at: (optional uint)
  }
)

(define-map disputes
  uint
  {
    escrow-id: uint,
    raised-by: principal,
    reason: (string-ascii 500),
    votes-for-buyer: uint,
    votes-for-seller: uint,
    total-votes: uint,
    resolved: bool,
    resolution: (optional (string-ascii 20))
  }
)

(define-map judges
  principal
  {
    stake: uint,
    reputation: uint,
    total-votes: uint,
    correct-votes: uint,
    is-active: bool
  }
)

(define-map judge-votes
  { judge: principal, escrow-id: uint }
  { vote: (string-ascii 20), voted-at: uint }
)

(define-map escrow-judges
  uint
  (list 10 principal)
)

(define-public (register-as-judge (stake-amount uint))
  (let ((current-stake (get stake (default-to { stake: u0, reputation: u100, total-votes: u0, correct-votes: u0, is-active: false } 
                                              (map-get? judges tx-sender)))))
    (asserts! (>= (+ current-stake stake-amount) MIN_JUDGE_STAKE) ERR_INSUFFICIENT_STAKE)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set judges tx-sender {
      stake: (+ current-stake stake-amount),
      reputation: u100,
      total-votes: u0,
      correct-votes: u0,
      is-active: true
    })
    (var-set total-judges (+ (var-get total-judges) u1))
    (ok true)
  )
)

(define-public (create-escrow (seller principal) (amount uint) (duration uint))
  (let ((escrow-id (var-get next-escrow-id))
        (current-block stacks-block-height))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? escrows escrow-id)) ERR_ESCROW_ALREADY_EXISTS)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set escrows escrow-id {
      buyer: tx-sender,
      seller: seller,
      amount: amount,
      created-at: current-block,
      expires-at: (+ current-block duration),
      status: "active",
      dispute-raised: false,
      dispute-started-at: none
    })
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (release-funds (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get buyer escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status escrow) "active") ERR_NOT_AUTHORIZED)
    (asserts! (not (get dispute-raised escrow)) ERR_DISPUTE_ALREADY_RAISED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get seller escrow))))
    (map-set escrows escrow-id (merge escrow { status: "completed" }))
    (ok true)
  )
)

(define-public (refund-escrow (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
        (current-block stacks-block-height))
    (asserts! (is-eq tx-sender (get seller escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (> current-block (get expires-at escrow)) ERR_ESCROW_NOT_EXPIRED)
    (asserts! (is-eq (get status escrow) "active") ERR_NOT_AUTHORIZED)
    (asserts! (not (get dispute-raised escrow)) ERR_DISPUTE_ALREADY_RAISED)
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get buyer escrow))))
    (map-set escrows escrow-id (merge escrow { status: "refunded" }))
    (ok true)
  )
)

(define-public (raise-dispute (escrow-id uint) (reason (string-ascii 500)))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
        (current-block stacks-block-height))
    (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status escrow) "active") ERR_NOT_AUTHORIZED)
    (asserts! (not (get dispute-raised escrow)) ERR_DISPUTE_ALREADY_RAISED)
    (asserts! (< current-block (get expires-at escrow)) ERR_ESCROW_EXPIRED)
    
    (let ((selected-judges (select-judges escrow-id)))
      (map-set escrows escrow-id (merge escrow { 
        dispute-raised: true, 
        dispute-started-at: (some current-block),
        status: "disputed"
      }))
      (map-set disputes escrow-id {
        escrow-id: escrow-id,
        raised-by: tx-sender,
        reason: reason,
        votes-for-buyer: u0,
        votes-for-seller: u0,
        total-votes: u0,
        resolved: false,
        resolution: none
      })
      (map-set escrow-judges escrow-id selected-judges)
      (ok true)
    )
  )
)

(define-public (vote-on-dispute (escrow-id uint) (vote-for (string-ascii 20)))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
        (dispute (unwrap! (map-get? disputes escrow-id) ERR_NO_DISPUTE))
        (judge (unwrap! (map-get? judges tx-sender) ERR_NOT_AUTHORIZED))
        (current-block stacks-block-height)
        (dispute-start (unwrap! (get dispute-started-at escrow) ERR_NO_DISPUTE)))
    
    (asserts! (get is-active judge) ERR_NOT_AUTHORIZED)
    (asserts! (get dispute-raised escrow) ERR_NO_DISPUTE)
    (asserts! (< current-block (+ dispute-start DISPUTE_DURATION)) ERR_VOTING_PERIOD_ENDED)
    (asserts! (is-none (map-get? judge-votes { judge: tx-sender, escrow-id: escrow-id })) ERR_ALREADY_VOTED)
    (asserts! (or (is-eq vote-for "buyer") (is-eq vote-for "seller")) ERR_NOT_AUTHORIZED)
    
    (map-set judge-votes { judge: tx-sender, escrow-id: escrow-id } { vote: vote-for, voted-at: current-block })
    
    (let ((updated-dispute (if (is-eq vote-for "buyer")
                             (merge dispute { 
                               votes-for-buyer: (+ (get votes-for-buyer dispute) u1),
                               total-votes: (+ (get total-votes dispute) u1)
                             })
                             (merge dispute { 
                               votes-for-seller: (+ (get votes-for-seller dispute) u1),
                               total-votes: (+ (get total-votes dispute) u1)
                             }))))
      (map-set disputes escrow-id updated-dispute)
      (map-set judges tx-sender (merge judge { total-votes: (+ (get total-votes judge) u1) }))
      (ok true)
    )
  )
)

(define-public (resolve-dispute (escrow-id uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
        (dispute (unwrap! (map-get? disputes escrow-id) ERR_NO_DISPUTE))
        (current-block stacks-block-height)
        (dispute-start (unwrap! (get dispute-started-at escrow) ERR_NO_DISPUTE)))
    
    (asserts! (get dispute-raised escrow) ERR_NO_DISPUTE)
    (asserts! (> current-block (+ dispute-start DISPUTE_DURATION)) ERR_VOTING_PERIOD_ACTIVE)
    (asserts! (not (get resolved dispute)) ERR_NOT_AUTHORIZED)
    
    (let ((winner (if (> (get votes-for-buyer dispute) (get votes-for-seller dispute)) "buyer" "seller"))
          (total-amount (get amount escrow))
          (judge-reward (/ (* total-amount JUDGE_REWARD_PERCENTAGE) u100))
          (remaining-amount (- total-amount judge-reward)))
      
      (if (is-eq winner "buyer")
        (try! (as-contract (stx-transfer? remaining-amount tx-sender (get buyer escrow))))
        (try! (as-contract (stx-transfer? remaining-amount tx-sender (get seller escrow))))
      )
      
      (unwrap! (distribute-judge-rewards escrow-id judge-reward winner) ERR_NOT_AUTHORIZED)
      
      (map-set disputes escrow-id (merge dispute { resolved: true, resolution: (some winner) }))
      (map-set escrows escrow-id (merge escrow { status: "resolved" }))
      (ok winner)
    )
  )
)

(define-private (select-judges (escrow-id uint))
  (list tx-sender)
)

(define-private (distribute-judge-rewards (escrow-id uint) (total-reward uint) (winning-side (string-ascii 20)))
  (let ((judges-list (default-to (list) (map-get? escrow-judges escrow-id))))
    (ok true)
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id)
)

(define-read-only (get-dispute (escrow-id uint))
  (map-get? disputes escrow-id)
)

(define-read-only (get-judge (judge principal))
  (map-get? judges judge)
)

(define-read-only (get-judge-vote (judge principal) (escrow-id uint))
  (map-get? judge-votes { judge: judge, escrow-id: escrow-id })
)

(define-read-only (get-next-escrow-id)
  (var-get next-escrow-id)
)

(define-read-only (get-total-judges)
  (var-get total-judges)
)

(define-read-only (is-dispute-active (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow (match (get dispute-started-at escrow)
             dispute-start (let ((current-block stacks-block-height))
                            (and (get dispute-raised escrow)
                                 (< current-block (+ dispute-start DISPUTE_DURATION))))
             false)
    false
  )
)