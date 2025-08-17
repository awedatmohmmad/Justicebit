
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
(define-constant ERR_MULTIPARTY_NOT_FOUND (err u112))
(define-constant ERR_INVALID_PARTY (err u113))
(define-constant ERR_INSUFFICIENT_FUNDS (err u114))
(define-constant ERR_RELEASE_ALREADY_REQUESTED (err u115))
(define-constant ERR_INVALID_RELEASE_AMOUNT (err u116))
(define-constant ERR_PARTY_ALREADY_EXISTS (err u117))
(define-constant ERR_INVALID_THRESHOLD (err u118))
(define-constant ERR_INSURANCE_NOT_FOUND (err u119))
(define-constant ERR_CLAIM_NOT_FOUND (err u120))
(define-constant ERR_INSUFFICIENT_POOL_FUNDS (err u121))
(define-constant ERR_CLAIM_ALREADY_EXISTS (err u122))
(define-constant ERR_CLAIM_NOT_ELIGIBLE (err u123))
(define-constant ERR_INVALID_COVERAGE_AMOUNT (err u124))
(define-constant ERR_CONTRIBUTOR_NOT_FOUND (err u125))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u126))

(define-constant DISPUTE_DURATION u144)
(define-constant MIN_JUDGE_STAKE u1000)
(define-constant JUDGE_REWARD_PERCENTAGE u10)
(define-constant INSURANCE_PREMIUM_RATE u2)
(define-constant MIN_INSURANCE_CONTRIBUTION u500)
(define-constant CLAIM_VALIDATION_PERIOD u48)
(define-constant MAX_COVERAGE_PERCENTAGE u80)

(define-data-var next-escrow-id uint u1)
(define-data-var total-judges uint u0)
(define-data-var next-multiparty-id uint u1)
(define-data-var insurance-pool-balance uint u0)
(define-data-var total-contributors uint u0)
(define-data-var next-claim-id uint u1)

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

(define-map multiparty-escrows
  uint
  {
    creator: principal,
    total-amount: uint,
    available-amount: uint,
    approval-threshold: uint,
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 20),
    party-count: uint
  }
)

(define-map multiparty-parties
  { escrow-id: uint, party: principal }
  {
    contribution: uint,
    is-authorized: bool,
    joined-at: uint
  }
)

(define-map partial-releases
  uint
  {
    escrow-id: uint,
    recipient: principal,
    amount: uint,
    requester: principal,
    approvals: uint,
    executed: bool,
    created-at: uint
  }
)

(define-map release-approvals
  { release-id: uint, approver: principal }
  { approved: bool, approved-at: uint }
)

(define-data-var next-release-id uint u1)

(define-map insurance-contributors
  principal
  {
    contribution: uint,
    rewards-earned: uint,
    joined-at: uint,
    is-active: bool
  }
)

(define-map escrow-insurance
  uint
  {
    coverage-amount: uint,
    premium-paid: uint,
    is-covered: bool,
    coverage-activated-at: uint
  }
)

(define-map insurance-claims
  uint
  {
    escrow-id: uint,
    claimant: principal,
    claim-amount: uint,
    reason: (string-ascii 500),
    submitted-at: uint,
    status: (string-ascii 20),
    validator-votes: uint,
    approved-votes: uint,
    processed-at: (optional uint)
  }
)

(define-map claim-validations
  { claim-id: uint, validator: principal }
  { approved: bool, validated-at: uint }
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

(define-public (create-multiparty-escrow (approval-threshold uint) (duration uint))
  (let ((escrow-id (var-get next-multiparty-id))
        (current-block stacks-block-height))
    (asserts! (> approval-threshold u0) ERR_INVALID_THRESHOLD)
    (asserts! (> duration u0) ERR_INVALID_AMOUNT)
    (map-set multiparty-escrows escrow-id {
      creator: tx-sender,
      total-amount: u0,
      available-amount: u0,
      approval-threshold: approval-threshold,
      created-at: current-block,
      expires-at: (+ current-block duration),
      status: "active",
      party-count: u0
    })
    (var-set next-multiparty-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (join-multiparty-escrow (escrow-id uint) (contribution uint))
  (let ((escrow (unwrap! (map-get? multiparty-escrows escrow-id) ERR_MULTIPARTY_NOT_FOUND))
        (current-block stacks-block-height))
    (asserts! (> contribution u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq (get status escrow) "active") ERR_NOT_AUTHORIZED)
    (asserts! (< current-block (get expires-at escrow)) ERR_ESCROW_EXPIRED)
    (asserts! (is-none (map-get? multiparty-parties { escrow-id: escrow-id, party: tx-sender })) ERR_PARTY_ALREADY_EXISTS)
    
    (try! (stx-transfer? contribution tx-sender (as-contract tx-sender)))
    
    (map-set multiparty-parties { escrow-id: escrow-id, party: tx-sender } {
      contribution: contribution,
      is-authorized: true,
      joined-at: current-block
    })
    
    (map-set multiparty-escrows escrow-id (merge escrow {
      total-amount: (+ (get total-amount escrow) contribution),
      available-amount: (+ (get available-amount escrow) contribution),
      party-count: (+ (get party-count escrow) u1)
    }))
    
    (ok true)
  )
)

(define-public (request-partial-release (escrow-id uint) (recipient principal) (amount uint))
  (let ((escrow (unwrap! (map-get? multiparty-escrows escrow-id) ERR_MULTIPARTY_NOT_FOUND))
        (party (unwrap! (map-get? multiparty-parties { escrow-id: escrow-id, party: tx-sender }) ERR_INVALID_PARTY))
        (release-id (var-get next-release-id))
        (current-block stacks-block-height))
    
    (asserts! (get is-authorized party) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status escrow) "active") ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_RELEASE_AMOUNT)
    (asserts! (<= amount (get available-amount escrow)) ERR_INSUFFICIENT_FUNDS)
    
    (map-set partial-releases release-id {
      escrow-id: escrow-id,
      recipient: recipient,
      amount: amount,
      requester: tx-sender,
      approvals: u1,
      executed: false,
      created-at: current-block
    })
    
    (map-set release-approvals { release-id: release-id, approver: tx-sender } {
      approved: true,
      approved-at: current-block
    })
    
    (var-set next-release-id (+ release-id u1))
    (ok release-id)
  )
)

(define-public (approve-partial-release (release-id uint))
  (let ((release (unwrap! (map-get? partial-releases release-id) ERR_MULTIPARTY_NOT_FOUND))
        (escrow-id (get escrow-id release))
        (escrow (unwrap! (map-get? multiparty-escrows escrow-id) ERR_MULTIPARTY_NOT_FOUND))
        (party (unwrap! (map-get? multiparty-parties { escrow-id: escrow-id, party: tx-sender }) ERR_INVALID_PARTY))
        (current-block stacks-block-height))
    
    (asserts! (get is-authorized party) ERR_NOT_AUTHORIZED)
    (asserts! (not (get executed release)) ERR_NOT_AUTHORIZED)
    (asserts! (is-none (map-get? release-approvals { release-id: release-id, approver: tx-sender })) ERR_ALREADY_VOTED)
    
    (map-set release-approvals { release-id: release-id, approver: tx-sender } {
      approved: true,
      approved-at: current-block
    })
    
    (let ((new-approvals (+ (get approvals release) u1)))
      (map-set partial-releases release-id (merge release { approvals: new-approvals }))
      
      (if (>= new-approvals (get approval-threshold escrow))
        (execute-partial-release release-id)
        (ok true)
      )
    )
  )
)

(define-public (execute-partial-release (release-id uint))
  (let ((release (unwrap! (map-get? partial-releases release-id) ERR_MULTIPARTY_NOT_FOUND))
        (escrow-id (get escrow-id release))
        (escrow (unwrap! (map-get? multiparty-escrows escrow-id) ERR_MULTIPARTY_NOT_FOUND)))
    
    (asserts! (not (get executed release)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get approvals release) (get approval-threshold escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (<= (get amount release) (get available-amount escrow)) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? (get amount release) tx-sender (get recipient release))))
    
    (map-set partial-releases release-id (merge release { executed: true }))
    (map-set multiparty-escrows escrow-id (merge escrow {
      available-amount: (- (get available-amount escrow) (get amount release))
    }))
    
    (ok true)
  )
)

(define-public (withdraw-from-multiparty (escrow-id uint))
  (let ((escrow (unwrap! (map-get? multiparty-escrows escrow-id) ERR_MULTIPARTY_NOT_FOUND))
        (party (unwrap! (map-get? multiparty-parties { escrow-id: escrow-id, party: tx-sender }) ERR_INVALID_PARTY))
        (current-block stacks-block-height))
    
    (asserts! (get is-authorized party) ERR_NOT_AUTHORIZED)
    (asserts! (> current-block (get expires-at escrow)) ERR_ESCROW_NOT_EXPIRED)
    (asserts! (is-eq (get status escrow) "active") ERR_NOT_AUTHORIZED)
    
    (let ((contribution (get contribution party))
          (proportion (/ (* contribution u10000) (get total-amount escrow)))
          (withdrawal-amount (/ (* (get available-amount escrow) proportion) u10000)))
      
      (asserts! (> withdrawal-amount u0) ERR_INSUFFICIENT_FUNDS)
      
      (try! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)))
      
      (map-set multiparty-parties { escrow-id: escrow-id, party: tx-sender } 
        (merge party { is-authorized: false }))
      
      (map-set multiparty-escrows escrow-id (merge escrow {
        available-amount: (- (get available-amount escrow) withdrawal-amount),
        party-count: (- (get party-count escrow) u1)
      }))
      
      (ok withdrawal-amount)
    )
  )
)

(define-public (contribute-to-insurance-pool (amount uint))
  (let ((current-contributor (default-to { contribution: u0, rewards-earned: u0, joined-at: u0, is-active: false }
                                         (map-get? insurance-contributors tx-sender)))
        (current-block stacks-block-height))
    (asserts! (>= amount MIN_INSURANCE_CONTRIBUTION) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (if (get is-active current-contributor)
      (map-set insurance-contributors tx-sender (merge current-contributor {
        contribution: (+ (get contribution current-contributor) amount)
      }))
      (begin
        (map-set insurance-contributors tx-sender {
          contribution: amount,
          rewards-earned: u0,
          joined-at: current-block,
          is-active: true
        })
        (var-set total-contributors (+ (var-get total-contributors) u1))
      )
    )
    
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) amount))
    (ok true)
  )
)

(define-public (purchase-escrow-insurance (escrow-id uint) (coverage-amount uint))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
        (premium (/ (* coverage-amount INSURANCE_PREMIUM_RATE) u100))
        (max-coverage (/ (* (get amount escrow) MAX_COVERAGE_PERCENTAGE) u100))
        (current-block stacks-block-height))
    
    (asserts! (is-eq tx-sender (get buyer escrow)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status escrow) "active") ERR_NOT_AUTHORIZED)
    (asserts! (> coverage-amount u0) ERR_INVALID_COVERAGE_AMOUNT)
    (asserts! (<= coverage-amount max-coverage) ERR_INVALID_COVERAGE_AMOUNT)
    (asserts! (is-none (map-get? escrow-insurance escrow-id)) ERR_ESCROW_ALREADY_EXISTS)
    
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    
    (map-set escrow-insurance escrow-id {
      coverage-amount: coverage-amount,
      premium-paid: premium,
      is-covered: true,
      coverage-activated-at: current-block
    })
    
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) premium))
    (ok true)
  )
)

(define-public (submit-insurance-claim (escrow-id uint) (claim-amount uint) (reason (string-ascii 500)))
  (let ((escrow (unwrap! (map-get? escrows escrow-id) ERR_ESCROW_NOT_FOUND))
        (insurance (unwrap! (map-get? escrow-insurance escrow-id) ERR_INSURANCE_NOT_FOUND))
        (claim-id (var-get next-claim-id))
        (current-block stacks-block-height))
    
    (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) ERR_NOT_AUTHORIZED)
    (asserts! (get is-covered insurance) ERR_CLAIM_NOT_ELIGIBLE)
    (asserts! (> claim-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= claim-amount (get coverage-amount insurance)) ERR_INVALID_COVERAGE_AMOUNT)
    (asserts! (or (is-eq (get status escrow) "disputed") (is-eq (get status escrow) "resolved")) ERR_CLAIM_NOT_ELIGIBLE)
    (asserts! (is-none (map-get? insurance-claims claim-id)) ERR_CLAIM_ALREADY_EXISTS)
    
    (map-set insurance-claims claim-id {
      escrow-id: escrow-id,
      claimant: tx-sender,
      claim-amount: claim-amount,
      reason: reason,
      submitted-at: current-block,
      status: "pending",
      validator-votes: u0,
      approved-votes: u0,
      processed-at: none
    })
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (validate-insurance-claim (claim-id uint) (approve bool))
  (let ((claim (unwrap! (map-get? insurance-claims claim-id) ERR_CLAIM_NOT_FOUND))
        (judge (unwrap! (map-get? judges tx-sender) ERR_NOT_AUTHORIZED))
        (current-block stacks-block-height))
    
    (asserts! (get is-active judge) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status claim) "pending") ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (is-none (map-get? claim-validations { claim-id: claim-id, validator: tx-sender })) ERR_ALREADY_VOTED)
    
    (map-set claim-validations { claim-id: claim-id, validator: tx-sender } {
      approved: approve,
      validated-at: current-block
    })
    
    (let ((new-validator-votes (+ (get validator-votes claim) u1))
          (new-approved-votes (if approve (+ (get approved-votes claim) u1) (get approved-votes claim))))
      
      (map-set insurance-claims claim-id (merge claim {
        validator-votes: new-validator-votes,
        approved-votes: new-approved-votes
      }))
      
      (if (>= new-validator-votes u3)
        (if (> new-approved-votes (/ new-validator-votes u2))
          (process-insurance-claim claim-id)
          (begin
            (map-set insurance-claims claim-id (merge claim { status: "rejected", processed-at: (some current-block) }))
            (ok false)
          )
        )
        (ok true)
      )
    )
  )
)

(define-public (process-insurance-claim (claim-id uint))
  (let ((claim (unwrap! (map-get? insurance-claims claim-id) ERR_CLAIM_NOT_FOUND))
        (current-pool-balance (var-get insurance-pool-balance)))
    
    (asserts! (is-eq (get status claim) "pending") ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (> (get approved-votes claim) (/ (get validator-votes claim) u2)) ERR_NOT_AUTHORIZED)
    (asserts! (<= (get claim-amount claim) current-pool-balance) ERR_INSUFFICIENT_POOL_FUNDS)
    
    (try! (as-contract (stx-transfer? (get claim-amount claim) tx-sender (get claimant claim))))
    
    (map-set insurance-claims claim-id (merge claim {
      status: "approved",
      processed-at: (some stacks-block-height)
    }))
    
    (var-set insurance-pool-balance (- current-pool-balance (get claim-amount claim)))
    (ok true)
  )
)

(define-public (withdraw-insurance-contribution (percentage uint))
  (let ((contributor (unwrap! (map-get? insurance-contributors tx-sender) ERR_CONTRIBUTOR_NOT_FOUND))
        (current-pool-balance (var-get insurance-pool-balance))
        (total-contribution (get contribution contributor)))
    
    (asserts! (get is-active contributor) ERR_NOT_AUTHORIZED)
    (asserts! (> percentage u0) ERR_INVALID_AMOUNT)
    (asserts! (<= percentage u100) ERR_INVALID_AMOUNT)
    
    (let ((withdrawal-amount (/ (* total-contribution percentage) u100))
          (pool-share (if (> current-pool-balance u0) (/ (* withdrawal-amount u10000) current-pool-balance) u0))
          (available-withdrawal (/ (* current-pool-balance pool-share) u10000)))
      
      (asserts! (> available-withdrawal u0) ERR_INSUFFICIENT_FUNDS)
      (asserts! (<= available-withdrawal current-pool-balance) ERR_INSUFFICIENT_POOL_FUNDS)
      
      (try! (as-contract (stx-transfer? available-withdrawal tx-sender tx-sender)))
      
      (if (is-eq percentage u100)
        (begin
          (map-set insurance-contributors tx-sender (merge contributor { is-active: false }))
          (var-set total-contributors (- (var-get total-contributors) u1))
        )
        (map-set insurance-contributors tx-sender (merge contributor {
          contribution: (- total-contribution withdrawal-amount)
        }))
      )
      
      (var-set insurance-pool-balance (- current-pool-balance available-withdrawal))
      (ok available-withdrawal)
    )
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

(define-read-only (get-multiparty-escrow (escrow-id uint))
  (map-get? multiparty-escrows escrow-id)
)

(define-read-only (get-multiparty-party (escrow-id uint) (party principal))
  (map-get? multiparty-parties { escrow-id: escrow-id, party: party })
)

(define-read-only (get-partial-release (release-id uint))
  (map-get? partial-releases release-id)
)

(define-read-only (get-release-approval (release-id uint) (approver principal))
  (map-get? release-approvals { release-id: release-id, approver: approver })
)

(define-read-only (get-next-multiparty-id)
  (var-get next-multiparty-id)
)

(define-read-only (get-next-release-id)
  (var-get next-release-id)
)

(define-read-only (calculate-withdrawal-amount (escrow-id uint) (party principal))
  (match (map-get? multiparty-escrows escrow-id)
    escrow (match (map-get? multiparty-parties { escrow-id: escrow-id, party: party })
             party-data (if (and (get is-authorized party-data) (> (get total-amount escrow) u0))
                          (let ((contribution (get contribution party-data))
                                (proportion (/ (* contribution u10000) (get total-amount escrow))))
                            (some (/ (* (get available-amount escrow) proportion) u10000)))
                          none)
             none)
    none
  )
)

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool-balance)
)

(define-read-only (get-total-contributors)
  (var-get total-contributors)
)

(define-read-only (get-insurance-contributor (contributor principal))
  (map-get? insurance-contributors contributor)
)

(define-read-only (get-escrow-insurance (escrow-id uint))
  (map-get? escrow-insurance escrow-id)
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims claim-id)
)

(define-read-only (get-claim-validation (claim-id uint) (validator principal))
  (map-get? claim-validations { claim-id: claim-id, validator: validator })
)

(define-read-only (get-next-claim-id)
  (var-get next-claim-id)
)

(define-read-only (calculate-insurance-premium (coverage-amount uint))
  (/ (* coverage-amount INSURANCE_PREMIUM_RATE) u100)
)

(define-read-only (get-max-coverage-for-escrow (escrow-id uint))
  (match (map-get? escrows escrow-id)
    escrow (some (/ (* (get amount escrow) MAX_COVERAGE_PERCENTAGE) u100))
    none
  )
)



