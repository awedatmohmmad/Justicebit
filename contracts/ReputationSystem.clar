;; Reputation System for Justicebit Platform
;; Manages comprehensive reputation scoring for all platform participants

(define-constant ERR-UNAUTHORIZED (err u300))
(define-constant ERR-USER-NOT-FOUND (err u301))
(define-constant ERR-INVALID-SCORE (err u302))
(define-constant ERR-REPUTATION-EXISTS (err u303))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u304))
(define-constant ERR-INVALID-CATEGORY (err u305))
(define-constant ERR-BADGE-NOT-FOUND (err u306))
(define-constant ERR-MILESTONE-NOT-FOUND (err u307))

;; Reputation scoring constants
(define-constant SUCCESSFUL_ESCROW_POINTS u10)
(define-constant DISPUTED_ESCROW_PENALTY u5)
(define-constant CORRECT_JUDGE_VOTE_POINTS u15)
(define-constant INCORRECT_JUDGE_VOTE_PENALTY u8)
(define-constant INSURANCE_CONTRIBUTION_POINTS u5)
(define-constant FALSE_CLAIM_PENALTY u20)
(define-constant MULTIPARTY_SUCCESS_POINTS u8)

;; Reputation tiers
(define-constant BRONZE_TIER u100)
(define-constant SILVER_TIER u500)
(define-constant GOLD_TIER u1500)
(define-constant PLATINUM_TIER u5000)

;; Maximum reputation boost percentage
(define-constant MAX_REPUTATION_BOOST u25)

;; Track user reputation profiles
(define-map user-reputation principal 
  {
    total-score: uint,
    escrow-score: uint,
    judge-score: uint,
    insurance-score: uint,
    tier-level: uint,
    total-transactions: uint,
    successful-transactions: uint,
    disputes-involved: uint,
    disputes-won: uint,
    last-updated: uint,
    reputation-locked: bool
  }
)

;; Track reputation events for transparency
(define-map reputation-events {user: principal, event-id: uint}
  {
    event-type: (string-ascii 50),
    score-change: int,
    escrow-id: (optional uint),
    block-height: uint,
    description: (string-ascii 200)
  }
)

;; Track next event ID per user
(define-map next-event-id principal uint)

;; Track reputation milestones
(define-map reputation-milestones uint
  {
    title: (string-ascii 100),
    description: (string-ascii 200),
    required-score: uint,
    category: (string-ascii 30),
    reward-multiplier: uint
  }
)

;; Track user milestone achievements
(define-map user-milestones {user: principal, milestone-id: uint}
  {
    achieved-at: uint,
    reward-claimed: bool
  }
)

;; Track reputation boosts (temporary bonuses)
(define-map reputation-boosts principal
  {
    boost-percentage: uint,
    expires-at: uint,
    reason: (string-ascii 100)
  }
)

;; Initialize default milestones
(define-private (init-milestones)
  (begin
    (map-set reputation-milestones u1 {
      title: "First Steps",
      description: "Complete your first successful escrow transaction",
      required-score: u10,
      category: "escrow",
      reward-multiplier: u110
    })
    (map-set reputation-milestones u2 {
      title: "Trusted Trader",
      description: "Achieve 100 reputation points in escrow transactions",
      required-score: u100,
      category: "escrow", 
      reward-multiplier: u105
    })
    (map-set reputation-milestones u3 {
      title: "Fair Judge",
      description: "Successfully judge 10 dispute cases correctly",
      required-score: u150,
      category: "judge",
      reward-multiplier: u120
    })
    (map-set reputation-milestones u4 {
      title: "Insurance Guardian",
      description: "Contribute significantly to the insurance pool",
      required-score: u200,
      category: "insurance",
      reward-multiplier: u115
    })
  )
)

;; Initialize user reputation profile
(define-public (initialize-user-reputation)
  (let (
    (caller tx-sender)
  )
    (asserts! (is-none (map-get? user-reputation caller)) ERR-REPUTATION-EXISTS)
    (map-set user-reputation caller {
      total-score: u0,
      escrow-score: u0,
      judge-score: u0,
      insurance-score: u0,
      tier-level: u0,
      total-transactions: u0,
      successful-transactions: u0,
      disputes-involved: u0,
      disputes-won: u0,
      last-updated: stacks-block-height,
      reputation-locked: false
    })
    (map-set next-event-id caller u1)
    (ok true)
  )
)

;; Record successful escrow completion
(define-public (record-successful-escrow (user principal) (escrow-id uint))
  (let (
    (user-rep (unwrap! (map-get? user-reputation user) ERR-USER-NOT-FOUND))
    (current-boost (get-reputation-boost user))
    (base-points SUCCESSFUL_ESCROW_POINTS)
    (actual-points (/ (* base-points (+ u100 current-boost)) u100))
  )
    ;; Verify caller is authorized (main contract)
    (asserts! (is-eq contract-caller .Justicebit) ERR-UNAUTHORIZED)
    (asserts! (not (get reputation-locked user-rep)) ERR-UNAUTHORIZED)
    
    ;; Update reputation
    (map-set user-reputation user (merge user-rep {
      total-score: (+ (get total-score user-rep) actual-points),
      escrow-score: (+ (get escrow-score user-rep) actual-points),
      total-transactions: (+ (get total-transactions user-rep) u1),
      successful-transactions: (+ (get successful-transactions user-rep) u1),
      last-updated: stacks-block-height,
      tier-level: (calculate-tier (+ (get total-score user-rep) actual-points))
    }))
    
    ;; Record event
    (record-reputation-event user "successful-escrow" (to-int actual-points) (some escrow-id) "Successfully completed escrow transaction")
    
    ;; Check for milestone achievements
    (check-milestone-achievements user)
    
    (ok actual-points)
  )
)

;; Record disputed escrow penalty
(define-public (record-dispute-penalty (user principal) (escrow-id uint) (won-dispute bool))
  (let (
    (user-rep (unwrap! (map-get? user-reputation user) ERR-USER-NOT-FOUND))
    (penalty-points (if won-dispute u0 DISPUTED_ESCROW_PENALTY))
    (updated-rep (merge user-rep {
      disputes-involved: (+ (get disputes-involved user-rep) u1),
      disputes-won: (if won-dispute (+ (get disputes-won user-rep) u1) (get disputes-won user-rep)),
      last-updated: stacks-block-height
    }))
  )
    ;; Verify caller is authorized
    (asserts! (is-eq contract-caller .Justicebit) ERR-UNAUTHORIZED)
    (asserts! (not (get reputation-locked user-rep)) ERR-UNAUTHORIZED)
    
    ;; Apply penalty if dispute was lost
    (if (not won-dispute)
      (map-set user-reputation user (merge updated-rep {
        total-score: (if (>= (get total-score updated-rep) penalty-points) 
                        (- (get total-score updated-rep) penalty-points) 
                        u0),
        tier-level: (calculate-tier (if (>= (get total-score updated-rep) penalty-points) 
                                      (- (get total-score updated-rep) penalty-points) 
                                      u0))
      }))
      (map-set user-reputation user updated-rep)
    )
    
    ;; Record event
    (if won-dispute
      (record-reputation-event user "dispute-won" 0 (some escrow-id) "Won dispute case")
      (record-reputation-event user "dispute-penalty" (to-int (- penalty-points)) (some escrow-id) "Lost dispute case")
    )
    
    (ok true)
  )
)

;; Record judge performance
(define-public (record-judge-performance (judge principal) (escrow-id uint) (correct-vote bool))
  (let (
    (judge-rep (unwrap! (map-get? user-reputation judge) ERR-USER-NOT-FOUND))
    (current-boost (get-reputation-boost judge))
    (points-change (if correct-vote 
                     (/ (* CORRECT_JUDGE_VOTE_POINTS (+ u100 current-boost)) u100)
                     INCORRECT_JUDGE_VOTE_PENALTY))
  )
    ;; Verify caller is authorized
    (asserts! (is-eq contract-caller .Justicebit) ERR-UNAUTHORIZED)
    (asserts! (not (get reputation-locked judge-rep)) ERR-UNAUTHORIZED)
    
    ;; Update judge reputation
    (if correct-vote
      (map-set user-reputation judge (merge judge-rep {
        total-score: (+ (get total-score judge-rep) points-change),
        judge-score: (+ (get judge-score judge-rep) points-change),
        last-updated: stacks-block-height,
        tier-level: (calculate-tier (+ (get total-score judge-rep) points-change))
      }))
      (map-set user-reputation judge (merge judge-rep {
        total-score: (if (>= (get total-score judge-rep) points-change) 
                        (- (get total-score judge-rep) points-change) 
                        u0),
        judge-score: (if (>= (get judge-score judge-rep) points-change) 
                        (- (get judge-score judge-rep) points-change) 
                        u0),
        last-updated: stacks-block-height,
        tier-level: (calculate-tier (if (>= (get total-score judge-rep) points-change) 
                                      (- (get total-score judge-rep) points-change) 
                                      u0))
      }))
    )
    
    ;; Record event
    (if correct-vote
      (record-reputation-event judge "correct-judgement" (to-int points-change) (some escrow-id) "Made correct judgment in dispute")
      (record-reputation-event judge "incorrect-judgement" (to-int (- points-change)) (some escrow-id) "Made incorrect judgment in dispute")
    )
    
    ;; Check milestones
    (check-milestone-achievements judge)
    
    (ok points-change)
  )
)

;; Record insurance contribution
(define-public (record-insurance-contribution (contributor principal) (amount uint))
  (let (
    (contrib-rep (unwrap! (map-get? user-reputation contributor) ERR-USER-NOT-FOUND))
    (current-boost (get-reputation-boost contributor))
    (base-points (/ amount u100)) ;; 1 point per 100 STX contributed
    (actual-points (/ (* base-points (+ u100 current-boost)) u100))
  )
    ;; Verify caller is authorized
    (asserts! (is-eq contract-caller .Justicebit) ERR-UNAUTHORIZED)
    (asserts! (not (get reputation-locked contrib-rep)) ERR-UNAUTHORIZED)
    
    ;; Update reputation
    (map-set user-reputation contributor (merge contrib-rep {
      total-score: (+ (get total-score contrib-rep) actual-points),
      insurance-score: (+ (get insurance-score contrib-rep) actual-points),
      last-updated: stacks-block-height,
      tier-level: (calculate-tier (+ (get total-score contrib-rep) actual-points))
    }))
    
    ;; Record event
    (record-reputation-event contributor "insurance-contribution" (to-int actual-points) none "Contributed to insurance pool")
    
    ;; Check milestones
    (check-milestone-achievements contributor)
    
    (ok actual-points)
  )
)

;; Apply reputation boost
(define-public (apply-reputation-boost (user principal) (boost-percentage uint) (duration-blocks uint) (reason (string-ascii 100)))
  (let (
    (user-rep (unwrap! (map-get? user-reputation user) ERR-USER-NOT-FOUND))
    (expires-at (+ stacks-block-height duration-blocks))
  )
    ;; Verify caller is authorized and boost is reasonable
    (asserts! (is-eq contract-caller .Justicebit) ERR-UNAUTHORIZED)
    (asserts! (<= boost-percentage MAX_REPUTATION_BOOST) ERR-INVALID-SCORE)
    
    (map-set reputation-boosts user {
      boost-percentage: boost-percentage,
      expires-at: expires-at,
      reason: reason
    })
    
    (record-reputation-event user "reputation-boost" (to-int boost-percentage) none reason)
    (ok true)
  )
)

;; Helper function to record reputation events
(define-private (record-reputation-event (user principal) (event-type (string-ascii 50)) (score-change int) (escrow-id (optional uint)) (description (string-ascii 200)))
  (let (
    (event-id (default-to u1 (map-get? next-event-id user)))
  )
    (map-set reputation-events {user: user, event-id: event-id} {
      event-type: event-type,
      score-change: score-change,
      escrow-id: escrow-id,
      block-height: stacks-block-height,
      description: description
    })
    (map-set next-event-id user (+ event-id u1))
  )
)

;; Calculate tier based on total score
(define-private (calculate-tier (total-score uint))
  (if (>= total-score PLATINUM_TIER)
    u4
    (if (>= total-score GOLD_TIER)
      u3
      (if (>= total-score SILVER_TIER)
        u2
        (if (>= total-score BRONZE_TIER)
          u1
          u0
        )
      )
    )
  )
)

;; Get current reputation boost percentage
(define-private (get-reputation-boost (user principal))
  (match (map-get? reputation-boosts user)
    boost (if (> (get expires-at boost) stacks-block-height)
            (get boost-percentage boost)
            u0)
    u0
  )
)

;; Check and award milestone achievements
(define-private (check-milestone-achievements (user principal))
  (let (
    (user-rep (unwrap-panic (map-get? user-reputation user)))
  )
    ;; Check milestone 1: First Steps
    (if (and (>= (get escrow-score user-rep) u10) 
             (is-none (map-get? user-milestones {user: user, milestone-id: u1})))
      (map-set user-milestones {user: user, milestone-id: u1} {
        achieved-at: stacks-block-height,
        reward-claimed: false
      })
      true
    )
    ;; Check milestone 2: Trusted Trader  
    (if (and (>= (get escrow-score user-rep) u100) 
             (is-none (map-get? user-milestones {user: user, milestone-id: u2})))
      (map-set user-milestones {user: user, milestone-id: u2} {
        achieved-at: stacks-block-height,
        reward-claimed: false
      })
      true
    )
    ;; Additional milestone checks could be added here
  )
)

;; Read-only functions
(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation user)
)

(define-read-only (get-reputation-event (user principal) (event-id uint))
  (map-get? reputation-events {user: user, event-id: event-id})
)

(define-read-only (get-tier-name (tier-level uint))
  (if (is-eq tier-level u4)
    "Platinum"
    (if (is-eq tier-level u3)
      "Gold"
      (if (is-eq tier-level u2)
        "Silver"
        (if (is-eq tier-level u1)
          "Bronze"
          "Unranked"
        )
      )
    )
  )
)

(define-read-only (get-reputation-boost-info (user principal))
  (map-get? reputation-boosts user)
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? reputation-milestones milestone-id)
)

(define-read-only (get-user-milestone (user principal) (milestone-id uint))
  (map-get? user-milestones {user: user, milestone-id: milestone-id})
)

(define-read-only (calculate-reputation-discount (user principal))
  (match (map-get? user-reputation user)
    user-rep (let (
      (tier (get tier-level user-rep))
      (boost (get-reputation-boost user))
    )
      (+ boost (if (is-eq tier u4) u20
                 (if (is-eq tier u3) u15
                   (if (is-eq tier u2) u10
                     (if (is-eq tier u1) u5 u0)
                   )
                 )
               ))
    )
    u0
  )
)

;; Initialize the contract
(init-milestones)
