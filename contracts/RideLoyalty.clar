;; Ride-Sharing Loyalty Rewards System
;; Rewards active users with points and STX tokens based on engagement

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-insufficient-points (err u202))
(define-constant err-invalid-tier (err u203))
(define-constant err-already-redeemed (err u204))
(define-constant err-insufficient-funds (err u205))
(define-constant err-invalid-referral (err u206))

;; Loyalty tiers
(define-constant bronze-threshold u100)
(define-constant silver-threshold u500)
(define-constant gold-threshold u1500)
(define-constant platinum-threshold u5000)

;; Point values
(define-constant points-per-ride u10)
(define-constant points-per-rating u5)
(define-constant referral-bonus u50)
(define-constant monthly-decay-rate u10) ;; 10% monthly decay

;; Data variables
(define-data-var reward-pool uint u0)
(define-data-var total-members uint u0)
(define-data-var redemption-counter uint u0)

;; Member loyalty data
(define-map loyalty-members
  principal
  {
    total-points: uint,
    current-tier: uint, ;; 1=Bronze, 2=Silver, 3=Gold, 4=Platinum
    rides-completed: uint,
    ratings-given: uint,
    referrals-made: uint,
    last-activity: uint,
    joined-at: uint
  }
)

;; Referral tracking
(define-map referrals
  { referrer: principal, referred: principal }
  {
    bonus-claimed: bool,
    referred-at: uint
  }
)

;; Redemption history
(define-map redemptions
  uint
  {
    member: principal,
    points-spent: uint,
    stx-received: uint,
    redeemed-at: uint
  }
)

;; Tier rewards configuration
(define-map tier-multipliers
  uint
  uint ;; Multiplier in basis points (100 = 1x, 150 = 1.5x)
)

;; Initialize tier multipliers
(map-set tier-multipliers u1 u100) ;; Bronze: 1x
(map-set tier-multipliers u2 u125) ;; Silver: 1.25x  
(map-set tier-multipliers u3 u150) ;; Gold: 1.5x
(map-set tier-multipliers u4 u200) ;; Platinum: 2x

;; Public functions

;; Join loyalty program
(define-public (join-loyalty-program)
  (let ((member tx-sender))
    (if (is-some (map-get? loyalty-members member))
      err-already-redeemed
      (begin
        (map-set loyalty-members member {
          total-points: u0,
          current-tier: u1,
          rides-completed: u0,
          ratings-given: u0,
          referrals-made: u0,
          last-activity: stacks-block-height,
          joined-at: stacks-block-height
        })
        (var-set total-members (+ (var-get total-members) u1))
        (ok true)
      )
    )
  )
)

;; Add points for completing a ride
(define-public (add-ride-points (member principal))
  (let ((member-data (unwrap! (map-get? loyalty-members member) err-not-found)))
    (let (
      (new-points (+ (get total-points member-data) points-per-ride))
      (new-rides (+ (get rides-completed member-data) u1))
      (multiplier (unwrap! (map-get? tier-multipliers (get current-tier member-data)) (err u207)))
      (bonus-points (/ (* points-per-ride multiplier) u100))
    )
      (map-set loyalty-members member
        (merge member-data {
          total-points: (+ new-points bonus-points),
          rides-completed: new-rides,
          last-activity: stacks-block-height,
          current-tier: (calculate-tier (+ new-points bonus-points))
        })
      )
      (ok bonus-points)
    )
  )
)

;; Add points for giving a rating
(define-public (add-rating-points (member principal))
  (let ((member-data (unwrap! (map-get? loyalty-members member) err-not-found)))
    (let (
      (new-points (+ (get total-points member-data) points-per-rating))
      (new-ratings (+ (get ratings-given member-data) u1))
    )
      (map-set loyalty-members member
        (merge member-data {
          total-points: new-points,
          ratings-given: new-ratings,
          last-activity: stacks-block-height,
          current-tier: (calculate-tier new-points)
        })
      )
      (ok points-per-rating)
    )
  )
)

;; Referral system - referred user joins
(define-public (join-with-referral (referrer principal))
  (let ((referred tx-sender))
    (asserts! (is-some (map-get? loyalty-members referrer)) err-not-found)
    (asserts! (not (is-eq referrer referred)) err-invalid-referral)
    
    ;; Join loyalty program
    (try! (join-loyalty-program))
    
    ;; Track referral
    (map-set referrals { referrer: referrer, referred: referred } {
      bonus-claimed: false,
      referred-at: stacks-block-height
    })
    
    (ok true)
  )
)

;; Claim referral bonus
(define-public (claim-referral-bonus (referred principal))
  (let (
    (referrer tx-sender)
    (referral-data (unwrap! (map-get? referrals { referrer: referrer, referred: referred }) err-not-found))
    (referrer-data (unwrap! (map-get? loyalty-members referrer) err-not-found))
  )
    (asserts! (not (get bonus-claimed referral-data)) err-already-redeemed)
    
    ;; Update referral as claimed
    (map-set referrals { referrer: referrer, referred: referred }
      (merge referral-data { bonus-claimed: true }))
    
    ;; Add bonus points to referrer
    (map-set loyalty-members referrer
      (merge referrer-data {
        total-points: (+ (get total-points referrer-data) referral-bonus),
        referrals-made: (+ (get referrals-made referrer-data) u1),
        current-tier: (calculate-tier (+ (get total-points referrer-data) referral-bonus))
      }))
    
    (ok referral-bonus)
  )
)

;; Redeem points for STX
(define-public (redeem-points (points-to-spend uint))
  (let (
    (member tx-sender)
    (member-data (unwrap! (map-get? loyalty-members member) err-not-found))
    (redemption-id (var-get redemption-counter))
    (stx-amount (/ points-to-spend u10)) ;; 10 points = 1 microSTX
  )
    (asserts! (>= (get total-points member-data) points-to-spend) err-insufficient-points)
    (asserts! (>= (var-get reward-pool) stx-amount) err-insufficient-funds)
    
    ;; Update member points
    (map-set loyalty-members member
      (merge member-data {
        total-points: (- (get total-points member-data) points-to-spend),
        current-tier: (calculate-tier (- (get total-points member-data) points-to-spend))
      }))
    
    ;; Transfer STX from reward pool
    (try! (as-contract (stx-transfer? stx-amount tx-sender member)))
    
    ;; Update reward pool
    (var-set reward-pool (- (var-get reward-pool) stx-amount))
    
    ;; Record redemption
    (map-set redemptions redemption-id {
      member: member,
      points-spent: points-to-spend,
      stx-received: stx-amount,
      redeemed-at: stacks-block-height
    })
    
    (var-set redemption-counter (+ redemption-id u1))
    (ok stx-amount)
  )
)

;; Admin function to fund reward pool
(define-public (fund-reward-pool (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set reward-pool (+ (var-get reward-pool) amount))
    (ok true)
  )
)

;; Private helper functions

;; Calculate tier based on points
(define-private (calculate-tier (points uint))
  (if (>= points platinum-threshold) u4
    (if (>= points gold-threshold) u3
      (if (>= points silver-threshold) u2
        u1)))
)

;; Read-only functions

(define-read-only (get-member-info (member principal))
  (map-get? loyalty-members member)
)

(define-read-only (get-member-tier-name (member principal))
  (let ((member-data (unwrap! (map-get? loyalty-members member) err-not-found)))
    (let ((tier (get current-tier member-data)))
      (ok (if (is-eq tier u1) "Bronze"
        (if (is-eq tier u2) "Silver"  
          (if (is-eq tier u3) "Gold"
            "Platinum"))))
    )
  )
)

(define-read-only (get-referral-info (referrer principal) (referred principal))
  (map-get? referrals { referrer: referrer, referred: referred })
)

(define-read-only (get-redemption (redemption-id uint))
  (map-get? redemptions redemption-id)
)

(define-read-only (get-program-stats)
  {
    total-members: (var-get total-members),
    reward-pool: (var-get reward-pool),
    total-redemptions: (var-get redemption-counter)
  }
)

(define-read-only (get-tier-requirements)
  {
    bronze: bronze-threshold,
    silver: silver-threshold,
    gold: gold-threshold,
    platinum: platinum-threshold
  }
)
