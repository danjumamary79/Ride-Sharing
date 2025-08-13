;; title: Ride-Sharing
;; version: 1.0
;; summary: A decentralized ride-sharing platform on Stacks
;; description: Enables direct peer-to-peer ride-sharing with no middleman fees,
;; allowing drivers to set their own prices and build reputation.

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-ride-not-available (err u104))
(define-constant err-ride-already-started (err u105))
(define-constant err-ride-not-started (err u106))
(define-constant err-ride-already-completed (err u107))
(define-constant err-insufficient-funds (err u108))
(define-constant err-invalid-rating (err u109))
(define-constant err-ride-not-completed (err u110))
(define-constant err-not-rider (err u111))
(define-constant err-not-driver (err u112))

;; Data variables
(define-data-var platform-fee uint u5) ;; 5% platform fee

;; Data maps
;; Driver profiles
(define-map drivers 
  principal 
  {
    name: (string-ascii 50),
    vehicle-type: (string-ascii 20),
    license-plate: (string-ascii 10),
    active: bool,
    total-rides: uint,
    total-ratings: uint,
    rating-sum: uint,
    earnings: uint
  }
)

;; Rider profiles
(define-map riders
  principal
  {
    name: (string-ascii 50),
    total-rides: uint,
    total-ratings: uint,
    rating-sum: uint
  }
)

;; Available rides offered by drivers
(define-map available-rides
  uint ;; ride-id
  {
    driver: principal,
    start-location: (string-ascii 100),
    end-location: (string-ascii 100),
    price: uint, ;; in STX
    seats-available: uint,
    timestamp: uint
  }
)

;; Active rides that have been booked
(define-map active-rides
  uint ;; ride-id
  {
    driver: principal,
    rider: principal,
    start-location: (string-ascii 100),
    end-location: (string-ascii 100),
    price: uint,
    started: bool,
    completed: bool,
    timestamp: uint
  }
)

;; Ride counter for generating unique ride IDs
(define-data-var ride-counter uint u0)

;; Public functions

;; Register as a driver
(define-public (register-driver (name (string-ascii 50)) (vehicle-type (string-ascii 20)) (license-plate (string-ascii 10)))
  (let ((driver tx-sender))
    (if (is-some (map-get? drivers driver))
      err-already-exists
      (ok (map-set drivers driver {
        name: name,
        vehicle-type: vehicle-type,
        license-plate: license-plate,
        active: true,
        total-rides: u0,
        total-ratings: u0,
        rating-sum: u0,
        earnings: u0
      }))
    )
  )
)

;; Register as a rider
(define-public (register-rider (name (string-ascii 50)))
  (let ((rider tx-sender))
    (if (is-some (map-get? riders rider))
      err-already-exists
      (ok (map-set riders rider {
        name: name,
        total-rides: u0,
        total-ratings: u0,
        rating-sum: u0
      }))
    )
  )
)

;; Create a new ride offer
(define-public (create-ride (start-location (string-ascii 100)) (end-location (string-ascii 100)) (price uint) (seats-available uint))
  (let (
    (driver tx-sender)
    (ride-id (var-get ride-counter))
  )
    (asserts! (is-some (map-get? drivers driver)) err-not-found)
    (var-set ride-counter (+ ride-id u1))
    (ok (map-set available-rides ride-id {
      driver: driver,
      start-location: start-location,
      end-location: end-location,
      price: price,
      seats-available: seats-available,
      timestamp: stacks-block-height
    }))
  )
)

;; Book a ride
(define-public (book-ride (ride-id uint))
  (let (
    (rider tx-sender)
    (ride (unwrap! (map-get? available-rides ride-id) err-not-found))
    (price (get price ride))
  )
    (asserts! (is-some (map-get? riders rider)) err-not-found)
    (asserts! (> (get seats-available ride) u0) err-ride-not-available)
    
    ;; Transfer payment to escrow (contract)
    (try! (stx-transfer? price rider (as-contract tx-sender)))
    
    ;; Create active ride
    (map-set active-rides ride-id {
      driver: (get driver ride),
      rider: rider,
      start-location: (get start-location ride),
      end-location: (get end-location ride),
      price: price,
      started: false,
      completed: false,
      timestamp: stacks-block-height
    })
    
    ;; Update available seats or remove if no more seats
    (if (> (get seats-available ride) u1)
      (map-set available-rides ride-id 
        (merge ride { seats-available: (- (get seats-available ride) u1) }))
      (map-delete available-rides ride-id)
    )
    
    (ok true)
  )
)

;; Start ride (driver confirms pickup)
(define-public (start-ride (ride-id uint))
  (let (
    (driver tx-sender)
    (ride (unwrap! (map-get? active-rides ride-id) err-not-found))
  )
    (asserts! (is-eq driver (get driver ride)) err-not-driver)
    (asserts! (not (get started ride)) err-ride-already-started)
    (asserts! (not (get completed ride)) err-ride-already-completed)
    
    (ok (map-set active-rides ride-id 
      (merge ride { started: true })))
  )
)

;; Complete ride (driver confirms dropoff)
(define-public (complete-ride (ride-id uint))
  (let (
    (driver tx-sender)
    (ride (unwrap! (map-get? active-rides ride-id) err-not-found))
  )
    (asserts! (is-eq driver (get driver ride)) err-not-driver)
    (asserts! (get started ride) err-ride-not-started)
    (asserts! (not (get completed ride)) err-ride-already-completed)
    
    (ok (map-set active-rides ride-id 
      (merge ride { completed: true })))
  )
)

;; Rate driver (by rider)
(define-public (rate-driver (ride-id uint) (rating uint))
  (let (
    (rider tx-sender)
    (ride (unwrap! (map-get? active-rides ride-id) err-not-found))
    (driver (get driver ride))
    (driver-data (unwrap! (map-get? drivers driver) err-not-found))
  )
    (asserts! (is-eq rider (get rider ride)) err-not-rider)
    (asserts! (get completed ride) err-ride-not-completed)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    
    (ok (map-set drivers driver 
      (merge driver-data {
        total-ratings: (+ (get total-ratings driver-data) u1),
        rating-sum: (+ (get rating-sum driver-data) rating)
      })))
  )
)

;; Rate rider (by driver)
(define-public (rate-rider (ride-id uint) (rating uint))
  (let (
    (driver tx-sender)
    (ride (unwrap! (map-get? active-rides ride-id) err-not-found))
    (rider (get rider ride))
    (rider-data (unwrap! (map-get? riders rider) err-not-found))
  )
    (asserts! (is-eq driver (get driver ride)) err-not-driver)
    (asserts! (get completed ride) err-ride-not-completed)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    
    (ok (map-set riders rider 
      (merge rider-data {
        total-ratings: (+ (get total-ratings rider-data) u1),
        rating-sum: (+ (get rating-sum rider-data) rating)
      })))
  )
)

;; Release payment after ride completion
(define-public (release-payment (ride-id uint))
  (let (
    (rider tx-sender)
    (ride (unwrap! (map-get? active-rides ride-id) err-not-found))
    (driver (get driver ride))
    (price (get price ride))
    (driver-data (unwrap! (map-get? drivers driver) err-not-found))
    (platform-fee-amount (/ (* price (var-get platform-fee)) u100))
    (driver-amount (- price platform-fee-amount))
  )
    (asserts! (is-eq rider (get rider ride)) err-not-rider)
    (asserts! (get completed ride) err-ride-not-completed)
    
    ;; Transfer payment from escrow to driver
    (try! (as-contract (stx-transfer? driver-amount tx-sender driver)))
    
    ;; Transfer platform fee to contract owner
    (try! (as-contract (stx-transfer? platform-fee-amount tx-sender contract-owner)))
    
    ;; Update driver earnings
    (map-set drivers driver 
      (merge driver-data {
        total-rides: (+ (get total-rides driver-data) u1),
        earnings: (+ (get earnings driver-data) driver-amount)
      }))
    
    ;; Update rider stats
    (let ((rider-data (unwrap! (map-get? riders rider) err-not-found)))
      (map-set riders rider 
        (merge rider-data {
          total-rides: (+ (get total-rides rider-data) u1)
        }))
    )
    
    (ok true)
  )
)

;; Cancel ride (by rider before it starts)
(define-public (cancel-ride (ride-id uint))
  (let (
    (rider tx-sender)
    (ride (unwrap! (map-get? active-rides ride-id) err-not-found))
    (price (get price ride))
  )
    (asserts! (is-eq rider (get rider ride)) err-not-rider)
    (asserts! (not (get started ride)) err-ride-already-started)
    
    ;; Refund payment to rider
    (try! (as-contract (stx-transfer? price tx-sender rider)))
    
    ;; Delete the active ride
    (map-delete active-rides ride-id)
    
    ;; Add back to available rides
    (map-set available-rides ride-id {
      driver: (get driver ride),
      start-location: (get start-location ride),
      end-location: (get end-location ride),
      price: price,
      seats-available: u1,
      timestamp: stacks-block-height
    })
    
    (ok true)
  )
)

;; Read-only functions

;; Get driver profile
(define-read-only (get-driver-profile (driver principal))
  (map-get? drivers driver)
)

;; Get rider profile
(define-read-only (get-rider-profile (rider principal))
  (map-get? riders rider)
)

;; Get ride details
(define-read-only (get-ride-details (ride-id uint))
  (map-get? active-rides ride-id)
)

;; Get available ride details
(define-read-only (get-available-ride (ride-id uint))
  (map-get? available-rides ride-id)
)

;; Calculate driver rating
(define-read-only (get-driver-rating (driver principal))
  (let ((driver-data (unwrap! (map-get? drivers driver) err-not-found)))
    (if (> (get total-ratings driver-data) u0)
      (ok (/ (get rating-sum driver-data) (get total-ratings driver-data)))
      (ok u0)
    )
  )
)

;; Calculate rider rating
(define-read-only (get-rider-rating (rider principal))
  (let ((rider-data (unwrap! (map-get? riders rider) err-not-found)))
    (if (> (get total-ratings rider-data) u0)
      (ok (/ (get rating-sum rider-data) (get total-ratings rider-data)))
      (ok u0)
    )
  )
)

;; Admin functions

;; Update platform fee
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u20) (err u113)) ;; Max 20% fee
    (ok (var-set platform-fee new-fee))
  )
)



(define-map driver-schedules
  { driver: principal, day: uint }
  {
    start-hour: uint,
    end-hour: uint,
    max-rides: uint
  }
)

(define-public (set-schedule (day uint) (start-hour uint) (end-hour uint) (max-rides uint))
  (let ((driver tx-sender))
    (asserts! (is-some (map-get? drivers driver)) err-not-found)
    (asserts! (and (>= day u1) (<= day u7)) (err u200))
    (asserts! (and (>= start-hour u0) (< start-hour u24)) (err u201))
    (asserts! (and (>= end-hour u0) (< end-hour u24)) (err u202))
    (asserts! (> end-hour start-hour) (err u203))
    
    (ok (map-set driver-schedules 
      { driver: driver, day: day }
      {
        start-hour: start-hour,
        end-hour: end-hour,
        max-rides: max-rides
      }
    ))
  )
)

(define-read-only (get-driver-schedule (driver principal) (day uint))
  (map-get? driver-schedules { driver: driver, day: day })
)


(define-map emergency-contacts
  principal
  {
    contact1: principal,
    contact2: principal,
    contact-count: uint
  }
)

(define-map emergency-alerts
  uint
  {
    ride-id: uint,
    rider: principal,
    driver: principal,
    timestamp: uint,
    location: (string-ascii 100),
    resolved: bool
  }
)

(define-data-var alert-counter uint u0)

(define-public (add-emergency-contact (contact principal))
  (let (
    (user tx-sender)
    (current-contacts (default-to 
      { contact1: user, contact2: user, contact-count: u0 }
      (map-get? emergency-contacts user)
    ))
  )
    (asserts! (< (get contact-count current-contacts) u2) (err u300))
    (ok (map-set emergency-contacts user
      (merge current-contacts 
        {
          contact1: (if (is-eq (get contact-count current-contacts) u0)
            contact
            (get contact1 current-contacts)),
          contact2: (if (is-eq (get contact-count current-contacts) u1)
            contact
            (get contact2 current-contacts)),
          contact-count: (+ (get contact-count current-contacts) u1)
        }
      )
    ))
  )
)

(define-public (trigger-emergency-alert (ride-id uint) (location (string-ascii 100)))
  (let (
    (rider tx-sender)
    (ride (unwrap! (map-get? active-rides ride-id) err-not-found))
    (alert-id (var-get alert-counter))
  )
    (asserts! (is-eq rider (get rider ride)) err-not-rider)
    (var-set alert-counter (+ alert-id u1))
    (ok (map-set emergency-alerts alert-id
      {
        ride-id: ride-id,
        rider: rider,
        driver: (get driver ride),
        timestamp: stacks-block-height,
        location: location,
        resolved: false
      }
    ))
  )
)

(define-map pricing-zones
  uint
  {
    zone-name: (string-ascii 50),
    base-multiplier: uint,
    current-demand: uint,
    active-drivers: uint,
    last-updated: uint
  }
)

(define-map time-multipliers
  uint
  uint
)

(define-map weather-multipliers
  (string-ascii 20)
  uint
)

(define-data-var zone-counter uint u0)

(define-public (create-pricing-zone (zone-name (string-ascii 50)) (base-multiplier uint))
  (let ((zone-id (var-get zone-counter)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set zone-counter (+ zone-id u1))
    (ok (map-set pricing-zones zone-id
      {
        zone-name: zone-name,
        base-multiplier: base-multiplier,
        current-demand: u100,
        active-drivers: u0,
        last-updated: stacks-block-height
      }
    ))
  )
)

(define-public (set-time-multiplier (hour uint) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (< hour u24) (err u400))
    (asserts! (and (>= multiplier u50) (<= multiplier u300)) (err u401))
    (ok (map-set time-multipliers hour multiplier))
  )
)

(define-public (set-weather-multiplier (weather-condition (string-ascii 20)) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= multiplier u100) (<= multiplier u250)) (err u402))
    (ok (map-set weather-multipliers weather-condition multiplier))
  )
)

(define-public (update-zone-demand (zone-id uint) (demand-level uint))
  (let ((zone (unwrap! (map-get? pricing-zones zone-id) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= demand-level u50) (<= demand-level u300)) (err u403))
    (ok (map-set pricing-zones zone-id
      (merge zone 
        {
          current-demand: demand-level,
          last-updated: stacks-block-height
        }
      )
    ))
  )
)

(define-public (register-driver-in-zone (zone-id uint))
  (let (
    (driver tx-sender)
    (zone (unwrap! (map-get? pricing-zones zone-id) err-not-found))
  )
    (asserts! (is-some (map-get? drivers driver)) err-not-found)
    (ok (map-set pricing-zones zone-id
      (merge zone 
        {
          active-drivers: (+ (get active-drivers zone) u1),
          last-updated: stacks-block-height
        }
      )
    ))
  )
)

(define-public (create-dynamic-ride (start-location (string-ascii 100)) (end-location (string-ascii 100)) (base-price uint) (seats-available uint) (zone-id uint) (current-hour uint) (weather-condition (string-ascii 20)))
  (let (
    (driver tx-sender)
    (ride-id (var-get ride-counter))
    (final-price (unwrap! (calculate-dynamic-price base-price zone-id current-hour weather-condition) (err u404)))
  )
    (asserts! (is-some (map-get? drivers driver)) err-not-found)
    (asserts! (< current-hour u24) (err u405))
    (var-set ride-counter (+ ride-id u1))
    (ok (map-set available-rides ride-id {
      driver: driver,
      start-location: start-location,
      end-location: end-location,
      price: final-price,
      seats-available: seats-available,
      timestamp: stacks-block-height
    }))
  )
)

(define-read-only (calculate-dynamic-price (base-price uint) (zone-id uint) (current-hour uint) (weather-condition (string-ascii 20)))
  (let (
    (zone (unwrap! (map-get? pricing-zones zone-id) err-not-found))
    (time-mult (default-to u100 (map-get? time-multipliers current-hour)))
    (weather-mult (default-to u100 (map-get? weather-multipliers weather-condition)))
    (demand-mult (get current-demand zone))
    (supply-mult (if (> (get active-drivers zone) u0)
      (/ u10000 (+ (get active-drivers zone) u1))
      u150))
  )
    (let (
      (zone-adjusted (/ (* base-price (get base-multiplier zone)) u100))
      (time-adjusted (/ (* zone-adjusted time-mult) u100))
      (weather-adjusted (/ (* time-adjusted weather-mult) u100))
      (demand-adjusted (/ (* weather-adjusted demand-mult) u100))
      (final-price (/ (* demand-adjusted supply-mult) u100))
    )
      (ok final-price)
    )
  )
)

(define-read-only (get-pricing-zone (zone-id uint))
  (map-get? pricing-zones zone-id)
)

(define-read-only (get-current-multipliers (zone-id uint) (current-hour uint) (weather-condition (string-ascii 20)))
  (let (
    (zone (unwrap! (map-get? pricing-zones zone-id) err-not-found))
    (time-mult (default-to u100 (map-get? time-multipliers current-hour)))
    (weather-mult (default-to u100 (map-get? weather-multipliers weather-condition)))
  )
    (ok {
      zone-multiplier: (get base-multiplier zone),
      time-multiplier: time-mult,
      weather-multiplier: weather-mult,
      demand-multiplier: (get current-demand zone),
      supply-factor: (if (> (get active-drivers zone) u0)
        (/ u10000 (+ (get active-drivers zone) u1))
        u150)
    })
  )
)

(define-read-only (preview-dynamic-price (base-price uint) (zone-id uint) (current-hour uint) (weather-condition (string-ascii 20)))
  (calculate-dynamic-price base-price zone-id current-hour weather-condition)
)

;; Community Dispute Resolution System

;; Dispute constants
(define-constant err-dispute-not-found (err u600))
(define-constant err-not-dispute-participant (err u601))
(define-constant err-dispute-already-exists (err u602))
(define-constant err-dispute-already-resolved (err u603))
(define-constant err-not-arbitrator (err u604))
(define-constant err-arbitrator-already-voted (err u605))
(define-constant err-insufficient-stake (err u606))
(define-constant err-invalid-dispute-type (err u607))
(define-constant err-dispute-period-expired (err u608))
(define-constant err-not-enough-arbitrators (err u609))

;; Dispute data structures
(define-map disputes
  uint ;; dispute-id
  {
    ride-id: uint,
    complainant: principal, ;; who filed the dispute
    defendant: principal, ;; who the dispute is against
    dispute-type: uint, ;; 1=payment, 2=service, 3=safety, 4=other
    description: (string-ascii 200),
    amount-disputed: uint,
    filed-at: uint,
    resolved: bool,
    resolution: uint, ;; 0=pending, 1=favor-complainant, 2=favor-defendant, 3=split
    total-votes: uint,
    votes-for-complainant: uint,
    votes-for-defendant: uint,
    assigned-arbitrators: (list 3 principal),
    arbitrator-count: uint
  }
)

;; Arbitrator registration and qualifications
(define-map arbitrators
  principal
  {
    registered-at: uint,
    total-cases: uint,
    correct-votes: uint,
    reputation-score: uint,
    stake-amount: uint,
    active: bool,
    specialization: uint ;; 1=payment, 2=service, 3=safety, 4=general
  }
)

;; Individual arbitrator votes for disputes
(define-map arbitrator-votes
  { dispute-id: uint, arbitrator: principal }
  {
    vote: uint, ;; 1=favor-complainant, 2=favor-defendant
    reasoning: (string-ascii 150),
    voted-at: uint
  }
)

;; Evidence submitted for disputes
(define-map dispute-evidence
  { dispute-id: uint, submitter: principal }
  {
    evidence-type: uint, ;; 1=description, 2=witness, 3=photo-hash
    content: (string-ascii 200),
    submitted-at: uint
  }
)

;; Counters and settings
(define-data-var dispute-counter uint u0)
(define-data-var arbitrator-stake-required uint u1000000) ;; 1 STX in microSTX
(define-data-var dispute-filing-fee uint u100000) ;; 0.1 STX
(define-data-var arbitrator-reward uint u50000) ;; 0.05 STX per case
(define-data-var dispute-period-blocks uint u1008) ;; ~7 days

;; Register as an arbitrator
(define-public (register-arbitrator (specialization uint))
  (let ((arbitrator tx-sender))
    (asserts! (is-none (map-get? arbitrators arbitrator)) err-already-exists)
    (asserts! (and (>= specialization u1) (<= specialization u4)) err-invalid-dispute-type)
    
    ;; Require stake to become arbitrator
    (try! (stx-transfer? (var-get arbitrator-stake-required) arbitrator (as-contract tx-sender)))
    
    (ok (map-set arbitrators arbitrator {
      registered-at: stacks-block-height,
      total-cases: u0,
      correct-votes: u0,
      reputation-score: u100, ;; Start with 100 reputation
      stake-amount: (var-get arbitrator-stake-required),
      active: true,
      specialization: specialization
    }))
  )
)

;; File a dispute about a ride
(define-public (file-dispute (ride-id uint) (defendant principal) (dispute-type uint) (description (string-ascii 200)) (amount-disputed uint))
  (let (
    (complainant tx-sender)
    (dispute-id (var-get dispute-counter))
    (ride (unwrap! (map-get? active-rides ride-id) err-not-found))
  )
    ;; Verify complainant is part of the ride
    (asserts! (or (is-eq complainant (get rider ride)) (is-eq complainant (get driver ride))) err-not-dispute-participant)
    ;; Verify defendant is the other party
    (asserts! (or (is-eq defendant (get rider ride)) (is-eq defendant (get driver ride))) err-not-dispute-participant)
    (asserts! (not (is-eq complainant defendant)) err-not-dispute-participant)
    (asserts! (and (>= dispute-type u1) (<= dispute-type u4)) err-invalid-dispute-type)
    
    ;; Simplified check - in production would check against existing disputes
    (asserts! (< dispute-id u99999) err-dispute-already-exists)
    
    ;; Charge filing fee
    (try! (stx-transfer? (var-get dispute-filing-fee) complainant (as-contract tx-sender)))
    
    (var-set dispute-counter (+ dispute-id u1))
    
    (ok (map-set disputes dispute-id {
      ride-id: ride-id,
      complainant: complainant,
      defendant: defendant,
      dispute-type: dispute-type,
      description: description,
      amount-disputed: amount-disputed,
      filed-at: stacks-block-height,
      resolved: false,
      resolution: u0,
      total-votes: u0,
      votes-for-complainant: u0,
      votes-for-defendant: u0,
      assigned-arbitrators: (list),
      arbitrator-count: u0
    }))
  )
)

;; Submit evidence for a dispute
(define-public (submit-evidence (dispute-id uint) (evidence-type uint) (content (string-ascii 200)))
  (let (
    (submitter tx-sender)
    (dispute (unwrap! (map-get? disputes dispute-id) err-dispute-not-found))
  )
    (asserts! (or (is-eq submitter (get complainant dispute)) (is-eq submitter (get defendant dispute))) err-not-dispute-participant)
    (asserts! (not (get resolved dispute)) err-dispute-already-resolved)
    (asserts! (and (>= evidence-type u1) (<= evidence-type u3)) (err u610))
    
    (ok (map-set dispute-evidence { dispute-id: dispute-id, submitter: submitter } {
      evidence-type: evidence-type,
      content: content,
      submitted-at: stacks-block-height
    }))
  )
)

;; Assign arbitrators to a dispute (called by contract owner or automated)
(define-public (assign-arbitrators (dispute-id uint) (arbitrator1 principal) (arbitrator2 principal) (arbitrator3 principal))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) err-dispute-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get resolved dispute)) err-dispute-already-resolved)
    (asserts! (is-eq (get arbitrator-count dispute) u0) (err u611))
    
    ;; Verify all are registered arbitrators
    (asserts! (is-some (map-get? arbitrators arbitrator1)) err-not-arbitrator)
    (asserts! (is-some (map-get? arbitrators arbitrator2)) err-not-arbitrator)
    (asserts! (is-some (map-get? arbitrators arbitrator3)) err-not-arbitrator)
    
    (let ((assigned-list (list arbitrator1 arbitrator2 arbitrator3)))
      (ok (map-set disputes dispute-id 
        (merge dispute {
          assigned-arbitrators: assigned-list,
          arbitrator-count: u3
        })))
    )
  )
)

;; Vote on a dispute as an arbitrator
(define-public (vote-on-dispute (dispute-id uint) (vote uint) (reasoning (string-ascii 150)))
  (let (
    (arbitrator tx-sender)
    (dispute (unwrap! (map-get? disputes dispute-id) err-dispute-not-found))
  )
    (asserts! (not (get resolved dispute)) err-dispute-already-resolved)
    (asserts! (and (>= vote u1) (<= vote u2)) (err u612))
    (asserts! (is-some (index-of? (get assigned-arbitrators dispute) arbitrator)) err-not-arbitrator)
    (asserts! (is-none (map-get? arbitrator-votes { dispute-id: dispute-id, arbitrator: arbitrator })) err-arbitrator-already-voted)
    
    ;; Record the vote
    (map-set arbitrator-votes { dispute-id: dispute-id, arbitrator: arbitrator } {
      vote: vote,
      reasoning: reasoning,
      voted-at: stacks-block-height
    })
    
    ;; Update dispute vote counts
    (let (
      (new-total-votes (+ (get total-votes dispute) u1))
      (new-complainant-votes (if (is-eq vote u1) (+ (get votes-for-complainant dispute) u1) (get votes-for-complainant dispute)))
      (new-defendant-votes (if (is-eq vote u2) (+ (get votes-for-defendant dispute) u1) (get votes-for-defendant dispute)))
    )
      (map-set disputes dispute-id 
        (merge dispute {
          total-votes: new-total-votes,
          votes-for-complainant: new-complainant-votes,
          votes-for-defendant: new-defendant-votes
        }))
      
      ;; Check if we have majority and resolve
      (if (>= new-total-votes u2)
        (auto-resolve-dispute dispute-id)
        (ok true)
      )
    )
  )
)

;; Automatically resolve dispute based on votes
(define-private (auto-resolve-dispute (dispute-id uint))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) err-dispute-not-found)))
    (let (
      (complainant-votes (get votes-for-complainant dispute))
      (defendant-votes (get votes-for-defendant dispute))
      (resolution (if (> complainant-votes defendant-votes) u1 
                    (if (> defendant-votes complainant-votes) u2 u3)))
    )
      ;; Update dispute as resolved
      (map-set disputes dispute-id 
        (merge dispute { 
          resolved: true,
          resolution: resolution
        }))
      
      ;; Distribute compensation based on resolution
      (try! (distribute-dispute-compensation dispute-id resolution))
      
      ;; Reward arbitrators
      (try! (reward-arbitrators dispute-id))
      
      (ok true)
    )
  )
)

;; Distribute compensation based on dispute resolution
(define-private (distribute-dispute-compensation (dispute-id uint) (resolution uint))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) err-dispute-not-found)))
    (let ((amount (get amount-disputed dispute)))
      (if (is-eq resolution u1) ;; Favor complainant
        (as-contract (stx-transfer? amount tx-sender (get complainant dispute)))
        (if (is-eq resolution u2) ;; Favor defendant  
          (as-contract (stx-transfer? amount tx-sender (get defendant dispute)))
          ;; Split resolution
          (begin
            (try! (as-contract (stx-transfer? (/ amount u2) tx-sender (get complainant dispute))))
            (as-contract (stx-transfer? (/ amount u2) tx-sender (get defendant dispute)))
          )
        )
      )
    )
  )
)

;; Reward arbitrators for their work
(define-private (reward-arbitrators (dispute-id uint))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) err-dispute-not-found)))
    (let ((arbitrators-list (get assigned-arbitrators dispute)))
      (try! (pay-arbitrator (unwrap! (element-at? arbitrators-list u0) (ok true))))
      (try! (pay-arbitrator (unwrap! (element-at? arbitrators-list u1) (ok true))))
      (try! (pay-arbitrator (unwrap! (element-at? arbitrators-list u2) (ok true))))
      (ok true)
    )
  )
)

;; Pay individual arbitrator
(define-private (pay-arbitrator (arbitrator principal))
  (let ((arbitrator-data (unwrap! (map-get? arbitrators arbitrator) err-not-arbitrator)))
    (try! (as-contract (stx-transfer? (var-get arbitrator-reward) tx-sender arbitrator)))
    (ok (map-set arbitrators arbitrator 
      (merge arbitrator-data {
        total-cases: (+ (get total-cases arbitrator-data) u1)
      })))
  )
)

;; Helper function to get disputes for a ride (simplified check)
(define-private (get-disputes-for-ride (ride-id uint))
  (list) ;; Simplified implementation
)

;; Read-only functions

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id)
)

(define-read-only (get-arbitrator-info (arbitrator principal))
  (map-get? arbitrators arbitrator)
)

(define-read-only (get-dispute-evidence (dispute-id uint) (submitter principal))
  (map-get? dispute-evidence { dispute-id: dispute-id, submitter: submitter })
)

(define-read-only (get-arbitrator-vote (dispute-id uint) (arbitrator principal))
  (map-get? arbitrator-votes { dispute-id: dispute-id, arbitrator: arbitrator })
)

(define-read-only (get-dispute-status (dispute-id uint))
  (let ((dispute (unwrap! (map-get? disputes dispute-id) err-dispute-not-found)))
    (ok {
      resolved: (get resolved dispute),
      total-votes: (get total-votes dispute),
      complainant-votes: (get votes-for-complainant dispute),
      defendant-votes: (get votes-for-defendant dispute),
      resolution: (get resolution dispute)
    })
  )
)

;; Pool ride constants
(define-constant err-pool-full (err u500))
(define-constant err-pool-not-found (err u501))
(define-constant err-already-in-pool (err u502))
(define-constant err-pool-not-ready (err u503))
(define-constant err-insufficient-participants (err u504))

;; Pool ride data maps
(define-map pool-rides
  uint
  {
    driver: principal,
    start-location: (string-ascii 100),
    end-location: (string-ascii 100),
    base-price: uint,
    max-participants: uint,
    current-participants: uint,
    participants: (list 8 principal),
    individual-cost: uint,
    started: bool,
    completed: bool,
    timestamp: uint
  }
)

(define-map pool-participants
  { pool-id: uint, participant: principal }
  {
    joined-at: uint,
    paid: bool,
    pickup-location: (string-ascii 100)
  }
)

(define-data-var pool-counter uint u0)

(define-public (create-pool-ride (start-location (string-ascii 100)) (end-location (string-ascii 100)) (base-price uint) (max-participants uint))
  (let (
    (driver tx-sender)
    (pool-id (var-get pool-counter))
  )
    (asserts! (is-some (map-get? drivers driver)) err-not-found)
    (asserts! (and (>= max-participants u2) (<= max-participants u8)) (err u505))
    (var-set pool-counter (+ pool-id u1))
    (ok (map-set pool-rides pool-id {
      driver: driver,
      start-location: start-location,
      end-location: end-location,
      base-price: base-price,
      max-participants: max-participants,
      current-participants: u0,
      participants: (list),
      individual-cost: (/ base-price max-participants),
      started: false,
      completed: false,
      timestamp: stacks-block-height
    }))
  )
)

(define-public (join-pool-ride (pool-id uint) (pickup-location (string-ascii 100)))
  (let (
    (rider tx-sender)
    (pool (unwrap! (map-get? pool-rides pool-id) err-pool-not-found))
    (individual-cost (get individual-cost pool))
  )
    (asserts! (is-some (map-get? riders rider)) err-not-found)
    (asserts! (< (get current-participants pool) (get max-participants pool)) err-pool-full)
    (asserts! (is-none (map-get? pool-participants { pool-id: pool-id, participant: rider })) err-already-in-pool)
    (asserts! (not (get started pool)) err-ride-already-started)
    
    (try! (stx-transfer? individual-cost rider (as-contract tx-sender)))
    
    (map-set pool-participants { pool-id: pool-id, participant: rider } {
      joined-at: stacks-block-height,
      paid: true,
      pickup-location: pickup-location
    })
    
    (let ((new-participants (unwrap! (as-max-len? (append (get participants pool) rider) u8) (err u506))))
      (ok (map-set pool-rides pool-id 
        (merge pool {
          current-participants: (+ (get current-participants pool) u1),
          participants: new-participants
        })))
    )
  )
)

(define-public (start-pool-ride (pool-id uint))
  (let (
    (driver tx-sender)
    (pool (unwrap! (map-get? pool-rides pool-id) err-pool-not-found))
  )
    (asserts! (is-eq driver (get driver pool)) err-not-driver)
    (asserts! (>= (get current-participants pool) u2) err-insufficient-participants)
    (asserts! (not (get started pool)) err-ride-already-started)
    
    (ok (map-set pool-rides pool-id 
      (merge pool { started: true })))
  )
)

(define-public (complete-pool-ride (pool-id uint))
  (let (
    (driver tx-sender)
    (pool (unwrap! (map-get? pool-rides pool-id) err-pool-not-found))
  )
    (asserts! (is-eq driver (get driver pool)) err-not-driver)
    (asserts! (get started pool) err-ride-not-started)
    (asserts! (not (get completed pool)) err-ride-already-completed)
    
    (ok (map-set pool-rides pool-id 
      (merge pool { completed: true })))
  )
)

(define-public (release-pool-payment (pool-id uint))
  (let (
    (caller tx-sender)
    (pool (unwrap! (map-get? pool-rides pool-id) err-pool-not-found))
    (driver (get driver pool))
    (total-collected (* (get individual-cost pool) (get current-participants pool)))
    (platform-fee-amount (/ (* total-collected (var-get platform-fee)) u100))
    (driver-amount (- total-collected platform-fee-amount))
    (driver-data (unwrap! (map-get? drivers driver) err-not-found))
  )
    (asserts! (get completed pool) err-ride-not-completed)
    (asserts! (is-some (map-get? pool-participants { pool-id: pool-id, participant: caller })) err-not-rider)
    
    (try! (as-contract (stx-transfer? driver-amount tx-sender driver)))
    (try! (as-contract (stx-transfer? platform-fee-amount tx-sender contract-owner)))
    
    (map-set drivers driver 
      (merge driver-data {
        total-rides: (+ (get total-rides driver-data) u1),
        earnings: (+ (get earnings driver-data) driver-amount)
      }))
    
    (ok true)
  )
)

(define-public (leave-pool-ride (pool-id uint))
  (let (
    (rider tx-sender)
    (pool (unwrap! (map-get? pool-rides pool-id) err-pool-not-found))
    (participant-data (unwrap! (map-get? pool-participants { pool-id: pool-id, participant: rider }) err-not-found))
    (individual-cost (get individual-cost pool))
  )
    (asserts! (not (get started pool)) err-ride-already-started)
    
    (try! (as-contract (stx-transfer? individual-cost tx-sender rider)))
    
    (map-delete pool-participants { pool-id: pool-id, participant: rider })
    
    (let ((updated-participants (filter remove-participant (get participants pool))))
      (ok (map-set pool-rides pool-id 
        (merge pool {
          current-participants: (- (get current-participants pool) u1),
          participants: updated-participants
        })))
    )
  )
)

(define-private (remove-participant (participant principal))
  (not (is-eq participant tx-sender))
)

(define-read-only (get-pool-ride (pool-id uint))
  (map-get? pool-rides pool-id)
)

(define-read-only (get-pool-participant (pool-id uint) (participant principal))
  (map-get? pool-participants { pool-id: pool-id, participant: participant })
)

(define-read-only (calculate-pool-savings (pool-id uint))
  (let ((pool (unwrap! (map-get? pool-rides pool-id) err-pool-not-found)))
    (let (
      (individual-cost (get individual-cost pool))
      (solo-cost (get base-price pool))
      (savings (- solo-cost individual-cost))
      (savings-percentage (/ (* savings u100) solo-cost))
    )
      (ok {
        individual-cost: individual-cost,
        solo-cost: solo-cost,
        savings: savings,
        savings-percentage: savings-percentage
      })
    )
  )
)

