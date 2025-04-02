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
