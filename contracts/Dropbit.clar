;; title: Dropbit
;; version: 1.0.0
;; summary: Decentralized Delivery Escrow with GPS-confirmed delivery
;; description: Smart contract for secure package delivery with escrow and GPS verification

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_DELIVERY_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_DELIVERY_EXPIRED (err u104))
(define-constant ERR_ALREADY_COMPLETED (err u105))
(define-constant ERR_GPS_VERIFICATION_FAILED (err u106))
(define-constant ERR_INVALID_COORDINATES (err u107))

(define-data-var delivery-counter uint u0)
(define-data-var platform-fee-rate uint u250)

(define-map deliveries
  { delivery-id: uint }
  {
    sender: principal,
    recipient: principal,
    courier: (optional principal),
    amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    expires-at: uint,
    pickup-lat: int,
    pickup-lng: int,
    delivery-lat: int,
    delivery-lng: int,
    actual-lat: (optional int),
    actual-lng: (optional int),
    description: (string-ascii 500)
  }
)

(define-map courier-ratings
  { courier: principal }
  { total-rating: uint, delivery-count: uint }
)

(define-map delivery-disputes
  { delivery-id: uint }
  { disputed-by: principal, reason: (string-ascii 200), created-at: uint }
)

(define-public (create-delivery 
  (recipient principal)
  (pickup-lat int)
  (pickup-lng int)
  (delivery-lat int)
  (delivery-lng int)
  (description (string-ascii 500))
  (expires-in-blocks uint))
  (let
    (
      (delivery-id (+ (var-get delivery-counter) u1))
      (amount (stx-get-balance tx-sender))
      (expires-at (+ stacks-block-height expires-in-blocks))
    )
    (asserts! (> amount u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (and (>= pickup-lat -90000000) (<= pickup-lat 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= pickup-lng -180000000) (<= pickup-lng 180000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= delivery-lat -90000000) (<= delivery-lat 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= delivery-lng -180000000) (<= delivery-lng 180000000)) ERR_INVALID_COORDINATES)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set deliveries
      { delivery-id: delivery-id }
      {
        sender: tx-sender,
        recipient: recipient,
        courier: none,
        amount: amount,
        status: "pending",
        created-at: stacks-block-height,
        expires-at: expires-at,
        pickup-lat: pickup-lat,
        pickup-lng: pickup-lng,
        delivery-lat: delivery-lat,
        delivery-lng: delivery-lng,
        actual-lat: none,
        actual-lng: none,
        description: description
      }
    )
    
    (var-set delivery-counter delivery-id)
    (ok delivery-id)
  )
)

(define-public (accept-delivery (delivery-id uint))
  (let
    (
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
    )
    (asserts! (is-eq (get status delivery) "pending") ERR_INVALID_STATUS)
    (asserts! (< stacks-block-height (get expires-at delivery)) ERR_DELIVERY_EXPIRED)
    
    (map-set deliveries
      { delivery-id: delivery-id }
      (merge delivery { courier: (some tx-sender), status: "accepted" })
    )
    (ok true)
  )
)

(define-public (pickup-package (delivery-id uint))
  (let
    (
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
    )
    (asserts! (is-eq (some tx-sender) (get courier delivery)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status delivery) "accepted") ERR_INVALID_STATUS)
    
    (map-set deliveries
      { delivery-id: delivery-id }
      (merge delivery { status: "in-transit" })
    )
    (ok true)
  )
)

(define-public (confirm-delivery 
  (delivery-id uint)
  (actual-lat int)
  (actual-lng int))
  (let
    (
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
      (distance (calculate-distance (get delivery-lat delivery) (get delivery-lng delivery) actual-lat actual-lng))
    )
    (asserts! (is-eq (some tx-sender) (get courier delivery)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status delivery) "in-transit") ERR_INVALID_STATUS)
    (asserts! (< distance 1000) ERR_GPS_VERIFICATION_FAILED)
    
    (map-set deliveries
      { delivery-id: delivery-id }
      (merge delivery { 
        status: "delivered", 
        actual-lat: (some actual-lat),
        actual-lng: (some actual-lng)
      })
    )
    (ok true)
  )
)

(define-public (release-payment (delivery-id uint))
  (let
    (
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
      (courier (unwrap! (get courier delivery) ERR_NOT_AUTHORIZED))
      (platform-fee (/ (* (get amount delivery) (var-get platform-fee-rate)) u10000))
      (courier-payment (- (get amount delivery) platform-fee))
    )
    (asserts! (or 
      (is-eq tx-sender (get recipient delivery))
      (and (is-eq (get status delivery) "delivered") (> stacks-block-height (+ (get created-at delivery) u144)))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status delivery) "delivered") ERR_INVALID_STATUS)
    
    (try! (as-contract (stx-transfer? courier-payment tx-sender courier)))
    (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
    
    (map-set deliveries
      { delivery-id: delivery-id }
      (merge delivery { status: "completed" })
    )
    
    (update-courier-rating courier)
    (ok true)
  )
)

(define-public (dispute-delivery (delivery-id uint) (reason (string-ascii 200)))
  (let
    (
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender (get sender delivery))
      (is-eq tx-sender (get recipient delivery))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq (get status delivery) "completed")) ERR_ALREADY_COMPLETED)
    
    (map-set delivery-disputes
      { delivery-id: delivery-id }
      { disputed-by: tx-sender, reason: reason, created-at: stacks-block-height }
    )
    
    (map-set deliveries
      { delivery-id: delivery-id }
      (merge delivery { status: "disputed" })
    )
    (ok true)
  )
)

(define-public (resolve-dispute (delivery-id uint) (refund-sender bool))
  (let
    (
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status delivery) "disputed") ERR_INVALID_STATUS)
    
    (if refund-sender
      (begin
        (try! (as-contract (stx-transfer? (get amount delivery) tx-sender (get sender delivery))))
        (map-set deliveries
          { delivery-id: delivery-id }
          (merge delivery { status: "refunded" })
        )
      )
      (begin
        (let
          (
            (courier (unwrap! (get courier delivery) ERR_NOT_AUTHORIZED))
            (platform-fee (/ (* (get amount delivery) (var-get platform-fee-rate)) u10000))
            (courier-payment (- (get amount delivery) platform-fee))
          )
          (try! (as-contract (stx-transfer? courier-payment tx-sender courier)))
          (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
          (map-set deliveries
            { delivery-id: delivery-id }
            (merge delivery { status: "completed" })
          )
          (update-courier-rating courier)
        )
      )
    )
    (ok true)
  )
)

(define-public (cancel-delivery (delivery-id uint))
  (let
    (
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get sender delivery)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status delivery) "pending") ERR_INVALID_STATUS)
    
    (try! (as-contract (stx-transfer? (get amount delivery) tx-sender (get sender delivery))))
    
    (map-set deliveries
      { delivery-id: delivery-id }
      (merge delivery { status: "cancelled" })
    )
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee-rate u1000) (err u108))
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-read-only (get-delivery (delivery-id uint))
  (map-get? deliveries { delivery-id: delivery-id })
)

(define-read-only (get-courier-rating (courier principal))
  (default-to 
    { total-rating: u0, delivery-count: u0 }
    (map-get? courier-ratings { courier: courier })
  )
)

(define-read-only (get-delivery-dispute (delivery-id uint))
  (map-get? delivery-disputes { delivery-id: delivery-id })
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-delivery-counter)
  (var-get delivery-counter)
)

(define-private (calculate-distance (lat1 int) (lng1 int) (lat2 int) (lng2 int))
  (let
    (
      (lat-diff (if (> lat1 lat2) (- lat1 lat2) (- lat2 lat1)))
      (lng-diff (if (> lng1 lng2) (- lng1 lng2) (- lng2 lng1)))
    )
    (+ lat-diff lng-diff)
  )
)

(define-private (update-courier-rating (courier principal))
  (let
    (
      (current-rating (get-courier-rating courier))
      (new-count (+ (get delivery-count current-rating) u1))
      (new-total (+ (get total-rating current-rating) u5))
    )
    (map-set courier-ratings
      { courier: courier }
      { total-rating: new-total, delivery-count: new-count }
    )
  )
)