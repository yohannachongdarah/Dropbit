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
(define-constant ERR_ROUTE_NOT_FOUND (err u108))
(define-constant ERR_ROUTE_FULL (err u109))
(define-constant ERR_INVALID_ROUTE_STATUS (err u110))
(define-constant ERR_INSURANCE_CLAIM_EXISTS (err u111))
(define-constant ERR_INSURANCE_NOT_ACTIVE (err u112))
(define-constant ERR_CLAIM_PERIOD_EXPIRED (err u113))
(define-constant ERR_INSUFFICIENT_INSURANCE_FUNDS (err u114))
(define-constant ERR_ARBITRATOR_EXISTS (err u115))
(define-constant ERR_ARBITRATOR_NOT_FOUND (err u116))
(define-constant ERR_INSUFFICIENT_STAKE (err u117))
(define-constant ERR_VOTING_PERIOD_ENDED (err u118))
(define-constant ERR_ALREADY_VOTED (err u119))
(define-constant ERR_ARBITRATION_NOT_FOUND (err u120))
(define-constant ERR_ARBITRATION_COMPLETE (err u121))
(define-constant ERR_MINIMUM_ARBITRATORS_NOT_MET (err u122))
(define-constant ERR_INVALID_ZONE (err u123))
(define-constant ERR_PRICING_NOT_FOUND (err u124))
(define-constant ERR_INVALID_URGENCY_LEVEL (err u125))
(define-constant ERR_SURGE_PRICING_ACTIVE (err u126))

(define-data-var delivery-counter uint u0)
(define-data-var platform-fee-rate uint u250)
(define-data-var route-counter uint u0)
(define-data-var insurance-pool uint u0)
(define-data-var base-insurance-rate uint u500)

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

(define-map delivery-routes
  { route-id: uint }
  {
    courier: principal,
    delivery-ids: (list 10 uint),
    status: (string-ascii 20),
    created-at: uint,
    estimated-duration: uint,
    total-distance: uint,
    route-fee: uint
  }
)

(define-map delivery-insurance
  { delivery-id: uint }
  {
    insured-amount: uint,
    premium-paid: uint,
    coverage-type: (string-ascii 50),
    active: bool,
    claim-deadline: uint
  }
)

(define-map insurance-claims
  { claim-id: uint }
  {
    delivery-id: uint,
    claimant: principal,
    amount-claimed: uint,
    reason: (string-ascii 300),
    status: (string-ascii 20),
    created-at: uint,
    evidence-hash: (optional (buff 32))
  }
)

(define-data-var claim-counter uint u0)

(define-map arbitrators
  { arbitrator: principal }
  {
    stake-amount: uint,
    reputation-score: uint,
    total-cases: uint,
    successful-cases: uint,
    active: bool,
    joined-at: uint
  }
)

(define-map arbitrations
  { arbitration-id: uint }
  {
    delivery-id: uint,
    requester: principal,
    evidence-score: uint,
    total-votes: uint,
    votes-for-sender: uint,
    votes-for-courier: uint,
    status: (string-ascii 20),
    created-at: uint,
    voting-deadline: uint,
    automatic-resolution: bool
  }
)

(define-map arbitrator-votes
  { arbitration-id: uint, arbitrator: principal }
  { vote: (string-ascii 20), weight: uint, timestamp: uint }
)

(define-map zone-demand
  { zone-id: uint }
  {
    active-deliveries: uint,
    completed-deliveries: uint,
    available-couriers: uint,
    demand-score: uint,
    last-updated: uint
  }
)

(define-map delivery-pricing
  { delivery-id: uint }
  {
    base-price: uint,
    distance-fee: uint,
    urgency-fee: uint,
    surge-fee: uint,
    peak-hour-fee: uint,
    total-price: uint,
    urgency-level: (string-ascii 20)
  }
)

(define-map hourly-demand
  { hour: uint, zone-id: uint }
  { delivery-count: uint, courier-count: uint, demand-ratio: uint }
)
(define-data-var arbitrator-counter uint u0)
(define-data-var arbitration-counter uint u0)
(define-data-var minimum-arbitrator-stake uint u1000000)
(define-data-var voting-period-blocks uint u1008)
(define-data-var minimum-arbitrators-required uint u3)
(define-data-var base-delivery-price uint u100000)
(define-data-var distance-rate uint u100)
(define-data-var surge-multiplier uint u100)
(define-data-var peak-hours-start uint u8)
(define-data-var peak-hours-end uint u18)
(define-data-var urgency-multiplier-express uint u150)
(define-data-var urgency-multiplier-rush uint u200)

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
    (asserts! (<= new-fee-rate u1000) ERR_ROUTE_NOT_FOUND)
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-public (create-delivery-route 
  (delivery-ids (list 10 uint))
  (estimated-duration uint)
  (route-fee uint))
  (let
    (
      (route-id (+ (var-get route-counter) u1))
      (total-distance (calculate-route-distance delivery-ids))
    )
    (asserts! (> (len delivery-ids) u0) ERR_INVALID_STATUS)
    (asserts! (<= (len delivery-ids) u10) ERR_ROUTE_FULL)
    (asserts! (validate-route-deliveries delivery-ids) ERR_INVALID_STATUS)
    
    (map-set delivery-routes
      { route-id: route-id }
      {
        courier: tx-sender,
        delivery-ids: delivery-ids,
        status: "planned",
        created-at: stacks-block-height,
        estimated-duration: estimated-duration,
        total-distance: total-distance,
        route-fee: route-fee
      }
    )
    
    (var-set route-counter route-id)
    (ok route-id)
  )
)

(define-public (start-delivery-route (route-id uint))
  (let
    (
      (route (unwrap! (map-get? delivery-routes { route-id: route-id }) ERR_ROUTE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get courier route)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status route) "planned") ERR_INVALID_ROUTE_STATUS)
    
    (map-set delivery-routes
      { route-id: route-id }
      (merge route { status: "active" })
    )
    
    (fold update-delivery-status-for-route (get delivery-ids route) true)
    (ok true)
  )
)

(define-public (complete-delivery-route (route-id uint))
  (let
    (
      (route (unwrap! (map-get? delivery-routes { route-id: route-id }) ERR_ROUTE_NOT_FOUND))
      (route-bonus (calculate-route-bonus route))
    )
    (asserts! (is-eq tx-sender (get courier route)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status route) "active") ERR_INVALID_ROUTE_STATUS)
    (asserts! (all-deliveries-completed (get delivery-ids route)) ERR_INVALID_STATUS)
    
    (try! (as-contract (stx-transfer? route-bonus tx-sender (get courier route))))
    
    (map-set delivery-routes
      { route-id: route-id }
      (merge route { status: "completed" })
    )
    (ok true)
  )
)

(define-public (purchase-delivery-insurance 
  (delivery-id uint)
  (coverage-type (string-ascii 50)))
  (let
    (
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
      (insured-amount (get amount delivery))
      (premium (calculate-insurance-premium insured-amount coverage-type))
      (claim-deadline (+ stacks-block-height u1008))
    )
    (asserts! (is-eq tx-sender (get sender delivery)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status delivery) "pending") ERR_INVALID_STATUS)
    (asserts! (>= (stx-get-balance tx-sender) premium) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) premium))
    
    (map-set delivery-insurance
      { delivery-id: delivery-id }
      {
        insured-amount: insured-amount,
        premium-paid: premium,
        coverage-type: coverage-type,
        active: true,
        claim-deadline: claim-deadline
      }
    )
    (ok true)
  )
)

(define-public (file-insurance-claim 
  (delivery-id uint)
  (amount-claimed uint)
  (reason (string-ascii 300))
  (evidence-hash (optional (buff 32))))
  (let
    (
      (claim-id (+ (var-get claim-counter) u1))
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
      (insurance (unwrap! (map-get? delivery-insurance { delivery-id: delivery-id }) ERR_INSURANCE_NOT_ACTIVE))
    )
    (asserts! (or 
      (is-eq tx-sender (get sender delivery))
      (is-eq tx-sender (get recipient delivery))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (get active insurance) ERR_INSURANCE_NOT_ACTIVE)
    (asserts! (< stacks-block-height (get claim-deadline insurance)) ERR_CLAIM_PERIOD_EXPIRED)
    (asserts! (<= amount-claimed (get insured-amount insurance)) ERR_INSUFFICIENT_INSURANCE_FUNDS)
    (asserts! (is-none (map-get? insurance-claims { claim-id: claim-id })) ERR_INSURANCE_CLAIM_EXISTS)
    
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        delivery-id: delivery-id,
        claimant: tx-sender,
        amount-claimed: amount-claimed,
        reason: reason,
        status: "pending",
        created-at: stacks-block-height,
        evidence-hash: evidence-hash
      }
    )
    
    (var-set claim-counter claim-id)
    (ok claim-id)
  )
)

(define-public (process-insurance-claim (claim-id uint) (approve bool))
  (let
    (
      (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR_INSURANCE_CLAIM_EXISTS))
      (payout-amount (get amount-claimed claim))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status claim) "pending") ERR_INVALID_STATUS)
    
    (if approve
      (begin
        (asserts! (>= (var-get insurance-pool) payout-amount) ERR_INSUFFICIENT_INSURANCE_FUNDS)
        (try! (as-contract (stx-transfer? payout-amount tx-sender (get claimant claim))))
        (var-set insurance-pool (- (var-get insurance-pool) payout-amount))
        (map-set insurance-claims
          { claim-id: claim-id }
          (merge claim { status: "approved" })
        )
      )
      (map-set insurance-claims
        { claim-id: claim-id }
        (merge claim { status: "rejected" })
      )
    )
    (ok true)
  )
)

(define-public (add-insurance-funds (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) amount))
    (ok true)
  )
)

(define-public (set-insurance-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rate u2000) ERR_INVALID_STATUS)
    (var-set base-insurance-rate new-rate)
    (ok true)
  )
)

(define-public (register-arbitrator (stake-amount uint))
  (let
    (
      (arbitrator-id (+ (var-get arbitrator-counter) u1))
      (minimum-stake (var-get minimum-arbitrator-stake))
    )
    (asserts! (>= stake-amount minimum-stake) ERR_INSUFFICIENT_STAKE)
    (asserts! (is-none (map-get? arbitrators { arbitrator: tx-sender })) ERR_ARBITRATOR_EXISTS)
    (asserts! (>= (stx-get-balance tx-sender) stake-amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set arbitrators
      { arbitrator: tx-sender }
      {
        stake-amount: stake-amount,
        reputation-score: u100,
        total-cases: u0,
        successful-cases: u0,
        active: true,
        joined-at: stacks-block-height
      }
    )
    
    (var-set arbitrator-counter arbitrator-id)
    (ok arbitrator-id)
  )
)

(define-public (stake-additional-funds (additional-amount uint))
  (let
    (
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR_ARBITRATOR_NOT_FOUND))
    )
    (asserts! (> additional-amount u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (get active arbitrator-data) ERR_ARBITRATOR_NOT_FOUND)
    (asserts! (>= (stx-get-balance tx-sender) additional-amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? additional-amount tx-sender (as-contract tx-sender)))
    
    (map-set arbitrators
      { arbitrator: tx-sender }
      (merge arbitrator-data { 
        stake-amount: (+ (get stake-amount arbitrator-data) additional-amount)
      })
    )
    (ok true)
  )
)

(define-public (request-arbitration (delivery-id uint))
  (let
    (
      (arbitration-id (+ (var-get arbitration-counter) u1))
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
      (evidence-score (calculate-evidence-score delivery-id))
      (voting-deadline (+ stacks-block-height (var-get voting-period-blocks)))
      (auto-resolve (>= evidence-score u80))
    )
    (asserts! (or 
      (is-eq tx-sender (get sender delivery))
      (is-eq tx-sender (get recipient delivery))
      (is-eq (some tx-sender) (get courier delivery))
    ) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status delivery) "disputed") ERR_INVALID_STATUS)
    (asserts! (>= (count-active-arbitrators) (var-get minimum-arbitrators-required)) ERR_MINIMUM_ARBITRATORS_NOT_MET)
    
    (map-set arbitrations
      { arbitration-id: arbitration-id }
      {
        delivery-id: delivery-id,
        requester: tx-sender,
        evidence-score: evidence-score,
        total-votes: u0,
        votes-for-sender: u0,
        votes-for-courier: u0,
        status: (if auto-resolve "auto-resolved" "voting"),
        created-at: stacks-block-height,
        voting-deadline: voting-deadline,
        automatic-resolution: auto-resolve
      }
    )
    
    (var-set arbitration-counter arbitration-id)
    
    (if auto-resolve
      (execute-automatic-resolution arbitration-id delivery-id evidence-score)
      (ok arbitration-id)
    )
  )
)

(define-public (vote-on-arbitration 
  (arbitration-id uint)
  (vote-for-sender bool))
  (let
    (
      (arbitration (unwrap! (map-get? arbitrations { arbitration-id: arbitration-id }) ERR_ARBITRATION_NOT_FOUND))
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR_ARBITRATOR_NOT_FOUND))
      (vote-weight (calculate-vote-weight arbitrator-data))
      (vote-string (if vote-for-sender "sender" "courier"))
    )
    (asserts! (get active arbitrator-data) ERR_ARBITRATOR_NOT_FOUND)
    (asserts! (is-eq (get status arbitration) "voting") ERR_ARBITRATION_COMPLETE)
    (asserts! (< stacks-block-height (get voting-deadline arbitration)) ERR_VOTING_PERIOD_ENDED)
    (asserts! (is-none (map-get? arbitrator-votes { arbitration-id: arbitration-id, arbitrator: tx-sender })) ERR_ALREADY_VOTED)
    
    (map-set arbitrator-votes
      { arbitration-id: arbitration-id, arbitrator: tx-sender }
      { vote: vote-string, weight: vote-weight, timestamp: stacks-block-height }
    )
    
    (let
      (
        (new-total-votes (+ (get total-votes arbitration) vote-weight))
        (new-votes-for-sender (if vote-for-sender 
          (+ (get votes-for-sender arbitration) vote-weight)
          (get votes-for-sender arbitration)
        ))
        (new-votes-for-courier (if vote-for-sender
          (get votes-for-courier arbitration)
          (+ (get votes-for-courier arbitration) vote-weight)
        ))
      )
      (map-set arbitrations
        { arbitration-id: arbitration-id }
        (merge arbitration {
          total-votes: new-total-votes,
          votes-for-sender: new-votes-for-sender,
          votes-for-courier: new-votes-for-courier
        })
      )
    )
    (ok true)
  )
)

(define-public (finalize-arbitration (arbitration-id uint))
  (let
    (
      (arbitration (unwrap! (map-get? arbitrations { arbitration-id: arbitration-id }) ERR_ARBITRATION_NOT_FOUND))
      (delivery-id (get delivery-id arbitration))
      (delivery (unwrap! (map-get? deliveries { delivery-id: delivery-id }) ERR_DELIVERY_NOT_FOUND))
      (refund-sender (> (get votes-for-sender arbitration) (get votes-for-courier arbitration)))
    )
    (asserts! (is-eq (get status arbitration) "voting") ERR_ARBITRATION_COMPLETE)
    (asserts! (>= stacks-block-height (get voting-deadline arbitration)) ERR_VOTING_PERIOD_ENDED)
    (asserts! (> (get total-votes arbitration) u0) ERR_INVALID_STATUS)
    
    (execute-arbitration-result delivery-id refund-sender)
    
    (map-set arbitrations
      { arbitration-id: arbitration-id }
      (merge arbitration { status: "finalized" })
    )
    
    (unwrap-panic (update-arbitrator-reputations arbitration-id refund-sender))
    (ok refund-sender)
  )
)

(define-public (withdraw-arbitrator-stake)
  (let
    (
      (arbitrator-data (unwrap! (map-get? arbitrators { arbitrator: tx-sender }) ERR_ARBITRATOR_NOT_FOUND))
      (stake-amount (get stake-amount arbitrator-data))
    )
    (asserts! (get active arbitrator-data) ERR_ARBITRATOR_NOT_FOUND)
    (asserts! (> stake-amount u0) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
    
    (map-set arbitrators
      { arbitrator: tx-sender }
      (merge arbitrator-data { 
        stake-amount: u0,
        active: false
      })
    )
    (ok stake-amount)
  )
)

(define-public (calculate-delivery-price 
  (pickup-lat int)
  (pickup-lng int)
  (delivery-lat int)
  (delivery-lng int)
  (urgency-level (string-ascii 20)))
  (let
    (
      (distance (to-uint (calculate-distance pickup-lat pickup-lng delivery-lat delivery-lng)))
      (zone-id (calculate-zone-id delivery-lat delivery-lng))
      (base-price (var-get base-delivery-price))
      (distance-fee (* distance (var-get distance-rate)))
      (urgency-fee (calculate-urgency-fee urgency-level))
      (zone-demand-data (get-zone-demand-data zone-id))
      (surge-fee (calculate-surge-fee zone-demand-data base-price))
      (peak-hour-fee (calculate-peak-hour-fee))
      (total-price (+ base-price distance-fee urgency-fee surge-fee peak-hour-fee))
    )
    (asserts! (is-valid-urgency-level urgency-level) ERR_INVALID_URGENCY_LEVEL)
    (ok {
      base-price: base-price,
      distance-fee: distance-fee,
      urgency-fee: urgency-fee,
      surge-fee: surge-fee,
      peak-hour-fee: peak-hour-fee,
      total-price: total-price,
      zone-id: zone-id
    })
  )
)

(define-public (create-delivery-with-pricing
  (recipient principal)
  (pickup-lat int)
  (pickup-lng int)
  (delivery-lat int)
  (delivery-lng int)
  (description (string-ascii 500))
  (expires-in-blocks uint)
  (urgency-level (string-ascii 20)))
  (let
    (
      (delivery-id (+ (var-get delivery-counter) u1))
      (pricing-result (unwrap! (calculate-delivery-price pickup-lat pickup-lng delivery-lat delivery-lng urgency-level) ERR_PRICING_NOT_FOUND))
      (total-price (get total-price pricing-result))
      (zone-id (get zone-id pricing-result))
      (expires-at (+ stacks-block-height expires-in-blocks))
    )
    (asserts! (>= (stx-get-balance tx-sender) total-price) ERR_INSUFFICIENT_FUNDS)
    (asserts! (and (>= pickup-lat -90000000) (<= pickup-lat 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= pickup-lng -180000000) (<= pickup-lng 180000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= delivery-lat -90000000) (<= delivery-lat 90000000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= delivery-lng -180000000) (<= delivery-lng 180000000)) ERR_INVALID_COORDINATES)
    
    (try! (stx-transfer? total-price tx-sender (as-contract tx-sender)))
    
    (map-set deliveries
      { delivery-id: delivery-id }
      {
        sender: tx-sender,
        recipient: recipient,
        courier: none,
        amount: total-price,
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
    
    (map-set delivery-pricing
      { delivery-id: delivery-id }
      {
        base-price: (get base-price pricing-result),
        distance-fee: (get distance-fee pricing-result),
        urgency-fee: (get urgency-fee pricing-result),
        surge-fee: (get surge-fee pricing-result),
        peak-hour-fee: (get peak-hour-fee pricing-result),
        total-price: total-price,
        urgency-level: urgency-level
      }
    )
    
    (unwrap-panic (update-zone-demand zone-id 1 0))
    (var-set delivery-counter delivery-id)
    (ok delivery-id)
  )
)

(define-public (update-zone-demand (zone-id uint) (active-change int) (courier-change int))
  (let
    (
      (current-demand (default-to 
        { active-deliveries: u0, completed-deliveries: u0, available-couriers: u0, demand-score: u100, last-updated: u0 }
        (map-get? zone-demand { zone-id: zone-id })
      ))
      (new-active (if (>= active-change 0) 
        (+ (get active-deliveries current-demand) (to-uint active-change))
        (if (>= (get active-deliveries current-demand) (to-uint (- 0 active-change)))
          (- (get active-deliveries current-demand) (to-uint (- 0 active-change)))
          u0
        )
      ))
      (new-couriers (if (>= courier-change 0)
        (+ (get available-couriers current-demand) (to-uint courier-change))
        (if (>= (get available-couriers current-demand) (to-uint (- 0 courier-change)))
          (- (get available-couriers current-demand) (to-uint (- 0 courier-change)))
          u0
        )
      ))
      (demand-score (calculate-demand-score new-active new-couriers))
    )
    (asserts! (< zone-id u100) ERR_INVALID_ZONE)
    
    (map-set zone-demand
      { zone-id: zone-id }
      (merge current-demand {
        active-deliveries: new-active,
        available-couriers: new-couriers,
        demand-score: demand-score,
        last-updated: stacks-block-height
      })
    )
    (ok true)
  )
)

(define-public (register-courier-availability (zone-id uint))
  (begin
    (asserts! (< zone-id u100) ERR_INVALID_ZONE)
    (update-zone-demand zone-id 0 1)
  )
)

(define-public (update-pricing-parameters 
  (new-base-price uint)
  (new-distance-rate uint)
  (new-surge-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> new-base-price u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> new-distance-rate u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (<= new-surge-multiplier u500) ERR_INVALID_STATUS)
    
    (var-set base-delivery-price new-base-price)
    (var-set distance-rate new-distance-rate)
    (var-set surge-multiplier new-surge-multiplier)
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

(define-read-only (get-delivery-route (route-id uint))
  (map-get? delivery-routes { route-id: route-id })
)

(define-read-only (get-delivery-insurance (delivery-id uint))
  (map-get? delivery-insurance { delivery-id: delivery-id })
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool)
)

(define-read-only (get-route-counter)
  (var-get route-counter)
)

(define-read-only (get-claim-counter)
  (var-get claim-counter)
)

(define-read-only (get-base-insurance-rate)
  (var-get base-insurance-rate)
)

(define-read-only (get-arbitrator (arbitrator principal))
  (map-get? arbitrators { arbitrator: arbitrator })
)

(define-read-only (get-arbitration (arbitration-id uint))
  (map-get? arbitrations { arbitration-id: arbitration-id })
)

(define-read-only (get-arbitrator-vote (arbitration-id uint) (arbitrator principal))
  (map-get? arbitrator-votes { arbitration-id: arbitration-id, arbitrator: arbitrator })
)

(define-read-only (get-arbitration-counter)
  (var-get arbitration-counter)
)

(define-read-only (get-arbitrator-counter)
  (var-get arbitrator-counter)
)

(define-read-only (get-minimum-arbitrator-stake)
  (var-get minimum-arbitrator-stake)
)

(define-read-only (get-voting-period-blocks)
  (var-get voting-period-blocks)
)

(define-read-only (get-minimum-arbitrators-required)
  (var-get minimum-arbitrators-required)
)

(define-read-only (get-delivery-pricing (delivery-id uint))
  (map-get? delivery-pricing { delivery-id: delivery-id })
)

(define-read-only (get-zone-demand (zone-id uint))
  (map-get? zone-demand { zone-id: zone-id })
)

(define-read-only (get-base-delivery-price)
  (var-get base-delivery-price)
)

(define-read-only (get-current-surge-multiplier)
  (var-get surge-multiplier)
)

(define-read-only (get-hourly-demand (hour uint) (zone-id uint))
  (map-get? hourly-demand { hour: hour, zone-id: zone-id })
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

(define-private (calculate-route-distance (delivery-ids (list 10 uint)))
  (fold add-delivery-distance delivery-ids u0)
)

(define-private (add-delivery-distance (delivery-id uint) (total uint))
  (match (map-get? deliveries { delivery-id: delivery-id })
    delivery-data 
      (let
        (
          (pickup-lat (get pickup-lat delivery-data))
          (pickup-lng (get pickup-lng delivery-data))
          (delivery-lat (get delivery-lat delivery-data))
          (delivery-lng (get delivery-lng delivery-data))
          (distance (calculate-distance pickup-lat pickup-lng delivery-lat delivery-lng))
        )
        (+ total (to-uint distance))
      )
    total
  )
)

(define-private (validate-route-deliveries (delivery-ids (list 10 uint)))
  (fold check-delivery-status delivery-ids true)
)

(define-private (check-delivery-status (delivery-id uint) (all-valid bool))
  (if (not all-valid)
    false
    (match (map-get? deliveries { delivery-id: delivery-id })
      delivery-data (is-eq (get status delivery-data) "pending")
      false
    )
  )
)

(define-private (update-delivery-status-for-route (delivery-id uint) (success bool))
  (match (map-get? deliveries { delivery-id: delivery-id })
    delivery-data
      (begin
        (map-set deliveries
          { delivery-id: delivery-id }
          (merge delivery-data { status: "accepted" })
        )
        true
      )
    success
  )
)

(define-private (all-deliveries-completed (delivery-ids (list 10 uint)))
  (fold check-delivery-completed delivery-ids true)
)

(define-private (check-delivery-completed (delivery-id uint) (all-completed bool))
  (if (not all-completed)
    false
    (match (map-get? deliveries { delivery-id: delivery-id })
      delivery-data (is-eq (get status delivery-data) "delivered")
      false
    )
  )
)

(define-private (calculate-route-bonus (route-data { courier: principal, delivery-ids: (list 10 uint), status: (string-ascii 20), created-at: uint, estimated-duration: uint, total-distance: uint, route-fee: uint }))
  (let
    (
      (base-bonus (get route-fee route-data))
      (distance-bonus (/ (get total-distance route-data) u100))
      (delivery-count-bonus (* (len (get delivery-ids route-data)) u50))
    )
    (+ base-bonus distance-bonus delivery-count-bonus)
  )
)

(define-private (calculate-insurance-premium (amount uint) (coverage-type (string-ascii 50)))
  (let
    (
      (base-rate (var-get base-insurance-rate))
      (coverage-multiplier (get-coverage-multiplier coverage-type))
    )
    (/ (* amount base-rate coverage-multiplier) u100000)
  )
)

(define-private (get-coverage-multiplier (coverage-type (string-ascii 50)))
  (if (is-eq coverage-type "basic")
    u100
    (if (is-eq coverage-type "standard")
      u150
      (if (is-eq coverage-type "premium")
        u200
        u100
      )
    )
  )
)

(define-private (calculate-evidence-score (delivery-id uint))
  (match (map-get? deliveries { delivery-id: delivery-id })
    delivery-data
      (let
        (
          (has-actual-coordinates (and (is-some (get actual-lat delivery-data)) (is-some (get actual-lng delivery-data))))
          (gps-accuracy-score (if has-actual-coordinates
            (let
              (
                (actual-lat (unwrap-panic (get actual-lat delivery-data)))
                (actual-lng (unwrap-panic (get actual-lng delivery-data)))
                (target-lat (get delivery-lat delivery-data))
                (target-lng (get delivery-lng delivery-data))
                (distance (calculate-distance target-lat target-lng actual-lat actual-lng))
              )
              (if (< (to-uint distance) u100) u40
                (if (< (to-uint distance) u500) u25
                  (if (< (to-uint distance) u1000) u15 u0)
                )
              )
            )
            u0
          ))
          (delivery-time-score (let
            (
              (delivery-window (- stacks-block-height (get created-at delivery-data)))
            )
            (if (< delivery-window u144) u20
              (if (< delivery-window u288) u15
                (if (< delivery-window u432) u10 u5)
              )
            )
          ))
          (route-score (if (is-eq (get status delivery-data) "delivered") u15 u0))
          (courier-reputation-score (match (get courier delivery-data)
            courier-principal 
              (let
                (
                  (courier-rating (get-courier-rating courier-principal))
                  (success-rate (if (> (get delivery-count courier-rating) u0)
                    (/ (* (get total-rating courier-rating) u100) (* (get delivery-count courier-rating) u5))
                    u0
                  ))
                )
                (if (> success-rate u80) u15
                  (if (> success-rate u60) u10
                    (if (> success-rate u40) u5 u0)
                  )
                )
              )
            u0
          ))
          (insurance-score (if (is-some (map-get? delivery-insurance { delivery-id: delivery-id })) u10 u0))
        )
        (+ gps-accuracy-score delivery-time-score route-score courier-reputation-score insurance-score)
      )
    u0
  )
)

(define-private (count-active-arbitrators)
  (var-get arbitrator-counter)
)

(define-private (calculate-vote-weight (arbitrator-data { stake-amount: uint, reputation-score: uint, total-cases: uint, successful-cases: uint, active: bool, joined-at: uint }))
  (let
    (
      (base-weight u100)
      (stake-bonus (/ (get stake-amount arbitrator-data) u100000))
      (reputation-bonus (/ (get reputation-score arbitrator-data) u10))
      (experience-bonus (if (< (get total-cases arbitrator-data) u20) (get total-cases arbitrator-data) u20))
    )
    (+ base-weight stake-bonus reputation-bonus experience-bonus)
  )
)

(define-private (execute-automatic-resolution (arbitration-id uint) (delivery-id uint) (evidence-score uint))
  (let
    (
      (delivery (unwrap-panic (map-get? deliveries { delivery-id: delivery-id })))
      (favor-courier (>= evidence-score u80))
    )
    (execute-arbitration-result delivery-id (not favor-courier))
    (ok arbitration-id)
  )
)

(define-private (execute-arbitration-result (delivery-id uint) (refund-sender bool))
  (let
    (
      (delivery (unwrap-panic (map-get? deliveries { delivery-id: delivery-id })))
    )
    (if refund-sender
      (begin
        (unwrap-panic (as-contract (stx-transfer? (get amount delivery) tx-sender (get sender delivery))))
        (map-set deliveries
          { delivery-id: delivery-id }
          (merge delivery { status: "refunded" })
        )
      )
      (begin
        (let
          (
            (courier (unwrap-panic (get courier delivery)))
            (platform-fee (/ (* (get amount delivery) (var-get platform-fee-rate)) u10000))
            (courier-payment (- (get amount delivery) platform-fee))
          )
          (unwrap-panic (as-contract (stx-transfer? courier-payment tx-sender courier)))
          (unwrap-panic (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
          (map-set deliveries
            { delivery-id: delivery-id }
            (merge delivery { status: "completed" })
          )
          (update-courier-rating courier)
        )
      )
    )
  )
)

(define-private (update-arbitrator-reputations (arbitration-id uint) (correct-outcome bool))
  (ok true)
)

(define-private (calculate-zone-id (lat int) (lng int))
  (let
    (
      (lat-zone (/ (+ lat 90000000) 1800000))
      (lng-zone (/ (+ lng 180000000) 3600000))
    )
    (+ (* (to-uint lat-zone) u100) (to-uint lng-zone))
  )
)

(define-private (calculate-urgency-fee (urgency-level (string-ascii 20)))
  (let
    (
      (base-price (var-get base-delivery-price))
    )
    (if (is-eq urgency-level "express")
      (/ (* base-price (var-get urgency-multiplier-express)) u100)
      (if (is-eq urgency-level "rush")
        (/ (* base-price (var-get urgency-multiplier-rush)) u100)
        u0
      )
    )
  )
)

(define-private (is-valid-urgency-level (urgency-level (string-ascii 20)))
  (or 
    (is-eq urgency-level "standard")
    (or 
      (is-eq urgency-level "express")
      (is-eq urgency-level "rush")
    )
  )
)

(define-private (get-zone-demand-data (zone-id uint))
  (default-to 
    { active-deliveries: u0, completed-deliveries: u0, available-couriers: u0, demand-score: u100, last-updated: u0 }
    (map-get? zone-demand { zone-id: zone-id })
  )
)

(define-private (calculate-surge-fee (zone-demand-data { active-deliveries: uint, completed-deliveries: uint, available-couriers: uint, demand-score: uint, last-updated: uint }) (base-price uint))
  (let
    (
      (demand-score (get demand-score zone-demand-data))
      (surge-rate (var-get surge-multiplier))
    )
    (if (> demand-score u150)
      (/ (* base-price surge-rate) u100)
      u0
    )
  )
)

(define-private (calculate-peak-hour-fee)
  (let
    (
      (current-hour (mod stacks-block-height u24))
      (peak-start (var-get peak-hours-start))
      (peak-end (var-get peak-hours-end))
      (base-price (var-get base-delivery-price))
    )
    (if (and (>= current-hour peak-start) (<= current-hour peak-end))
      (/ (* base-price u20) u100)
      u0
    )
  )
)

(define-private (calculate-demand-score (active-deliveries uint) (available-couriers uint))
  (if (is-eq available-couriers u0)
    u200
    (let
      (
        (ratio (/ (* active-deliveries u100) available-couriers))
      )
      (if (> ratio u200) u200
        (if (< ratio u50) u50 ratio)
      )
    )
  )
)



