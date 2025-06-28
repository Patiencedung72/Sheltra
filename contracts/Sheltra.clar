(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-invalid-capacity (err u105))
(define-constant err-shelter-full (err u106))
(define-constant err-not-verified (err u107))
(define-constant err-invalid-amount (err u108))

(define-data-var next-shelter-id uint u0)
(define-data-var next-donor-id uint u0)
(define-data-var next-match-id uint u0)
(define-data-var total-donations uint u0)
(define-data-var platform-fee-rate uint u25)

(define-map shelters
  uint
  {
    owner: principal,
    name: (string-ascii 100),
    location: (string-ascii 100),
    capacity: uint,
    current-occupancy: uint,
    funding-goal: uint,
    funding-received: uint,
    verified: bool,
    active: bool,
    created-at: uint,
    contact-info: (string-ascii 200)
  }
)

(define-map donors
  uint
  {
    address: principal,
    name: (string-ascii 100),
    total-donated: uint,
    donations-count: uint,
    verified: bool,
    created-at: uint
  }
)

(define-map matches
  uint
  {
    shelter-id: uint,
    donor-id: uint,
    amount: uint,
    status: (string-ascii 20),
    matched-at: uint,
    completed-at: (optional uint)
  }
)

(define-map shelter-needs
  uint
  {
    food: bool,
    medical: bool,
    clothing: bool,
    education: bool,
    transportation: bool,
    legal-aid: bool
  }
)

(define-map donor-preferences
  uint
  {
    preferred-location: (optional (string-ascii 100)),
    max-donation: uint,
    preferred-needs: (list 6 (string-ascii 20)),
    recurring: bool
  }
)

(define-map shelter-ratings
  {shelter-id: uint, donor-id: uint}
  {
    rating: uint,
    feedback: (string-ascii 500),
    created-at: uint
  }
)

(define-public (register-shelter
  (name (string-ascii 100))
  (location (string-ascii 100))
  (capacity uint)
  (funding-goal uint)
  (contact-info (string-ascii 200))
  (needs {food: bool, medical: bool, clothing: bool, education: bool, transportation: bool, legal-aid: bool}))
  (let ((shelter-id (var-get next-shelter-id)))
    (asserts! (> capacity u0) err-invalid-capacity)
    (asserts! (> funding-goal u0) err-invalid-amount)
    (map-set shelters shelter-id
      {
        owner: tx-sender,
        name: name,
        location: location,
        capacity: capacity,
        current-occupancy: u0,
        funding-goal: funding-goal,
        funding-received: u0,
        verified: false,
        active: true,
        created-at: stacks-block-height,
        contact-info: contact-info
      })
    (map-set shelter-needs shelter-id needs)
    (var-set next-shelter-id (+ shelter-id u1))
    (ok shelter-id)))

(define-public (register-donor
  (name (string-ascii 100))
  (preferences {preferred-location: (optional (string-ascii 100)), max-donation: uint, preferred-needs: (list 6 (string-ascii 20)), recurring: bool}))
  (let ((donor-id (var-get next-donor-id)))
    (map-set donors donor-id
      {
        address: tx-sender,
        name: name,
        total-donated: u0,
        donations-count: u0,
        verified: false,
        created-at: stacks-block-height
      })
    (map-set donor-preferences donor-id preferences)
    (var-set next-donor-id (+ donor-id u1))
    (ok donor-id)))

(define-public (verify-shelter (shelter-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? shelters shelter-id)
      shelter (begin
        (map-set shelters shelter-id (merge shelter {verified: true}))
        (ok true))
      err-not-found)))

(define-public (verify-donor (donor-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? donors donor-id)
      donor (begin
        (map-set donors donor-id (merge donor {verified: true}))
        (ok true))
      err-not-found)))

(define-public (donate-to-shelter (shelter-id uint) (amount uint))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found)))
    (asserts! (get verified shelter) err-not-verified)
    (asserts! (get active shelter) err-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    (let ((platform-fee (/ (* amount (var-get platform-fee-rate)) u1000))
          (net-amount (- amount platform-fee)))
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (map-set shelters shelter-id
        (merge shelter {funding-received: (+ (get funding-received shelter) net-amount)}))
      (var-set total-donations (+ (var-get total-donations) amount))
      (ok net-amount))))

(define-public (create-match (shelter-id uint) (donor-id uint) (amount uint))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found))
        (donor (unwrap! (map-get? donors donor-id) err-not-found))
        (match-id (var-get next-match-id)))
    (asserts! (get verified shelter) err-not-verified)
    (asserts! (get verified donor) err-not-verified)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-eq (get address donor) tx-sender) err-unauthorized)
    (map-set matches match-id
      {
        shelter-id: shelter-id,
        donor-id: donor-id,
        amount: amount,
        status: "pending",
        matched-at: stacks-block-height,
        completed-at: none
      })
    (var-set next-match-id (+ match-id u1))
    (ok match-id)))

(define-public (complete-match (match-id uint))
  (let ((match-data (unwrap! (map-get? matches match-id) err-not-found))
        (shelter-id (get shelter-id match-data))
        (donor-id (get donor-id match-data))
        (amount (get amount match-data)))
    (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found))
          (donor (unwrap! (map-get? donors donor-id) err-not-found)))
      (asserts! (is-eq (get address donor) tx-sender) err-unauthorized)
      (asserts! (is-eq (get status match-data) "pending") err-unauthorized)
      (let ((platform-fee (/ (* amount (var-get platform-fee-rate)) u1000))
            (net-amount (- amount platform-fee)))
        (try! (stx-transfer? amount tx-sender (get owner shelter)))
        (map-set matches match-id
          (merge match-data {status: "completed", completed-at: (some stacks-block-height)}))
        (map-set shelters shelter-id
          (merge shelter {funding-received: (+ (get funding-received shelter) net-amount)}))
        (map-set donors donor-id
          (merge donor {
            total-donated: (+ (get total-donated donor) amount),
            donations-count: (+ (get donations-count donor) u1)
          }))
        (var-set total-donations (+ (var-get total-donations) amount))
        (ok true)))))

(define-public (update-occupancy (shelter-id uint) (new-occupancy uint))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (asserts! (<= new-occupancy (get capacity shelter)) err-shelter-full)
    (map-set shelters shelter-id
      (merge shelter {current-occupancy: new-occupancy}))
    (ok true)))

(define-public (rate-shelter (shelter-id uint) (donor-id uint) (rating uint) (feedback (string-ascii 500)))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found))
        (donor (unwrap! (map-get? donors donor-id) err-not-found)))
    (asserts! (is-eq (get address donor) tx-sender) err-unauthorized)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-amount)
    (map-set shelter-ratings {shelter-id: shelter-id, donor-id: donor-id}
      {
        rating: rating,
        feedback: feedback,
        created-at: stacks-block-height
      })
    (ok true)))

(define-public (deactivate-shelter (shelter-id uint))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (map-set shelters shelter-id (merge shelter {active: false}))
    (ok true)))

(define-public (emergency-withdraw (shelter-id uint))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (let ((balance (get funding-received shelter)))
      (asserts! (> balance u0) err-insufficient-funds)
      (try! (as-contract (stx-transfer? balance tx-sender (get owner shelter))))
      (map-set shelters shelter-id (merge shelter {funding-received: u0}))
      (ok balance))))

(define-read-only (get-shelter (shelter-id uint))
  (map-get? shelters shelter-id))

(define-read-only (get-donor (donor-id uint))
  (map-get? donors donor-id))

(define-read-only (get-match (match-id uint))
  (map-get? matches match-id))

(define-read-only (get-shelter-needs (shelter-id uint))
  (map-get? shelter-needs shelter-id))

(define-read-only (get-donor-preferences (donor-id uint))
  (map-get? donor-preferences donor-id))

(define-read-only (get-shelter-rating (shelter-id uint) (donor-id uint))
  (map-get? shelter-ratings {shelter-id: shelter-id, donor-id: donor-id}))

(define-read-only (get-platform-stats)
  {
    total-shelters: (var-get next-shelter-id),
    total-donors: (var-get next-donor-id),
    total-matches: (var-get next-match-id),
    total-donations: (var-get total-donations),
    platform-fee-rate: (var-get platform-fee-rate)
  })

(define-read-only (get-shelter-capacity-info (shelter-id uint))
  (match (map-get? shelters shelter-id)
    shelter (ok {
      capacity: (get capacity shelter),
      current-occupancy: (get current-occupancy shelter),
      available-spots: (- (get capacity shelter) (get current-occupancy shelter)),
      occupancy-rate: (/ (* (get current-occupancy shelter) u100) (get capacity shelter))
    })
    err-not-found))

(define-read-only (get-shelter-funding-info (shelter-id uint))
  (match (map-get? shelters shelter-id)
    shelter (ok {
      funding-goal: (get funding-goal shelter),
      funding-received: (get funding-received shelter),
      funding-remaining: (- (get funding-goal shelter) (get funding-received shelter)),
      funding-percentage: (/ (* (get funding-received shelter) u100) (get funding-goal shelter))
    })
    err-not-found))
