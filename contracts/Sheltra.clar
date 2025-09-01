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
(define-constant err-insufficient-inventory (err u109))
(define-constant err-invalid-resource-type (err u110))
(define-constant err-request-already-processed (err u111))
(define-constant err-invalid-expiry (err u112))
(define-constant err-invalid-priority (err u113))
(define-constant err-emergency-active (err u114))
(define-constant err-no-emergency (err u115))

(define-data-var next-shelter-id uint u0)
(define-data-var next-donor-id uint u0)
(define-data-var next-match-id uint u0)
(define-data-var next-volunteer-id uint u0)
(define-data-var next-opportunity-id uint u0)
(define-data-var next-volunteer-assignment-id uint u0)
(define-data-var next-resource-id uint u0)
(define-data-var next-resource-request-id uint u0)
(define-data-var next-resource-transaction-id uint u0)
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

(define-map volunteers
  uint
  {
    address: principal,
    name: (string-ascii 100),
    email: (string-ascii 100),
    phone: (string-ascii 20),
    skills: (list 10 (string-ascii 50)),
    availability: (string-ascii 200),
    total-hours: uint,
    assignments-count: uint,
    rating: uint,
    active: bool,
    created-at: uint,
    background-checked: bool
  }
)

(define-map volunteer-opportunities
  uint
  {
    shelter-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    required-skills: (list 5 (string-ascii 50)),
    time-commitment: uint,
    max-volunteers: uint,
    current-volunteers: uint,
    urgent: bool,
    active: bool,
    created-at: uint,
    deadline: (optional uint),
    contact-person: (string-ascii 100)
  }
)

(define-map volunteer-assignments
  uint
  {
    volunteer-id: uint,
    opportunity-id: uint,
    shelter-id: uint,
    status: (string-ascii 20),
    hours-committed: uint,
    hours-completed: uint,
    assigned-at: uint,
    completed-at: (optional uint),
    rating-from-shelter: (optional uint),
    rating-from-volunteer: (optional uint),
    feedback: (string-ascii 500)
  }
)

(define-map volunteer-achievements
  {volunteer-id: uint, achievement-type: (string-ascii 50)}
  {
    earned-at: uint,
    description: (string-ascii 200),
    hours-milestone: uint
  }
)

(define-map shelter-resources
  uint
  {
    shelter-id: uint,
    resource-type: (string-ascii 50),
    quantity: uint,
    unit: (string-ascii 20),
    expiry-date: (optional uint),
    low-stock-threshold: uint,
    last-updated: uint,
    notes: (string-ascii 200)
  }
)

(define-map resource-requests
  uint
  {
    requesting-shelter-id: uint,
    resource-type: (string-ascii 50),
    quantity-needed: uint,
    urgency-level: uint,
    description: (string-ascii 300),
    status: (string-ascii 20),
    created-at: uint,
    deadline: (optional uint),
    fulfilled-by: (optional uint),
    fulfilled-at: (optional uint)
  }
)

(define-map resource-transactions
  uint
  {
    from-shelter-id: (optional uint),
    to-shelter-id: uint,
    resource-id: uint,
    quantity: uint,
    transaction-type: (string-ascii 20),
    created-at: uint,
    notes: (string-ascii 300),
    authorized-by: principal
  }
)

(define-map resource-alerts
  {shelter-id: uint, resource-type: (string-ascii 50)}
  {
    alert-type: (string-ascii 30),
    triggered-at: uint,
    resolved: bool,
    resolved-at: (optional uint)
  }
)

;; Emergency Status System - allows shelters to broadcast urgent situations
(define-map emergency-status
  uint
  {
    emergency-type: (string-ascii 20),
    priority: uint,
    message: (string-ascii 300),
    declared-at: uint,
    resolved: bool,
    resolved-at: (optional uint),
    contact-override: (optional (string-ascii 100))
  }
)

(define-public (register-volunteer
  (name (string-ascii 100))
  (email (string-ascii 100))
  (phone (string-ascii 20))
  (skills (list 10 (string-ascii 50)))
  (availability (string-ascii 200)))
  (let ((volunteer-id (var-get next-volunteer-id)))
    (map-set volunteers volunteer-id
      {
        address: tx-sender,
        name: name,
        email: email,
        phone: phone,
        skills: skills,
        availability: availability,
        total-hours: u0,
        assignments-count: u0,
        rating: u0,
        active: true,
        created-at: stacks-block-height,
        background-checked: false
      })
    (var-set next-volunteer-id (+ volunteer-id u1))
    (ok volunteer-id)))

(define-public (create-volunteer-opportunity
  (shelter-id uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
  (required-skills (list 5 (string-ascii 50)))
  (time-commitment uint)
  (max-volunteers uint)
  (urgent bool)
  (deadline (optional uint))
  (contact-person (string-ascii 100)))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found))
        (opportunity-id (var-get next-opportunity-id)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (asserts! (get verified shelter) err-not-verified)
    (asserts! (> max-volunteers u0) err-invalid-capacity)
    (asserts! (> time-commitment u0) err-invalid-amount)
    (map-set volunteer-opportunities opportunity-id
      {
        shelter-id: shelter-id,
        title: title,
        description: description,
        required-skills: required-skills,
        time-commitment: time-commitment,
        max-volunteers: max-volunteers,
        current-volunteers: u0,
        urgent: urgent,
        active: true,
        created-at: stacks-block-height,
        deadline: deadline,
        contact-person: contact-person
      })
    (var-set next-opportunity-id (+ opportunity-id u1))
    (ok opportunity-id)))

(define-public (apply-for-volunteer-opportunity
  (volunteer-id uint)
  (opportunity-id uint)
  (hours-committed uint))
  (let ((volunteer (unwrap! (map-get? volunteers volunteer-id) err-not-found))
        (opportunity (unwrap! (map-get? volunteer-opportunities opportunity-id) err-not-found))
        (assignment-id (var-get next-volunteer-assignment-id)))
    (asserts! (is-eq (get address volunteer) tx-sender) err-unauthorized)
    (asserts! (get active volunteer) err-not-found)
    (asserts! (get active opportunity) err-not-found)
    (asserts! (< (get current-volunteers opportunity) (get max-volunteers opportunity)) err-shelter-full)
    (asserts! (> hours-committed u0) err-invalid-amount)
    (map-set volunteer-assignments assignment-id
      {
        volunteer-id: volunteer-id,
        opportunity-id: opportunity-id,
        shelter-id: (get shelter-id opportunity),
        status: "pending",
        hours-committed: hours-committed,
        hours-completed: u0,
        assigned-at: stacks-block-height,
        completed-at: none,
        rating-from-shelter: none,
        rating-from-volunteer: none,
        feedback: ""
      })
    (var-set next-volunteer-assignment-id (+ assignment-id u1))
    (ok assignment-id)))

(define-public (approve-volunteer-assignment (assignment-id uint))
  (let ((assignment (unwrap! (map-get? volunteer-assignments assignment-id) err-not-found))
        (opportunity-id (get opportunity-id assignment))
        (volunteer-id (get volunteer-id assignment)))
    (let ((opportunity (unwrap! (map-get? volunteer-opportunities opportunity-id) err-not-found))
          (shelter (unwrap! (map-get? shelters (get shelter-id opportunity)) err-not-found)))
      (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
      (asserts! (is-eq (get status assignment) "pending") err-unauthorized)
      (map-set volunteer-assignments assignment-id
        (merge assignment {status: "approved"}))
      (map-set volunteer-opportunities opportunity-id
        (merge opportunity {current-volunteers: (+ (get current-volunteers opportunity) u1)}))
      (let ((volunteer (unwrap! (map-get? volunteers volunteer-id) err-not-found)))
        (map-set volunteers volunteer-id
          (merge volunteer {assignments-count: (+ (get assignments-count volunteer) u1)}))
        (ok true)))))

(define-public (complete-volunteer-assignment
  (assignment-id uint)
  (hours-completed uint)
  (rating-from-shelter uint)
  (feedback (string-ascii 500)))
  (let ((assignment (unwrap! (map-get? volunteer-assignments assignment-id) err-not-found))
        (volunteer-id (get volunteer-id assignment))
        (opportunity-id (get opportunity-id assignment)))
    (let ((opportunity (unwrap! (map-get? volunteer-opportunities opportunity-id) err-not-found))
          (shelter (unwrap! (map-get? shelters (get shelter-id opportunity)) err-not-found)))
      (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
      (asserts! (is-eq (get status assignment) "approved") err-unauthorized)
      (asserts! (> hours-completed u0) err-invalid-amount)
      (asserts! (and (>= rating-from-shelter u1) (<= rating-from-shelter u5)) err-invalid-amount)
      (map-set volunteer-assignments assignment-id
        (merge assignment {
          status: "completed",
          hours-completed: hours-completed,
          completed-at: (some stacks-block-height),
          rating-from-shelter: (some rating-from-shelter),
          feedback: feedback
        }))
      (let ((volunteer (unwrap! (map-get? volunteers volunteer-id) err-not-found)))
        (let ((new-total-hours (+ (get total-hours volunteer) hours-completed))
              (new-rating (/ (+ (* (get rating volunteer) (get assignments-count volunteer)) rating-from-shelter) 
                            (+ (get assignments-count volunteer) u1))))
          (map-set volunteers volunteer-id
            (merge volunteer {
              total-hours: new-total-hours,
              rating: new-rating
            }))
          (try! (award-volunteer-achievement volunteer-id new-total-hours))
          (ok true))))))

(define-public (rate-volunteer-experience
  (assignment-id uint)
  (rating-from-volunteer uint))
  (let ((assignment (unwrap! (map-get? volunteer-assignments assignment-id) err-not-found))
        (volunteer-id (get volunteer-id assignment)))
    (let ((volunteer (unwrap! (map-get? volunteers volunteer-id) err-not-found)))
      (asserts! (is-eq (get address volunteer) tx-sender) err-unauthorized)
      (asserts! (is-eq (get status assignment) "completed") err-unauthorized)
      (asserts! (and (>= rating-from-volunteer u1) (<= rating-from-volunteer u5)) err-invalid-amount)
      (map-set volunteer-assignments assignment-id
        (merge assignment {rating-from-volunteer: (some rating-from-volunteer)}))
      (ok true))))

(define-public (background-check-volunteer (volunteer-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? volunteers volunteer-id)
      volunteer (begin
        (map-set volunteers volunteer-id (merge volunteer {background-checked: true}))
        (ok true))
      err-not-found)))

(define-public (deactivate-volunteer-opportunity (opportunity-id uint))
  (let ((opportunity (unwrap! (map-get? volunteer-opportunities opportunity-id) err-not-found)))
    (let ((shelter (unwrap! (map-get? shelters (get shelter-id opportunity)) err-not-found)))
      (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
      (map-set volunteer-opportunities opportunity-id
        (merge opportunity {active: false}))
      (ok true))))

(define-private (award-volunteer-achievement (volunteer-id uint) (total-hours uint))
  (let ((volunteer (unwrap! (map-get? volunteers volunteer-id) err-not-found)))
    (if (and (>= total-hours u50) (is-none (map-get? volunteer-achievements {volunteer-id: volunteer-id, achievement-type: "50-hours"})))
      (begin
        (map-set volunteer-achievements {volunteer-id: volunteer-id, achievement-type: "50-hours"}
          {
            earned-at: stacks-block-height,
            description: "Dedicated Volunteer - 50+ hours of service",
            hours-milestone: u50
          })
        (ok true))
      (if (and (>= total-hours u100) (is-none (map-get? volunteer-achievements {volunteer-id: volunteer-id, achievement-type: "100-hours"})))
        (begin
          (map-set volunteer-achievements {volunteer-id: volunteer-id, achievement-type: "100-hours"}
            {
              earned-at: stacks-block-height,
              description: "Community Champion - 100+ hours of service",
              hours-milestone: u100
            })
          (ok true))
        (if (and (>= total-hours u200) (is-none (map-get? volunteer-achievements {volunteer-id: volunteer-id, achievement-type: "200-hours"})))
          (begin
            (map-set volunteer-achievements {volunteer-id: volunteer-id, achievement-type: "200-hours"}
              {
                earned-at: stacks-block-height,
                description: "Service Hero - 200+ hours of service",
                hours-milestone: u200
              })
            (ok true))
          (ok false))))))

(define-public (add-resource-to-inventory
  (shelter-id uint)
  (resource-type (string-ascii 50))
  (quantity uint)
  (unit (string-ascii 20))
  (expiry-date (optional uint))
  (low-stock-threshold uint)
  (notes (string-ascii 200)))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found))
        (resource-id (var-get next-resource-id)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (asserts! (get verified shelter) err-not-verified)
    (asserts! (> quantity u0) err-invalid-amount)
    (asserts! (> low-stock-threshold u0) err-invalid-amount)
    (match expiry-date
      expiry (asserts! (> expiry stacks-block-height) err-invalid-expiry)
      true)
    (map-set shelter-resources resource-id
      {
        shelter-id: shelter-id,
        resource-type: resource-type,
        quantity: quantity,
        unit: unit,
        expiry-date: expiry-date,
        low-stock-threshold: low-stock-threshold,
        last-updated: stacks-block-height,
        notes: notes
      })
    (var-set next-resource-id (+ resource-id u1))
    (unwrap-panic (check-and-create-alerts shelter-id resource-type quantity low-stock-threshold))
    (ok resource-id)))

(define-public (update-resource-quantity
  (resource-id uint)
  (new-quantity uint)
  (notes (string-ascii 200)))
  (let ((resource (unwrap! (map-get? shelter-resources resource-id) err-not-found)))
    (let ((shelter (unwrap! (map-get? shelters (get shelter-id resource)) err-not-found)))
      (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
      (map-set shelter-resources resource-id
        (merge resource {
          quantity: new-quantity,
          last-updated: stacks-block-height,
          notes: notes
        }))
      (unwrap-panic (record-resource-transaction 
        none 
        (get shelter-id resource) 
        resource-id 
        (if (> new-quantity (get quantity resource))
          (- new-quantity (get quantity resource))
          (- (get quantity resource) new-quantity))
        (if (> new-quantity (get quantity resource)) "restock" "consumption")
        notes))
      (unwrap-panic (check-and-create-alerts 
        (get shelter-id resource) 
        (get resource-type resource) 
        new-quantity 
        (get low-stock-threshold resource)))
      (ok true))))

(define-public (create-resource-request
  (shelter-id uint)
  (resource-type (string-ascii 50))
  (quantity-needed uint)
  (urgency-level uint)
  (description (string-ascii 300))
  (deadline (optional uint)))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found))
        (request-id (var-get next-resource-request-id)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (asserts! (get verified shelter) err-not-verified)
    (asserts! (> quantity-needed u0) err-invalid-amount)
    (asserts! (and (>= urgency-level u1) (<= urgency-level u5)) err-invalid-amount)
    (match deadline
      deadline-value (asserts! (> deadline-value stacks-block-height) err-invalid-expiry)
      true)
    (map-set resource-requests request-id
      {
        requesting-shelter-id: shelter-id,
        resource-type: resource-type,
        quantity-needed: quantity-needed,
        urgency-level: urgency-level,
        description: description,
        status: "open",
        created-at: stacks-block-height,
        deadline: deadline,
        fulfilled-by: none,
        fulfilled-at: none
      })
    (var-set next-resource-request-id (+ request-id u1))
    (ok request-id)))

(define-public (fulfill-resource-request
  (request-id uint)
  (fulfilling-shelter-id uint)
  (resource-id uint))
  (let ((request (unwrap! (map-get? resource-requests request-id) err-not-found))
        (resource (unwrap! (map-get? shelter-resources resource-id) err-not-found))
        (fulfilling-shelter (unwrap! (map-get? shelters fulfilling-shelter-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner fulfilling-shelter)) err-unauthorized)
    (asserts! (is-eq (get shelter-id resource) fulfilling-shelter-id) err-unauthorized)
    (asserts! (is-eq (get status request) "open") err-request-already-processed)
    (asserts! (is-eq (get resource-type resource) (get resource-type request)) err-invalid-resource-type)
    (asserts! (>= (get quantity resource) (get quantity-needed request)) err-insufficient-inventory)
    (let ((new-quantity (- (get quantity resource) (get quantity-needed request))))
      (map-set shelter-resources resource-id
        (merge resource {
          quantity: new-quantity,
          last-updated: stacks-block-height
        }))
      (map-set resource-requests request-id
        (merge request {
          status: "fulfilled",
          fulfilled-by: (some fulfilling-shelter-id),
          fulfilled-at: (some stacks-block-height)
        }))
      (unwrap-panic (record-resource-transaction
        (some fulfilling-shelter-id)
        (get requesting-shelter-id request)
        resource-id
        (get quantity-needed request)
        "transfer"
        (get description request)))
      (unwrap-panic (check-and-create-alerts 
        fulfilling-shelter-id 
        (get resource-type resource) 
        new-quantity 
        (get low-stock-threshold resource)))
      (ok true))))

(define-public (transfer-resource-between-shelters
  (from-shelter-id uint)
  (to-shelter-id uint)
  (resource-id uint)
  (transfer-quantity uint)
  (notes (string-ascii 200)))
  (let ((resource (unwrap! (map-get? shelter-resources resource-id) err-not-found))
        (from-shelter (unwrap! (map-get? shelters from-shelter-id) err-not-found))
        (to-shelter (unwrap! (map-get? shelters to-shelter-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner from-shelter)) err-unauthorized)
    (asserts! (is-eq (get shelter-id resource) from-shelter-id) err-unauthorized)
    (asserts! (get verified to-shelter) err-not-verified)
    (asserts! (> transfer-quantity u0) err-invalid-amount)
    (asserts! (>= (get quantity resource) transfer-quantity) err-insufficient-inventory)
    (let ((new-quantity (- (get quantity resource) transfer-quantity))
          (to-resource-id (var-get next-resource-id)))
      (map-set shelter-resources resource-id
        (merge resource {
          quantity: new-quantity,
          last-updated: stacks-block-height
        }))
      (map-set shelter-resources to-resource-id
        {
          shelter-id: to-shelter-id,
          resource-type: (get resource-type resource),
          quantity: transfer-quantity,
          unit: (get unit resource),
          expiry-date: (get expiry-date resource),
          low-stock-threshold: (get low-stock-threshold resource),
          last-updated: stacks-block-height,
          notes: notes
        })
      (var-set next-resource-id (+ to-resource-id u1))
      (unwrap-panic (record-resource-transaction
        (some from-shelter-id)
        to-shelter-id
        resource-id
        transfer-quantity
        "transfer"
        notes))
      (unwrap-panic (check-and-create-alerts 
        from-shelter-id 
        (get resource-type resource) 
        new-quantity 
        (get low-stock-threshold resource)))
      (ok to-resource-id))))

(define-public (mark-resource-expired
  (resource-id uint))
  (let ((resource (unwrap! (map-get? shelter-resources resource-id) err-not-found)))
    (let ((shelter (unwrap! (map-get? shelters (get shelter-id resource)) err-not-found)))
      (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
      (match (get expiry-date resource)
        expiry (asserts! (<= expiry stacks-block-height) err-invalid-expiry)
        (asserts! false err-not-found))
      (map-set shelter-resources resource-id
        (merge resource {
          quantity: u0,
          last-updated: stacks-block-height,
          notes: "Marked as expired"
        }))
      (unwrap-panic (record-resource-transaction
        none
        (get shelter-id resource)
        resource-id
        (get quantity resource)
        "expired"
        "Resource marked as expired"))
      (ok true))))

(define-public (resolve-resource-alert
  (shelter-id uint)
  (resource-type (string-ascii 50)))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (let ((alert-key {shelter-id: shelter-id, resource-type: resource-type}))
      (match (map-get? resource-alerts alert-key)
        alert (begin
          (map-set resource-alerts alert-key
            (merge alert {
              resolved: true,
              resolved-at: (some stacks-block-height)
            }))
          (ok true))
        err-not-found))))

(define-private (record-resource-transaction
  (from-shelter-id (optional uint))
  (to-shelter-id uint)
  (resource-id uint)
  (quantity uint)
  (transaction-type (string-ascii 20))
  (notes (string-ascii 300)))
  (let ((transaction-id (var-get next-resource-transaction-id)))
    (map-set resource-transactions transaction-id
      {
        from-shelter-id: from-shelter-id,
        to-shelter-id: to-shelter-id,
        resource-id: resource-id,
        quantity: quantity,
        transaction-type: transaction-type,
        created-at: stacks-block-height,
        notes: notes,
        authorized-by: tx-sender
      })
    (var-set next-resource-transaction-id (+ transaction-id u1))
    (ok transaction-id)))

(define-private (check-and-create-alerts
  (shelter-id uint)
  (resource-type (string-ascii 50))
  (current-quantity uint)
  (threshold uint))
  (let ((alert-key {shelter-id: shelter-id, resource-type: resource-type}))
    (if (<= current-quantity threshold)
      (begin
        (map-set resource-alerts alert-key
          {
            alert-type: "low-stock",
            triggered-at: stacks-block-height,
            resolved: false,
            resolved-at: none
          })
        (ok true))
      (ok false))))

(define-read-only (get-resource (resource-id uint))
  (map-get? shelter-resources resource-id))

(define-read-only (get-resource-request (request-id uint))
  (map-get? resource-requests request-id))

(define-read-only (get-resource-transaction (transaction-id uint))
  (map-get? resource-transactions transaction-id))

(define-read-only (get-resource-alert (shelter-id uint) (resource-type (string-ascii 50)))
  (map-get? resource-alerts {shelter-id: shelter-id, resource-type: resource-type}))

(define-read-only (get-shelter-inventory-summary (shelter-id uint))
  (let ((resource-ids (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)))
    (ok {
      total-resources: (var-get next-resource-id),
      low-stock-count: u0,
      expired-count: u0,
      last-updated: stacks-block-height
    })))

(define-read-only (get-resource-stats (resource-type (string-ascii 50)))
  (ok {
    total-shelters-with-resource: u0,
    total-quantity: u0,
    average-stock-level: u0,
    total-requests: u0
  }))

(define-read-only (get-volunteer (volunteer-id uint))
  (map-get? volunteers volunteer-id))

(define-read-only (get-volunteer-opportunity (opportunity-id uint))
  (map-get? volunteer-opportunities opportunity-id))

(define-read-only (get-volunteer-assignment (assignment-id uint))
  (map-get? volunteer-assignments assignment-id))

(define-read-only (get-volunteer-achievement (volunteer-id uint) (achievement-type (string-ascii 50)))
  (map-get? volunteer-achievements {volunteer-id: volunteer-id, achievement-type: achievement-type}))

(define-read-only (get-volunteer-stats (volunteer-id uint))
  (match (map-get? volunteers volunteer-id)
    volunteer (ok {
      total-hours: (get total-hours volunteer),
      assignments-count: (get assignments-count volunteer),
      rating: (get rating volunteer),
      background-checked: (get background-checked volunteer),
      active: (get active volunteer)
    })
    err-not-found))

(define-read-only (get-opportunity-stats (opportunity-id uint))
  (match (map-get? volunteer-opportunities opportunity-id)
    opportunity (ok {
      current-volunteers: (get current-volunteers opportunity),
      max-volunteers: (get max-volunteers opportunity),
      available-spots: (- (get max-volunteers opportunity) (get current-volunteers opportunity)),
      fill-rate: (/ (* (get current-volunteers opportunity) u100) (get max-volunteers opportunity)),
      urgent: (get urgent opportunity),
      active: (get active opportunity)
    })
    err-not-found))

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

;; Emergency Status Management Functions
(define-public (declare-emergency
  (shelter-id uint)
  (emergency-type (string-ascii 20))
  (priority uint)
  (message (string-ascii 300))
  (contact-override (optional (string-ascii 100))))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (asserts! (get verified shelter) err-not-verified)
    (asserts! (and (>= priority u1) (<= priority u5)) err-invalid-priority)
    (asserts! (is-none (map-get? emergency-status shelter-id)) err-emergency-active)
    (map-set emergency-status shelter-id
      {
        emergency-type: emergency-type,
        priority: priority,
        message: message,
        declared-at: stacks-block-height,
        resolved: false,
        resolved-at: none,
        contact-override: contact-override
      })
    (ok true)))

(define-public (resolve-emergency (shelter-id uint))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found))
        (emergency (unwrap! (map-get? emergency-status shelter-id) err-no-emergency)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (asserts! (not (get resolved emergency)) err-no-emergency)
    (map-set emergency-status shelter-id
      (merge emergency {
        resolved: true,
        resolved-at: (some stacks-block-height)
      }))
    (ok true)))

(define-public (update-emergency-message
  (shelter-id uint)
  (new-message (string-ascii 300)))
  (let ((shelter (unwrap! (map-get? shelters shelter-id) err-not-found))
        (emergency (unwrap! (map-get? emergency-status shelter-id) err-no-emergency)))
    (asserts! (is-eq tx-sender (get owner shelter)) err-unauthorized)
    (asserts! (not (get resolved emergency)) err-no-emergency)
    (map-set emergency-status shelter-id
      (merge emergency {message: new-message}))
    (ok true)))

;; Emergency Status Query Functions
(define-read-only (get-emergency-status (shelter-id uint))
  (map-get? emergency-status shelter-id))

(define-read-only (get-active-emergencies)
  (let ((shelter-list (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)))
    (ok (filter is-emergency-active shelter-list))))

(define-read-only (get-high-priority-emergencies)
  (let ((shelter-list (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)))
    (ok (filter is-high-priority-emergency shelter-list))))

(define-read-only (get-emergency-summary (shelter-id uint))
  (match (get-emergency-status shelter-id)
    emergency (some {
      shelter-id: shelter-id,
      emergency-type: (get emergency-type emergency),
      priority: (get priority emergency),
      message: (get message emergency),
      hours-since-declared: (- stacks-block-height (get declared-at emergency)),
      resolved: (get resolved emergency),
      shelter-info: (unwrap-panic (get-shelter shelter-id))
    })
    none))

;; Private helper functions for emergency filtering
(define-private (is-emergency-active (shelter-id uint))
  (match (map-get? emergency-status shelter-id)
    emergency (not (get resolved emergency))
    false))

(define-private (is-high-priority-emergency (shelter-id uint))
  (match (map-get? emergency-status shelter-id)
    emergency (and (>= (get priority emergency) u4) (not (get resolved emergency)))
    false))

