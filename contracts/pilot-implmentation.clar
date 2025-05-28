;; Pilot Implementation Contract
;; Manages innovation testing phases

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-already-exists (err u302))
(define-constant err-unauthorized (err u303))
(define-constant err-invalid-status (err u304))
(define-constant err-pilot-closed (err u305))

;; Data Variables
(define-data-var next-pilot-id uint u1)

;; Data Maps
(define-map pilots
  { pilot-id: uint }
  {
    assessment-id: uint,
    entity-id: uint,
    pilot-name: (string-ascii 100),
    description: (string-ascii 500),
    status: (string-ascii 20),
    start-date: uint,
    end-date: (optional uint),
    budget-allocated: uint,
    budget-used: uint,
    success-metrics: (string-ascii 300),
    created-at: uint
  }
)

(define-map pilot-progress
  { pilot-id: uint, milestone-id: uint }
  {
    milestone-name: (string-ascii 100),
    description: (string-ascii 300),
    target-date: uint,
    completion-date: (optional uint),
    status: (string-ascii 20),
    notes: (string-ascii 500)
  }
)

(define-map pilot-results
  { pilot-id: uint }
  {
    environmental-impact: (string-ascii 500),
    economic-impact: (string-ascii 500),
    technical-performance: (string-ascii 500),
    lessons-learned: (string-ascii 500),
    recommendation: (string-ascii 20),
    submitted-at: uint
  }
)

(define-map pilot-managers
  { manager: principal }
  { authorized: bool }
)

;; Public Functions

;; Create new pilot program
(define-public (create-pilot
  (assessment-id uint)
  (entity-id uint)
  (pilot-name (string-ascii 100))
  (description (string-ascii 500))
  (budget-allocated uint)
  (success-metrics (string-ascii 300)))
  (let
    (
      (pilot-id (var-get next-pilot-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)

    (map-set pilots
      { pilot-id: pilot-id }
      {
        assessment-id: assessment-id,
        entity-id: entity-id,
        pilot-name: pilot-name,
        description: description,
        status: "planned",
        start-date: block-height,
        end-date: none,
        budget-allocated: budget-allocated,
        budget-used: u0,
        success-metrics: success-metrics,
        created-at: block-height
      }
    )

    (var-set next-pilot-id (+ pilot-id u1))
    (ok pilot-id)
  )
)

;; Start pilot implementation
(define-public (start-pilot (pilot-id uint))
  (let
    (
      (pilot (unwrap! (map-get? pilots { pilot-id: pilot-id }) err-not-found))
    )
    (asserts! (default-to false (get authorized (map-get? pilot-managers { manager: tx-sender }))) err-unauthorized)
    (asserts! (is-eq (get status pilot) "planned") err-invalid-status)

    (map-set pilots
      { pilot-id: pilot-id }
      (merge pilot { status: "active" })
    )
    (ok true)
  )
)

;; Add milestone to pilot
(define-public (add-milestone
  (pilot-id uint)
  (milestone-id uint)
  (milestone-name (string-ascii 100))
  (description (string-ascii 300))
  (target-date uint))
  (let
    (
      (pilot (unwrap! (map-get? pilots { pilot-id: pilot-id }) err-not-found))
    )
    (asserts! (default-to false (get authorized (map-get? pilot-managers { manager: tx-sender }))) err-unauthorized)
    (asserts! (is-none (map-get? pilot-progress { pilot-id: pilot-id, milestone-id: milestone-id })) err-already-exists)

    (map-set pilot-progress
      { pilot-id: pilot-id, milestone-id: milestone-id }
      {
        milestone-name: milestone-name,
        description: description,
        target-date: target-date,
        completion-date: none,
        status: "pending",
        notes: ""
      }
    )
    (ok true)
  )
)

;; Update milestone progress
(define-public (update-milestone
  (pilot-id uint)
  (milestone-id uint)
  (status (string-ascii 20))
  (notes (string-ascii 500)))
  (let
    (
      (milestone (unwrap! (map-get? pilot-progress { pilot-id: pilot-id, milestone-id: milestone-id }) err-not-found))
    )
    (asserts! (default-to false (get authorized (map-get? pilot-managers { manager: tx-sender }))) err-unauthorized)
    (asserts! (or (is-eq status "pending") (is-eq status "completed") (is-eq status "delayed")) err-invalid-status)

    (map-set pilot-progress
      { pilot-id: pilot-id, milestone-id: milestone-id }
      (merge milestone {
        status: status,
        notes: notes,
        completion-date: (if (is-eq status "completed") (some block-height) none)
      })
    )
    (ok true)
  )
)

;; Update budget usage
(define-public (update-budget-usage (pilot-id uint) (amount-used uint))
  (let
    (
      (pilot (unwrap! (map-get? pilots { pilot-id: pilot-id }) err-not-found))
    )
    (asserts! (default-to false (get authorized (map-get? pilot-managers { manager: tx-sender }))) err-unauthorized)
    (asserts! (<= amount-used (get budget-allocated pilot)) err-invalid-status)

    (map-set pilots
      { pilot-id: pilot-id }
      (merge pilot { budget-used: amount-used })
    )
    (ok true)
  )
)

;; Submit pilot results
(define-public (submit-pilot-results
  (pilot-id uint)
  (environmental-impact (string-ascii 500))
  (economic-impact (string-ascii 500))
  (technical-performance (string-ascii 500))
  (lessons-learned (string-ascii 500))
  (recommendation (string-ascii 20)))
  (let
    (
      (pilot (unwrap! (map-get? pilots { pilot-id: pilot-id }) err-not-found))
    )
    (asserts! (default-to false (get authorized (map-get? pilot-managers { manager: tx-sender }))) err-unauthorized)
    (asserts! (is-eq (get status pilot) "active") err-pilot-closed)

    (map-set pilot-results
      { pilot-id: pilot-id }
      {
        environmental-impact: environmental-impact,
        economic-impact: economic-impact,
        technical-performance: technical-performance,
        lessons-learned: lessons-learned,
        recommendation: recommendation,
        submitted-at: block-height
      }
    )

    (map-set pilots
      { pilot-id: pilot-id }
      (merge pilot {
        status: "completed",
        end-date: (some block-height)
      })
    )
    (ok true)
  )
)

;; Add pilot manager (owner only)
(define-public (add-pilot-manager (manager principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set pilot-managers { manager: manager } { authorized: true })
    (ok true)
  )
)

;; Remove pilot manager (owner only)
(define-public (remove-pilot-manager (manager principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set pilot-managers { manager: manager } { authorized: false })
    (ok true)
  )
)

;; Read-only Functions

;; Get pilot details
(define-read-only (get-pilot (pilot-id uint))
  (map-get? pilots { pilot-id: pilot-id })
)

;; Get milestone details
(define-read-only (get-milestone (pilot-id uint) (milestone-id uint))
  (map-get? pilot-progress { pilot-id: pilot-id, milestone-id: milestone-id })
)

;; Get pilot results
(define-read-only (get-pilot-results (pilot-id uint))
  (map-get? pilot-results { pilot-id: pilot-id })
)

;; Check if manager is authorized
(define-read-only (is-authorized-manager (manager principal))
  (default-to false (get authorized (map-get? pilot-managers { manager: manager })))
)

;; Get budget utilization percentage
(define-read-only (get-budget-utilization (pilot-id uint))
  (match (map-get? pilots { pilot-id: pilot-id })
    pilot
      (if (> (get budget-allocated pilot) u0)
        (some (/ (* (get budget-used pilot) u100) (get budget-allocated pilot)))
        none)
    none
  )
)

;; Get next pilot ID
(define-read-only (get-next-pilot-id)
  (var-get next-pilot-id)
)
