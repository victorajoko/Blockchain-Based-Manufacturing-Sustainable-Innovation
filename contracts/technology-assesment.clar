;; Technology Assessment Contract
;; Evaluates sustainability innovations

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-already-exists (err u202))
(define-constant err-unauthorized (err u203))
(define-constant err-invalid-score (err u204))
(define-constant err-assessment-closed (err u205))

;; Data Variables
(define-data-var next-assessment-id uint u1)

;; Data Maps
(define-map assessments
  { assessment-id: uint }
  {
    entity-id: uint,
    technology-name: (string-ascii 100),
    description: (string-ascii 500),
    sustainability-category: (string-ascii 50),
    status: (string-ascii 20),
    total-score: uint,
    assessment-count: uint,
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map assessment-scores
  { assessment-id: uint, assessor: principal }
  {
    environmental-impact: uint,
    economic-viability: uint,
    technical-feasibility: uint,
    scalability: uint,
    innovation-level: uint,
    comments: (string-ascii 500),
    submitted-at: uint
  }
)

(define-map assessors
  { assessor: principal }
  { authorized: bool, expertise: (string-ascii 100) }
)

;; Public Functions

;; Submit technology for assessment
(define-public (submit-technology
  (entity-id uint)
  (technology-name (string-ascii 100))
  (description (string-ascii 500))
  (sustainability-category (string-ascii 50)))
  (let
    (
      (assessment-id (var-get next-assessment-id))
    )
    ;; Verify entity exists and is verified (would call verification contract)

    (map-set assessments
      { assessment-id: assessment-id }
      {
        entity-id: entity-id,
        technology-name: technology-name,
        description: description,
        sustainability-category: sustainability-category,
        status: "open",
        total-score: u0,
        assessment-count: u0,
        created-at: block-height,
        completed-at: none
      }
    )

    (var-set next-assessment-id (+ assessment-id u1))
    (ok assessment-id)
  )
)

;; Submit assessment scores (assessors only)
(define-public (submit-assessment-score
  (assessment-id uint)
  (environmental-impact uint)
  (economic-viability uint)
  (technical-feasibility uint)
  (scalability uint)
  (innovation-level uint)
  (comments (string-ascii 500)))
  (let
    (
      (assessment (unwrap! (map-get? assessments { assessment-id: assessment-id }) err-not-found))
      (caller tx-sender)
      (existing-score (map-get? assessment-scores { assessment-id: assessment-id, assessor: caller }))
    )
    (asserts! (default-to false (get authorized (map-get? assessors { assessor: caller }))) err-unauthorized)
    (asserts! (is-eq (get status assessment) "open") err-assessment-closed)
    (asserts! (and (<= environmental-impact u100) (<= economic-viability u100)
                   (<= technical-feasibility u100) (<= scalability u100)
                   (<= innovation-level u100)) err-invalid-score)

    ;; Store individual assessment
    (map-set assessment-scores
      { assessment-id: assessment-id, assessor: caller }
      {
        environmental-impact: environmental-impact,
        economic-viability: economic-viability,
        technical-feasibility: technical-feasibility,
        scalability: scalability,
        innovation-level: innovation-level,
        comments: comments,
        submitted-at: block-height
      }
    )

    ;; Update assessment totals
    (let
      (
        (score-sum (+ environmental-impact economic-viability technical-feasibility scalability innovation-level))
        (current-total (get total-score assessment))
        (current-count (get assessment-count assessment))
        (is-new-assessment (is-none existing-score))
        (new-total (if is-new-assessment (+ current-total score-sum) current-total))
        (new-count (if is-new-assessment (+ current-count u1) current-count))
      )
      (map-set assessments
        { assessment-id: assessment-id }
        (merge assessment {
          total-score: new-total,
          assessment-count: new-count
        })
      )
    )

    (ok true)
  )
)

;; Close assessment (owner only)
(define-public (close-assessment (assessment-id uint))
  (let
    (
      (assessment (unwrap! (map-get? assessments { assessment-id: assessment-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status assessment) "open") err-assessment-closed)

    (map-set assessments
      { assessment-id: assessment-id }
      (merge assessment {
        status: "closed",
        completed-at: (some block-height)
      })
    )
    (ok true)
  )
)

;; Add authorized assessor (owner only)
(define-public (add-assessor (assessor principal) (expertise (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set assessors
      { assessor: assessor }
      { authorized: true, expertise: expertise }
    )
    (ok true)
  )
)

;; Remove assessor (owner only)
(define-public (remove-assessor (assessor principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set assessors
      { assessor: assessor }
      { authorized: false, expertise: "" }
    )
    (ok true)
  )
)

;; Read-only Functions

;; Get assessment details
(define-read-only (get-assessment (assessment-id uint))
  (map-get? assessments { assessment-id: assessment-id })
)

;; Get assessment score from specific assessor
(define-read-only (get-assessment-score (assessment-id uint) (assessor principal))
  (map-get? assessment-scores { assessment-id: assessment-id, assessor: assessor })
)

;; Calculate average score
(define-read-only (get-average-score (assessment-id uint))
  (match (map-get? assessments { assessment-id: assessment-id })
    assessment
      (if (> (get assessment-count assessment) u0)
        (some (/ (get total-score assessment) (* (get assessment-count assessment) u5)))
        none)
    none
  )
)

;; Check if assessor is authorized
(define-read-only (is-authorized-assessor (assessor principal))
  (default-to false (get authorized (map-get? assessors { assessor: assessor })))
)

;; Get assessor expertise
(define-read-only (get-assessor-expertise (assessor principal))
  (get expertise (map-get? assessors { assessor: assessor }))
)

;; Get next assessment ID
(define-read-only (get-next-assessment-id)
  (var-get next-assessment-id)
)
