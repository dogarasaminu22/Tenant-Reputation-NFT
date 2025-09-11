;; ===== TENANT REFERENCE SYSTEM =====
;; A comprehensive reference system for tenants to request and receive
;; verified references from previous landlords, enhancing tenant credibility

;; Error constants for reference system
(define-constant err-reference-not-found (err u500))
(define-constant err-request-not-found (err u501))
(define-constant err-already-requested (err u502))
(define-constant err-reference-expired (err u503))
(define-constant err-invalid-verification-code (err u504))
(define-constant err-request-expired (err u505))
(define-constant err-self-reference (err u506))
(define-constant err-no-rental-history (err u507))
(define-constant err-not-authorized (err u100))
(define-constant err-invalid-rating (err u200))

;; Constants for reference system
(define-constant REFERENCE-VALIDITY-PERIOD u52560) ;; 1 year in blocks
(define-constant REQUEST-EXPIRY-PERIOD u4320) ;; 30 days in blocks
(define-constant MAX-REFERENCES-PER-TENANT u20)

;; Data variables
(define-data-var last-reference-id uint u0)
(define-data-var last-request-id uint u0)

;; Reference request data structure
(define-map reference-requests
    uint
    {
        request-id: uint,
        tenant: principal,
        landlord: principal,
        property-address: (string-ascii 100),
        request-date: uint,
        expiry-date: uint,
        status: (string-ascii 20),
        message: (string-ascii 200)
    }
)

;; Verified tenant references
(define-map tenant-references
    uint
    {
        reference-id: uint,
        tenant: principal,
        landlord: principal,
        property-address: (string-ascii 100),
        lease-start: uint,
        lease-end: uint,
        reference-text: (string-ascii 800),
        payment-reliability: uint,
        property-care: uint,
        communication: uint,
        overall-rating: uint,
        would-rent-again: bool,
        verification-code: (string-ascii 32),
        issue-date: uint,
        expiry-date: uint,
        status: (string-ascii 20)
    }
)

;; Track reference requests per tenant
(define-map tenant-reference-requests
    principal
    (list 10 uint)
)

;; Track verified references per tenant
(define-map tenant-reference-history
    principal
    (list 20 uint)
)

;; Track reference statistics per tenant
(define-map tenant-reference-stats
    principal
    {
        total-references: uint,
        average-payment-reliability: uint,
        average-property-care: uint,
        average-communication: uint,
        overall-reference-score: uint,
        recommendation-percentage: uint
    }
)

;; Request a reference from a previous landlord
(define-public (request-tenant-reference
    (landlord principal)
    (property-address (string-ascii 100))
    (message (string-ascii 200)))
    (let
        ((new-request-id (+ (var-get last-request-id) u1))
         (current-time stacks-block-height)
         (expiry-date (+ current-time REQUEST-EXPIRY-PERIOD)))
        
        (asserts! (not (is-eq tx-sender landlord)) err-self-reference)
        ;; Check if landlord has reputation (exists) by calling main contract
        (asserts! (is-some (unwrap-panic (contract-call? .tenantRep get-landlord-reputation landlord))) err-not-authorized)
        
        ;; Check if tenant has rental history by calling main contract
        (asserts! (is-some (unwrap-panic (contract-call? .tenantRep get-tenant-history tx-sender))) err-no-rental-history)
        
        ;; Create reference request
        (map-set reference-requests new-request-id
            {
                request-id: new-request-id,
                tenant: tx-sender,
                landlord: landlord,
                property-address: property-address,
                request-date: current-time,
                expiry-date: expiry-date,
                status: "pending",
                message: message
            }
        )
        
        ;; Update tenant's request history
        (match (map-get? tenant-reference-requests tx-sender)
            existing-requests
                (map-set tenant-reference-requests tx-sender
                    (unwrap-panic (as-max-len? (concat existing-requests (list new-request-id)) u10))
                )
            (map-set tenant-reference-requests tx-sender (list new-request-id))
        )
        
        (var-set last-request-id new-request-id)
        (ok new-request-id)
    )
)

;; Provide a reference for a tenant (landlord function)
(define-public (provide-tenant-reference
    (request-id uint)
    (lease-start uint)
    (lease-end uint)
    (reference-text (string-ascii 800))
    (payment-reliability uint)
    (property-care uint)
    (communication uint)
    (would-rent-again bool))
    (let
        ((request (unwrap! (map-get? reference-requests request-id) err-request-not-found))
         (new-reference-id (+ (var-get last-reference-id) u1))
         (current-time stacks-block-height)
         (expiry-date (+ current-time REFERENCE-VALIDITY-PERIOD))
         (overall-rating (/ (+ payment-reliability property-care communication) u3))
         (verification-code (int-to-ascii (+ (* new-reference-id u1000) current-time))))
        
        (asserts! (is-eq tx-sender (get landlord request)) err-not-authorized)
        (asserts! (is-eq (get status request) "pending") err-request-not-found)
        (asserts! (<= current-time (get expiry-date request)) err-request-expired)
        (asserts! (<= payment-reliability u5) err-invalid-rating)
        (asserts! (<= property-care u5) err-invalid-rating)
        (asserts! (<= communication u5) err-invalid-rating)
        (asserts! (> payment-reliability u0) err-invalid-rating)
        (asserts! (> property-care u0) err-invalid-rating)
        (asserts! (> communication u0) err-invalid-rating)
        
        ;; Create verified reference
        (map-set tenant-references new-reference-id
            {
                reference-id: new-reference-id,
                tenant: (get tenant request),
                landlord: tx-sender,
                property-address: (get property-address request),
                lease-start: lease-start,
                lease-end: lease-end,
                reference-text: reference-text,
                payment-reliability: payment-reliability,
                property-care: property-care,
                communication: communication,
                overall-rating: overall-rating,
                would-rent-again: would-rent-again,
                verification-code: (unwrap-panic (as-max-len? verification-code u32)),
                issue-date: current-time,
                expiry-date: expiry-date,
                status: "active"
            }
        )
        
        ;; Update request status
        (map-set reference-requests request-id
            (merge request { status: "completed" })
        )
        
        ;; Add to tenant's reference history
        (match (map-get? tenant-reference-history (get tenant request))
            existing-refs
                (map-set tenant-reference-history (get tenant request)
                    (unwrap-panic (as-max-len? (concat existing-refs (list new-reference-id)) u20))
                )
            (map-set tenant-reference-history (get tenant request) (list new-reference-id))
        )
        
        ;; Update tenant reference statistics
        (try! (update-tenant-reference-stats (get tenant request)))
        
        (var-set last-reference-id new-reference-id)
        (ok new-reference-id)
    )
)

;; Update tenant reference statistics
(define-public (update-tenant-reference-stats (tenant principal))
    (let
        ((reference-history (default-to (list) (map-get? tenant-reference-history tenant)))
         (total-refs (len reference-history))
         (payment-sum (fold + (map get-payment-reliability-score reference-history) u0))
         (care-sum (fold + (map get-property-care-score reference-history) u0))
         (comm-sum (fold + (map get-communication-score reference-history) u0))
         (recommendations (fold + (map get-recommendation-value reference-history) u0)))
        
        (asserts! (> total-refs u0) err-reference-not-found)
        
        (ok (map-set tenant-reference-stats tenant
            {
                total-references: total-refs,
                average-payment-reliability: (/ payment-sum total-refs),
                average-property-care: (/ care-sum total-refs),
                average-communication: (/ comm-sum total-refs),
                overall-reference-score: (/ (+ payment-sum care-sum comm-sum) (* total-refs u3)),
                recommendation-percentage: (/ (* recommendations u100) total-refs)
            }
        ))
    )
)

;; Helper functions for statistics calculation
(define-private (get-payment-reliability-score (reference-id uint))
    (match (map-get? tenant-references reference-id)
        reference (get payment-reliability reference)
        u0
    )
)

(define-private (get-property-care-score (reference-id uint))
    (match (map-get? tenant-references reference-id)
        reference (get property-care reference)
        u0
    )
)

(define-private (get-communication-score (reference-id uint))
    (match (map-get? tenant-references reference-id)
        reference (get communication reference)
        u0
    )
)

(define-private (get-recommendation-value (reference-id uint))
    (match (map-get? tenant-references reference-id)
        reference (if (get would-rent-again reference) u1 u0)
        u0
    )
)

;; Verify a reference using verification code
(define-read-only (verify-tenant-reference (reference-id uint) (verification-code (string-ascii 32)))
    (match (map-get? tenant-references reference-id)
        reference 
            (ok (and 
                (is-eq (get verification-code reference) verification-code)
                (is-eq (get status reference) "active")
                (<= stacks-block-height (get expiry-date reference))
            ))
        (ok false)
    )
)

;; Read-only functions
(define-read-only (get-reference-request (request-id uint))
    (ok (map-get? reference-requests request-id))
)

(define-read-only (get-tenant-reference (reference-id uint))
    (ok (map-get? tenant-references reference-id))
)

(define-read-only (get-tenant-reference-history (tenant principal))
    (ok (map-get? tenant-reference-history tenant))
)

(define-read-only (get-tenant-reference-stats (tenant principal))
    (ok (map-get? tenant-reference-stats tenant))
)

(define-read-only (get-last-reference-id)
    (ok (var-get last-reference-id))
)

(define-read-only (get-last-request-id)
    (ok (var-get last-request-id))
)

;; Check if tenant is highly recommended
(define-read-only (is-tenant-highly-recommended (tenant principal))
    (match (map-get? tenant-reference-stats tenant)
        stats (ok (and 
            (>= (get overall-reference-score stats) u4)
            (>= (get recommendation-percentage stats) u80)
            (>= (get total-references stats) u3)
        ))
        (ok false)
    )
)

;; Get active references for a tenant
(define-read-only (get-active-tenant-references (tenant principal))
    (let
        ((reference-history (default-to (list) (map-get? tenant-reference-history tenant))))
        (ok (filter is-reference-active reference-history))
    )
)

(define-private (is-reference-active (reference-id uint))
    (match (map-get? tenant-references reference-id)
        reference (and 
            (is-eq (get status reference) "active")
            (<= stacks-block-height (get expiry-date reference))
        )
        false
    )
)
