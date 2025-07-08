
(define-non-fungible-token tenant-rep uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))


(define-constant ESCROW-TIMEOUT u4320)
(define-constant DISPUTE-RESOLUTION-PERIOD u2880)
(define-constant PENALTY-RATE u5)

(define-map escrow-agreements
    uint
    {
        tenant: principal,
        landlord: principal,
        property-address: (string-ascii 100),
        monthly-rent: uint,
        deposit-amount: uint,
        start-date: uint,
        end-date: uint,
        payment-frequency: uint,
        total-escrowed: uint,
        status: (string-ascii 20),
        next-payment-due: uint,
        dispute-status: (string-ascii 20),
        dispute-raised-date: uint,
        auto-release-enabled: bool
    }
)

(define-map escrow-balances
    uint
    {
        tenant-balance: uint,
        landlord-balance: uint,
        pending-release: uint,
        penalty-pool: uint
    }
)

(define-map payment-history
    uint
    (list 50 {
        payment-date: uint,
        amount: uint,
        payment-type: (string-ascii 20),
        released-date: uint,
        status: (string-ascii 20)
    })
)

(define-map escrow-disputes
    uint
    {
        dispute-id: uint,
        disputed-amount: uint,
        reason: (string-ascii 300),
        evidence: (string-ascii 500),
        resolution-deadline: uint,
        arbitrator: (optional principal),
        resolution: (optional (string-ascii 300))
    }
)

(define-data-var last-escrow-id uint u0)
(define-data-var last-dispute-id uint u0)
(define-data-var platform-fee-rate uint u250)

(define-constant err-escrow-not-found (err u300))
(define-constant err-insufficient-funds (err u301))
(define-constant err-escrow-expired (err u302))
(define-constant err-payment-overdue (err u303))
(define-constant err-dispute-active (err u304))
(define-constant err-invalid-amount (err u305))
(define-constant err-escrow-closed (err u306))
(define-constant err-unauthorized-party (err u307))
(define-constant err-dispute-expired (err u308))

(define-map tenant-records
    uint 
    {
        tenant: principal,
        landlord: principal,
        property-address: (string-ascii 100),
        start-date: uint,
        end-date: uint,
        rating: uint,
        review: (string-ascii 500)
    }
)

(define-map landlords principal bool)
(define-map tenant-history 
    principal 
    (list 10 uint)
)


(define-map landlord-ratings
    uint
    {
        rater: principal,
        landlord: principal,
        property-address: (string-ascii 100),
        responsiveness-rating: uint,
        maintenance-rating: uint,
        communication-rating: uint,
        overall-rating: uint,
        review: (string-ascii 500),
        rating-date: uint
    }
)

(define-map landlord-rating-history
    principal
    (list 50 uint)
)

(define-map landlord-reputation
    principal
    {
        total-ratings: uint,
        average-responsiveness: uint,
        average-maintenance: uint,
        average-communication: uint,
        overall-average: uint,
        total-properties-managed: uint
    }
)

(define-data-var last-rating-id uint u0)

(define-constant err-invalid-rating (err u200))
(define-constant err-already-rated (err u201))
(define-constant err-not-tenant (err u202))
(define-constant err-rating-not-found (err u203))

(define-data-var last-token-id uint u0)

(define-public (register-landlord)
    (begin
        (asserts! (is-none (map-get? landlords tx-sender)) err-already-exists)
        (ok (map-set landlords tx-sender true))
    )
)

(define-public (create-tenant-record 
    (tenant principal)
    (property-address (string-ascii 100))
    (start-date uint)
    (end-date uint)
    (rating uint)
    (review (string-ascii 500)))
    (let
        ((new-id (+ (var-get last-token-id) u1)))
        (asserts! (is-some (map-get? landlords tx-sender)) err-not-authorized)
        (asserts! (<= rating u5) (err u103))
        (try! (nft-mint? tenant-rep new-id tenant))
        (map-set tenant-records new-id
            {
                tenant: tenant,
                landlord: tx-sender,
                property-address: property-address,
                start-date: start-date,
                end-date: end-date,
                rating: rating,
                review: review
            }
        )
        (var-set last-token-id new-id)
        (match (map-get? tenant-history tenant)
            prev-history (map-set tenant-history tenant (unwrap-panic (as-max-len? (concat prev-history (list new-id)) u10)))
            (map-set tenant-history tenant (list new-id))
        )
        (ok new-id)
    )
)

(define-read-only (get-tenant-record (token-id uint))
    (ok (map-get? tenant-records token-id))
)

(define-read-only (get-tenant-history (tenant principal))
    (ok (map-get? tenant-history tenant))
)

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok none)
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? tenant-rep token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-authorized)
        (nft-transfer? tenant-rep token-id sender recipient)
    )
)



(define-public (burn (token-id uint) (sender principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-authorized)
        (nft-burn? tenant-rep token-id sender)
    )
)
(define-public (get-landlord (token-id uint))
    (ok (get landlord (map-get? tenant-records token-id)))
)
(define-public (get-property-address (token-id uint))
    (ok (get property-address (map-get? tenant-records token-id)))
)
(define-public (get-start-date (token-id uint))
    (ok (get start-date (map-get? tenant-records token-id)))
)
(define-public (get-end-date (token-id uint))
    (ok (get end-date (map-get? tenant-records token-id)))
)
(define-public (get-rating (token-id uint))
    (ok (get rating (map-get? tenant-records token-id)))
)


(define-map disputed-records 
    uint 
    {
        is-disputed: bool,
        dispute-reason: (string-ascii 200),
        dispute-date: uint
    }
)

(define-constant DISPUTE-WINDOW u604800)

(define-public (dispute-rating 
    (token-id uint) 
    (reason (string-ascii 200)))
    (let (
        (record (unwrap! (map-get? tenant-records token-id) err-not-found))
        (current-time stacks-block-height)
    )
        (asserts! (is-eq tx-sender (get tenant record)) err-not-authorized)
        (asserts! (< (- current-time (get end-date record)) DISPUTE-WINDOW) (err u104))
        (ok (map-set disputed-records token-id {
            is-disputed: true,
            dispute-reason: reason,
            dispute-date: current-time
        }))
    )
)

(define-read-only (get-dispute-status (token-id uint))
    (ok (map-get? disputed-records token-id))
)


(define-map tenant-stats
    principal
    {
        total-ratings: uint,
        average-rating: uint,
        total-properties: uint
    }
)

(define-public (calculate-tenant-stats (tenant principal))
    (let (
        (history (unwrap! (map-get? tenant-history tenant) err-not-found))
        (ratings-sum (fold + (map get-rating-for-token history) u0))
        (total-props (len history))
    )
        (asserts! (> total-props u0) err-not-found)
        (ok (map-set tenant-stats tenant {
            total-ratings: ratings-sum,
            average-rating: (/ ratings-sum total-props),
            total-properties: total-props
        }))
    )
)

(define-private (get-rating-for-token (token-id uint))
    (match (map-get? tenant-records token-id)
        record (get rating record)
        u0
    )
)

(define-read-only (get-tenant-stats (tenant principal))
    (ok (map-get? tenant-stats tenant))
)


(define-public (rate-landlord
    (landlord principal)
    (property-address (string-ascii 100))
    (responsiveness-rating uint)
    (maintenance-rating uint)
    (communication-rating uint)
    (review (string-ascii 500)))
    (let
        ((new-rating-id (+ (var-get last-rating-id) u1))
         (overall-rating (/ (+ responsiveness-rating maintenance-rating communication-rating) u3)))
        
        (asserts! (<= responsiveness-rating u5) err-invalid-rating)
        (asserts! (<= maintenance-rating u5) err-invalid-rating)
        (asserts! (<= communication-rating u5) err-invalid-rating)
        (asserts! (> responsiveness-rating u0) err-invalid-rating)
        (asserts! (> maintenance-rating u0) err-invalid-rating)
        (asserts! (> communication-rating u0) err-invalid-rating)
        
        (map-set landlord-ratings new-rating-id
            {
                rater: tx-sender,
                landlord: landlord,
                property-address: property-address,
                responsiveness-rating: responsiveness-rating,
                maintenance-rating: maintenance-rating,
                communication-rating: communication-rating,
                overall-rating: overall-rating,
                review: review,
                rating-date: stacks-block-height
            }
        )
        
        (var-set last-rating-id new-rating-id)
        
        (match (map-get? landlord-rating-history landlord)
            prev-history (map-set landlord-rating-history landlord 
                (unwrap-panic (as-max-len? (concat prev-history (list new-rating-id)) u50)))
            (map-set landlord-rating-history landlord (list new-rating-id))
        )
        
        (try! (update-landlord-reputation landlord))
        (ok new-rating-id)
    )
)

(define-public (update-landlord-reputation (landlord principal))
    (let
        ((rating-history (unwrap! (map-get? landlord-rating-history landlord) err-rating-not-found))
         (total-ratings (len rating-history))
         (responsiveness-sum (fold + (map get-responsiveness-rating rating-history) u0))
         (maintenance-sum (fold + (map get-maintenance-rating rating-history) u0))
         (communication-sum (fold + (map get-communication-rating rating-history) u0))
         (overall-sum (fold + (map get-overall-rating rating-history) u0)))
        
        (asserts! (> total-ratings u0) err-rating-not-found)
        
        (ok (map-set landlord-reputation landlord
            {
                total-ratings: total-ratings,
                average-responsiveness: (/ responsiveness-sum total-ratings),
                average-maintenance: (/ maintenance-sum total-ratings),
                average-communication: (/ communication-sum total-ratings),
                overall-average: (/ overall-sum total-ratings),
                total-properties-managed: total-ratings
            }
        ))
    )
)

(define-private (get-responsiveness-rating (rating-id uint))
    (match (map-get? landlord-ratings rating-id)
        rating (get responsiveness-rating rating)
        u0
    )
)

(define-private (get-maintenance-rating (rating-id uint))
    (match (map-get? landlord-ratings rating-id)
        rating (get maintenance-rating rating)
        u0
    )
)

(define-private (get-communication-rating (rating-id uint))
    (match (map-get? landlord-ratings rating-id)
        rating (get communication-rating rating)
        u0
    )
)

(define-private (get-overall-rating (rating-id uint))
    (match (map-get? landlord-ratings rating-id)
        rating (get overall-rating rating)
        u0
    )
)

(define-read-only (get-landlord-rating (rating-id uint))
    (ok (map-get? landlord-ratings rating-id))
)

(define-read-only (get-landlord-reputation (landlord principal))
    (ok (map-get? landlord-reputation landlord))
)

(define-read-only (get-landlord-rating-history (landlord principal))
    (ok (map-get? landlord-rating-history landlord))
)

(define-read-only (get-landlord-reviews (landlord principal))
    (let
        ((rating-history (default-to (list) (map-get? landlord-rating-history landlord))))
        (ok (map get-rating-details rating-history))
    )
)

(define-private (get-rating-details (rating-id uint))
    (map-get? landlord-ratings rating-id)
)

(define-read-only (is-landlord-highly-rated (landlord principal))
    (match (map-get? landlord-reputation landlord)
        reputation (ok (and (>= (get overall-average reputation) u4) (>= (get total-ratings reputation) u3)))
        (ok false)
    )
)

(define-read-only (get-last-rating-id)
    (ok (var-get last-rating-id))
)


(define-public (create-escrow-agreement
    (tenant principal)
    (property-address (string-ascii 100))
    (monthly-rent uint)
    (deposit-amount uint)
    (lease-duration uint)
    (payment-frequency uint))
    (let
        ((new-escrow-id (+ (var-get last-escrow-id) u1))
         (current-time stacks-block-height)
         (end-date (+ current-time lease-duration)))
        
        (asserts! (is-some (map-get? landlords tx-sender)) err-not-authorized)
        (asserts! (> monthly-rent u0) err-invalid-amount)
        (asserts! (> deposit-amount u0) err-invalid-amount)
        (asserts! (> lease-duration u0) err-invalid-amount)
        (asserts! (> payment-frequency u0) err-invalid-amount)
        
        (map-set escrow-agreements new-escrow-id
            {
                tenant: tenant,
                landlord: tx-sender,
                property-address: property-address,
                monthly-rent: monthly-rent,
                deposit-amount: deposit-amount,
                start-date: current-time,
                end-date: end-date,
                payment-frequency: payment-frequency,
                total-escrowed: u0,
                status: "active",
                next-payment-due: (+ current-time payment-frequency),
                dispute-status: "none",
                dispute-raised-date: u0,
                auto-release-enabled: true
            }
        )
        
        (map-set escrow-balances new-escrow-id
            {
                tenant-balance: u0,
                landlord-balance: u0,
                pending-release: u0,
                penalty-pool: u0
            }
        )
        
        (map-set payment-history new-escrow-id (list))
        
        (var-set last-escrow-id new-escrow-id)
        (ok new-escrow-id)
    )
)

(define-public (deposit-to-escrow (escrow-id uint) (amount uint))
    (let
        ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
         (balance (unwrap! (map-get? escrow-balances escrow-id) err-escrow-not-found))
         (current-time stacks-block-height))
        
        (asserts! (is-eq tx-sender (get tenant escrow)) err-unauthorized-party)
        (asserts! (is-eq (get status escrow) "active") err-escrow-closed)
        (asserts! (> amount u0) err-invalid-amount)
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set escrow-balances escrow-id
            (merge balance {
                tenant-balance: (+ (get tenant-balance balance) amount)
            })
        )
        
        (map-set escrow-agreements escrow-id
            (merge escrow {
                total-escrowed: (+ (get total-escrowed escrow) amount)
            })
        )
        
        (let
            ((current-history (default-to (list) (map-get? payment-history escrow-id)))
             (new-payment {
                payment-date: current-time,
                amount: amount,
                payment-type: "deposit",
                released-date: u0,
                status: "escrowed"
             }))
            
            (map-set payment-history escrow-id
                (unwrap-panic (as-max-len? (concat current-history (list new-payment)) u50))
            )
        )
        
        (ok amount)
    )
)

(define-public (process-monthly-payment (escrow-id uint))
    (let
        ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
         (balance (unwrap! (map-get? escrow-balances escrow-id) err-escrow-not-found))
         (current-time stacks-block-height)
         (monthly-rent (get monthly-rent escrow))
         (payment-due-date (get next-payment-due escrow)))
        
        (asserts! (is-eq (get status escrow) "active") err-escrow-closed)
        (asserts! (>= (get tenant-balance balance) monthly-rent) err-insufficient-funds)
        (asserts! (>= current-time payment-due-date) err-payment-overdue)
        
        (let
            ((platform-fee (/ (* monthly-rent (var-get platform-fee-rate)) u10000))
             (net-payment (- monthly-rent platform-fee)))
            
            (map-set escrow-balances escrow-id
                (merge balance {
                    tenant-balance: (- (get tenant-balance balance) monthly-rent),
                    pending-release: (+ (get pending-release balance) net-payment)
                })
            )
            
            (map-set escrow-agreements escrow-id
                (merge escrow {
                    next-payment-due: (+ payment-due-date (get payment-frequency escrow))
                })
            )
            
            (let
                ((current-history (default-to (list) (map-get? payment-history escrow-id)))
                 (new-payment {
                    payment-date: current-time,
                    amount: monthly-rent,
                    payment-type: "monthly-rent",
                    released-date: u0,
                    status: "pending-release"
                 }))
                
                (map-set payment-history escrow-id
                    (unwrap-panic (as-max-len? (concat current-history (list new-payment)) u50))
                )
            )
            
            (ok net-payment)
        )
    )
)

(define-public (release-payment (escrow-id uint))
    (let
        ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
         (balance (unwrap! (map-get? escrow-balances escrow-id) err-escrow-not-found))
         (current-time stacks-block-height)
         (pending-amount (get pending-release balance)))
        
        (asserts! (or (is-eq tx-sender (get landlord escrow)) (is-eq tx-sender (get tenant escrow))) err-unauthorized-party)
        (asserts! (is-eq (get dispute-status escrow) "none") err-dispute-active)
        (asserts! (> pending-amount u0) err-insufficient-funds)
        
        (try! (as-contract (stx-transfer? pending-amount tx-sender (get landlord escrow))))
        
        (map-set escrow-balances escrow-id
            (merge balance {
                pending-release: u0,
                landlord-balance: (+ (get landlord-balance balance) pending-amount)
            })
        )
        
        (ok pending-amount)
    )
)

(define-public (auto-release-payment (escrow-id uint))
    (let
        ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
         (balance (unwrap! (map-get? escrow-balances escrow-id) err-escrow-not-found))
         (current-time stacks-block-height)
         (pending-amount (get pending-release balance)))
        
        (asserts! (get auto-release-enabled escrow) err-not-authorized)
        (asserts! (is-eq (get dispute-status escrow) "none") err-dispute-active)
        (asserts! (> pending-amount u0) err-insufficient-funds)
        (asserts! (>= current-time (+ (get next-payment-due escrow) ESCROW-TIMEOUT)) err-escrow-expired)
        
        (try! (as-contract (stx-transfer? pending-amount tx-sender (get landlord escrow))))
        
        (map-set escrow-balances escrow-id
            (merge balance {
                pending-release: u0,
                landlord-balance: (+ (get landlord-balance balance) pending-amount)
            })
        )
        
        (ok pending-amount)
    )
)

(define-public (raise-payment-dispute
    (escrow-id uint)
    (disputed-amount uint)
    (reason (string-ascii 300))
    (evidence (string-ascii 500)))
    (let
        ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
         (new-dispute-id (+ (var-get last-dispute-id) u1))
         (current-time stacks-block-height)
         (resolution-deadline (+ current-time DISPUTE-RESOLUTION-PERIOD)))
        
        (asserts! (or (is-eq tx-sender (get tenant escrow)) (is-eq tx-sender (get landlord escrow))) err-unauthorized-party)
        (asserts! (is-eq (get dispute-status escrow) "none") err-dispute-active)
        (asserts! (> disputed-amount u0) err-invalid-amount)
        
        (map-set escrow-disputes escrow-id
            {
                dispute-id: new-dispute-id,
                disputed-amount: disputed-amount,
                reason: reason,
                evidence: evidence,
                resolution-deadline: resolution-deadline,
                arbitrator: none,
                resolution: none
            }
        )
        
        (map-set escrow-agreements escrow-id
            (merge escrow {
                dispute-status: "active",
                dispute-raised-date: current-time,
                auto-release-enabled: false
            })
        )
        
        (var-set last-dispute-id new-dispute-id)
        (ok new-dispute-id)
    )
)

(define-public (resolve-dispute
    (escrow-id uint)
    (tenant-award uint)
    (landlord-award uint)
    (resolution-note (string-ascii 300)))
    (let
        ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
         (dispute (unwrap! (map-get? escrow-disputes escrow-id) err-escrow-not-found))
         (balance (unwrap! (map-get? escrow-balances escrow-id) err-escrow-not-found))
         (current-time stacks-block-height))
        
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-eq (get dispute-status escrow) "active") err-dispute-active)
        (asserts! (<= current-time (get resolution-deadline dispute)) err-dispute-expired)
        
        (if (> tenant-award u0)
            (try! (as-contract (stx-transfer? tenant-award tx-sender (get tenant escrow))))
            true
        )
        
        (if (> landlord-award u0)
            (try! (as-contract (stx-transfer? landlord-award tx-sender (get landlord escrow))))
            true
        )
        
        (map-set escrow-disputes escrow-id
            (merge dispute {
                arbitrator: (some tx-sender),
                resolution: (some resolution-note)
            })
        )
        
        (map-set escrow-agreements escrow-id
            (merge escrow {
                dispute-status: "resolved",
                auto-release-enabled: true
            })
        )
        
        (ok true)
    )
)

(define-public (close-escrow-agreement (escrow-id uint))
    (let
        ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
         (balance (unwrap! (map-get? escrow-balances escrow-id) err-escrow-not-found))
         (current-time stacks-block-height))
        
        (asserts! (or (is-eq tx-sender (get landlord escrow)) (is-eq tx-sender (get tenant escrow))) err-unauthorized-party)
        (asserts! (>= current-time (get end-date escrow)) err-escrow-expired)
        (asserts! (is-eq (get dispute-status escrow) "none") err-dispute-active)
        
        (let
            ((remaining-tenant-balance (get tenant-balance balance))
             (remaining-pending (get pending-release balance)))
            
            (if (> remaining-tenant-balance u0)
                (try! (as-contract (stx-transfer? remaining-tenant-balance tx-sender (get tenant escrow))))
                true
            )
            
            (if (> remaining-pending u0)
                (try! (as-contract (stx-transfer? remaining-pending tx-sender (get landlord escrow))))
                true
            )
        )
        
        (map-set escrow-agreements escrow-id
            (merge escrow {
                status: "closed"
            })
        )
        
        (ok true)
    )
)

(define-public (apply-late-payment-penalty (escrow-id uint))
    (let
        ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
         (balance (unwrap! (map-get? escrow-balances escrow-id) err-escrow-not-found))
         (current-time stacks-block-height)
         (overdue-days (/ (- current-time (get next-payment-due escrow)) u144))
         (penalty-amount (/ (* (get monthly-rent escrow) PENALTY-RATE overdue-days) u100)))
        
        (asserts! (is-eq tx-sender (get landlord escrow)) err-unauthorized-party)
        (asserts! (> overdue-days u0) err-not-authorized)
        (asserts! (>= (get tenant-balance balance) penalty-amount) err-insufficient-funds)
        
        (map-set escrow-balances escrow-id
            (merge balance {
                tenant-balance: (- (get tenant-balance balance) penalty-amount),
                penalty-pool: (+ (get penalty-pool balance) penalty-amount)
            })
        )
        
        (ok penalty-amount)
    )
)

(define-read-only (get-escrow-agreement (escrow-id uint))
    (ok (map-get? escrow-agreements escrow-id))
)

(define-read-only (get-escrow-balance (escrow-id uint))
    (ok (map-get? escrow-balances escrow-id))
)

(define-read-only (get-payment-history (escrow-id uint))
    (ok (map-get? payment-history escrow-id))
)

(define-read-only (get-escrow-dispute (escrow-id uint))
    (ok (map-get? escrow-disputes escrow-id))
)

(define-read-only (get-last-escrow-id)
    (ok (var-get last-escrow-id))
)

(define-read-only (is-payment-overdue (escrow-id uint))
    (match (map-get? escrow-agreements escrow-id)
        escrow (ok (> stacks-block-height (get next-payment-due escrow)))
        (ok false)
    )
)

(define-read-only (calculate-escrow-utilization (escrow-id uint))
    (match (map-get? escrow-balances escrow-id)
        balance (let
            ((total-balance (+ (get tenant-balance balance) (get landlord-balance balance) (get pending-release balance)))
             (escrow-data (unwrap-panic (map-get? escrow-agreements escrow-id)))
             (required-balance (+ (get monthly-rent escrow-data) (get deposit-amount escrow-data))))
            (ok (/ (* total-balance u100) required-balance))
        )
        (ok u0)
    )
)