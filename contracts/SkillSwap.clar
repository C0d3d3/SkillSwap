;; SkillSwap: Professional Skill Exchange Platform
;; Version: 1.0.0
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-SKILL-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-LISTED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-EXPERIENCE (err u5))
(define-constant ERR-INVALID-DOMAIN (err u6))
(define-constant ERR-INVALID-EXPERTISE (err u7))
(define-constant ERR-INVALID-TITLE (err u8))
(define-constant ERR-INVALID-DESCRIPTION (err u9))
(define-constant ERR-EXCHANGE-NOT-FOUND (err u10))
(define-constant ERR-SELF-EXCHANGE (err u11))
(define-constant ERR-SKILL-UNAVAILABLE (err u12))
(define-constant ERR-EXCHANGE-INVALID-STATUS (err u13))
(define-constant MIN-EXPERIENCE u1)

(define-data-var next-skill-id uint u1)
(define-data-var next-exchange-id uint u1)

(define-map professional-skills
    uint
    {
        expert: principal,
        skill-name: (string-utf8 50),
        details: (string-utf8 200),
        domain: (string-utf8 15),
        expertise-level: (string-utf8 20),
        availability: (string-utf8 15),
        experience-years: uint
    }
)

(define-map skill-exchanges
    uint
    {
        learner: principal,
        mentor: principal,
        skill-id: uint,
        session-hours: uint,
        exchange-status: (string-utf8 15)
    }
)

(define-private (validate-domain (domain (string-utf8 15)))
    (or 
        (is-eq domain u"Programming")
        (is-eq domain u"Design")
        (is-eq domain u"Marketing")
        (is-eq domain u"Finance")
        (is-eq domain u"Management")
        (is-eq domain u"Engineering")
    )
)

(define-private (validate-expertise-level (expertise-level (string-utf8 20)))
    (or 
        (is-eq expertise-level u"Junior")
        (is-eq expertise-level u"Mid-Level")
        (is-eq expertise-level u"Senior")
        (is-eq expertise-level u"Lead")
        (is-eq expertise-level u"Principal")
    )
)

(define-private (validate-text-length (text (string-utf8 200)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    )
)

(define-public (list-skill 
    (skill-name (string-utf8 50))
    (details (string-utf8 200))
    (domain (string-utf8 15))
    (expertise-level (string-utf8 20))
    (experience-years uint)
)
    (let
        (
            (skill-id (var-get next-skill-id))
        )
        (asserts! (validate-text-length skill-name u3 u50) ERR-INVALID-TITLE)
        (asserts! (validate-text-length details u10 u200) ERR-INVALID-DESCRIPTION)
        (asserts! (>= experience-years MIN-EXPERIENCE) ERR-INVALID-EXPERIENCE)
        (asserts! (validate-domain domain) ERR-INVALID-DOMAIN)
        (asserts! (validate-expertise-level expertise-level) ERR-INVALID-EXPERTISE)
        
        (map-set professional-skills skill-id {
            expert: tx-sender,
            skill-name: skill-name,
            details: details,
            domain: domain,
            expertise-level: expertise-level,
            availability: u"available",
            experience-years: experience-years
        })
        (var-set next-skill-id (+ skill-id u1))
        (ok skill-id)
    )
)

(define-public (remove-skill (skill-id uint))
    (let
        (
            (skill (unwrap! (map-get? professional-skills skill-id) ERR-SKILL-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get expert skill)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get availability skill) u"available") ERR-INVALID-STATUS)
        (ok (map-set professional-skills skill-id (merge skill { availability: u"removed" })))
    )
)

(define-public (request-mentorship (skill-id uint) (session-hours uint))
    (let
        (
            (skill (unwrap! (map-get? professional-skills skill-id) ERR-SKILL-NOT-FOUND))
            (exchange-id (var-get next-exchange-id))
        )
        (asserts! (is-eq (get availability skill) u"available") ERR-SKILL-UNAVAILABLE)
        (asserts! (not (is-eq tx-sender (get expert skill))) ERR-SELF-EXCHANGE)
        (asserts! (<= session-hours (get experience-years skill)) ERR-INVALID-EXPERIENCE)
        
        (map-set skill-exchanges exchange-id {
            learner: tx-sender,
            mentor: (get expert skill),
            skill-id: skill-id,
            session-hours: session-hours,
            exchange-status: u"pending"
        })
        
        (map-set professional-skills skill-id (merge skill { availability: u"requested" }))
        (var-set next-exchange-id (+ exchange-id u1))
        (ok exchange-id)
    )
)

(define-public (approve-mentorship (exchange-id uint))
    (let
        (
            (exchange (unwrap! (map-get? skill-exchanges exchange-id) ERR-EXCHANGE-NOT-FOUND))
            (skill-id (get skill-id exchange))
            (skill (unwrap! (map-get? professional-skills skill-id) ERR-SKILL-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get mentor exchange)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get exchange-status exchange) u"pending") ERR-EXCHANGE-INVALID-STATUS)
        
        (map-set skill-exchanges exchange-id (merge exchange { exchange-status: u"approved" }))
        (map-set professional-skills skill-id (merge skill { availability: u"mentoring" }))
        
        (ok true)
    )
)

(define-public (reject-mentorship (exchange-id uint))
    (let
        (
            (exchange (unwrap! (map-get? skill-exchanges exchange-id) ERR-EXCHANGE-NOT-FOUND))
            (skill-id (get skill-id exchange))
            (skill (unwrap! (map-get? professional-skills skill-id) ERR-SKILL-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get mentor exchange)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get exchange-status exchange) u"pending") ERR-EXCHANGE-INVALID-STATUS)
        
        (map-set skill-exchanges exchange-id (merge exchange { exchange-status: u"rejected" }))
        (map-set professional-skills skill-id (merge skill { availability: u"available" }))
        
        (ok true)
    )
)

(define-public (finish-mentorship (exchange-id uint))
    (let
        (
            (exchange (unwrap! (map-get? skill-exchanges exchange-id) ERR-EXCHANGE-NOT-FOUND))
            (skill-id (get skill-id exchange))
            (skill (unwrap! (map-get? professional-skills skill-id) ERR-SKILL-NOT-FOUND))
        )
        (asserts! (or (is-eq tx-sender (get learner exchange)) (is-eq tx-sender (get mentor exchange))) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get exchange-status exchange) u"approved") ERR-EXCHANGE-INVALID-STATUS)
        
        (map-set skill-exchanges exchange-id (merge exchange { exchange-status: u"completed" }))
        (map-set professional-skills skill-id (merge skill { availability: u"available" }))
        
        (ok true)
    )
)

(define-read-only (get-skill (skill-id uint))
    (ok (map-get? professional-skills skill-id))
)

(define-read-only (get-expert (skill-id uint))
    (ok (get expert (unwrap! (map-get? professional-skills skill-id) ERR-SKILL-NOT-FOUND)))
)

(define-read-only (get-skill-exchange (exchange-id uint))
    (ok (map-get? skill-exchanges exchange-id))
)
